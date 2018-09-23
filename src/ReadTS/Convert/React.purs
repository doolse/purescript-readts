module ReadTS.Convert.React where 

import Prelude

import Data.Array (filter, find, foldMap, null, partition)
import Data.Array as Array
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe')
import Data.Set as Set
import Data.String (Pattern(..), split)
import Data.Tuple (Tuple(..), fst)
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (mkdir, writeTextFile)
import Node.FS.Sync as FS
import Node.Path (FilePath, resolve)
import Node.Path as Path
import ReadTS (NamedTSType, PSDeclaration(..), PSModule(..), PSName(..), PSTypeDecl(..), TSType(..), dataTypeRef, functionSymbol, localType, localType')
import ReadTS.CommonPS (arrayType, funcType, recordRefType)
import ReadTS.Convert (collectStrings, escapeFunc, isTSEQSymbol, mkEnumFunction, optionRecordType, optionalType, referenceMapping, simplified, standardMappings)
import ReadTS.WritePS (writeModule)

type Property = { name:: String, optional::Boolean, t:: PSTypeDecl, stringEnums :: Set.Set String }

type ComponentModule = {
  name :: String, 
  module :: PSModule, 
  classFuncName :: String,
  classRequire :: String, 
  classProperty :: String,
  componentType :: ComponentType,
  stringEnums :: Set.Set String
}

data ComponentType = PropsAndChildren PSTypeDecl Boolean | PropsOnly | ChildrenOnly PSTypeDecl

reactCompat :: String -> PSName
reactCompat = PSName "Data.TSCompat.React"

reactNodeType :: PSTypeDecl
reactNodeType = dataTypeRef (reactCompat "ReactNode") []

reactRefMapping :: String -> Array TSType -> Maybe PSTypeDecl
reactRefMapping "React.ReactElement" [Any] = Just $ reactType "ReactElement"
reactRefMapping "React.ReactNode" [] = Just reactNodeType
reactRefMapping "React.SyntheticEvent" [_] = Just $ reactEventType "SyntheticEvent"
reactRefMapping "React.MouseEvent" [_] = Just $ reactEventType "SyntheticMouseEvent"
reactRefMapping "React.AnimationEvent" [_] = Just $ reactEventType "SyntheticAnimationEvent"
reactRefMapping "React.KeyboardEvent" [_] = Just $ reactEventType "SyntheticKeyboardEvent"
reactRefMapping "React.FocusEvent" [_] = Just $ reactEventType "SyntheticFocusEvent"
reactRefMapping "React.UIEvent" [_] = Just $ reactEventType "SyntheticUIEvent"
reactRefMapping "React.ClipboardEvent" [_] = Just $ reactEventType "SyntheticClipboardEvent"
reactRefMapping "React.TouchEvent" [_] = Just $ reactEventType "SyntheticTouchEvent"
reactRefMapping "React.WheelEvent" [_] = Just $ reactEventType "SyntheticWheelEvent"
reactRefMapping "React.TransitionEvent" [_] = Just $ reactEventType "SyntheticTransitionEvent"
reactRefMapping "React.CompositionEvent" [_] = Just $ reactEventType "SyntheticCompositionEvent"
reactRefMapping "React.FormEvent" [_] = Just $ reactEventType "SyntheticEvent"
reactRefMapping "React.DragEvent" [_] = Just $ reactEventType "SyntheticEvent"
reactRefMapping _ _ = Nothing

reactComponentMapper :: (String -> Array TSType -> Maybe PSTypeDecl) -> TSType -> PSTypeDecl
reactComponentMapper refMapper = 
  let mapRefs = referenceMapping refMapper
      mapType t = fromMaybe' (\_ -> standardMappings mapType t) $ mapRefs t
  in mapType

reactName :: String -> PSName
reactName = PSName "React"

isChildrenProp :: Property -> Boolean
isChildrenProp {name:"children"} = true
isChildrenProp _ = false

reactEventType :: String -> PSTypeDecl
reactEventType n = dataTypeRef (PSName "React.SyntheticEvent" n) []

reactType :: String -> PSTypeDecl 
reactType n = reactType' n []

reactType' :: String -> (Array PSTypeDecl) -> PSTypeDecl 
reactType' n a = dataTypeRef (reactName n) a

reactClassType :: PSTypeDecl -> PSTypeDecl 
reactClassType a = reactType' "ReactClass" [a]

reactElementType :: PSTypeDecl
reactElementType = reactType "ReactElement"

createLeafFuncName :: PSName
createLeafFuncName = reactName "unsafeCreateLeafElement"

createElemFuncName :: PSName
createElemFuncName = reactName "unsafeCreateElementDynamic"

convertProperty :: (TSType -> PSTypeDecl) -> NamedTSType -> Property
convertProperty f {name,t,optional} = 
  let optType = fst $ optionalType t 
      stringEnums = Set.fromFoldable $ collectStrings t 
  in {name, optional, t: f $ simplified optType, stringEnums}

reactArray :: PSTypeDecl
reactArray = arrayType reactElementType

detectComponentType :: Array Property -> { componentType :: ComponentType, props :: Array Property }
detectComponentType [] = {componentType: ChildrenOnly reactArray, props:[] }
detectComponentType props | Just children <- find isChildrenProp props  = 
  let childType = case children.t of  
        t | t == reactElementType -> t
        t -> reactArray
      withoutChildren = filter (not isChildrenProp) props 
  in if null withoutChildren then {componentType: ChildrenOnly childType, props:[] } 
     else  {componentType: PropsAndChildren childType children.optional, props: withoutChildren}
detectComponentType props = {componentType: PropsAndChildren reactArray true, props}

propertiesToModule :: {moduleName :: String, classRequire :: String, classProperty :: String } 
  -> String -> Array Property -> ComponentType -> ComponentModule
propertiesToModule {moduleName,classRequire,classProperty} componentName props componentType =
  let 
    baseFuncName = escapeFunc componentName
    classFuncName = escapeFunc $ "class" <> componentName
    classFunc = DForeignFunction classFuncName $ TVariables ["a"] $ reactClassType (TVariable "a")
    childrenOnlyFunc name = DFunction {name, ftype: funcType reactArray reactElementType, 
        bodySyms: [functionSymbol createElemFuncName],
        body: \qual -> name <> " = " <> qual createElemFuncName <> " " <> classFuncName <> " {}"} 
    declarations = case componentType of
      ChildrenOnly childType -> [ classFunc, childrenOnlyFunc baseFuncName ]
      _ -> let 
          {yes:opts, no:mand} = partition _.optional props 
          toRowMember {name,t} = Tuple name t
          optionalName = componentName <> "PropsO"
          mandName = componentName <> "PropsM"
          optionalType = DTypeAlias optionalName ["r"] (TRow (toRowMember <$> opts) $ Just (TVariable "r"))
          mandType = DTypeAlias mandName [] (TRow (toRowMember <$> mand) $ Nothing)
          mandRef = localType mandName
          propRecord = optionRecordType (localType' optionalName $ [mandRef]) mandRef
          constrainedFuncType retType bodySyms body name = DFunction {name, bodySyms, 
            ftype:TVariables ["a"] (TConstraint isTSEQSymbol [recordRefType $ TVariable "a", propRecord ] $ 
              funcType (recordRefType $ TVariable "a") retType), body }
          elemFunc funcCall retType name = constrainedFuncType retType [functionSymbol funcCall] 
            (\qual -> name <> " = " <> qual funcCall <> " " <> classFuncName) name
          leafFunc = elemFunc createLeafFuncName reactElementType 
          bothFunc = elemFunc createElemFuncName (funcType reactArray reactElementType)
          funcs = case componentType of 
            PropsOnly -> [leafFunc baseFuncName]
            _ -> [bothFunc baseFuncName, 
                  childrenOnlyFunc $ baseFuncName <> "_", 
                  leafFunc $ baseFuncName <> "'"]
        in [classFunc, optionalType, mandType] <> funcs
  in { 
    name: componentName,  
    "module": PSModule { name: moduleName, declarations }, 
    classRequire,
    classProperty,
    classFuncName, 
    componentType, 
    stringEnums: foldMap _.stringEnums props
  }

createDirsForModule :: FilePath -> String -> Effect FilePath 
createDirsForModule _basepath moduleName = do 
  basepath <- resolve [_basepath] ""
  let paths = split (Pattern ".") moduleName 
      mkdirs path extpaths = do 
        whenM (not <$> FS.exists path) $ mkdir path
        case extpaths of 
          (Cons last Nil) -> pure $ Path.concat [path, last]
          (Cons fp others) -> mkdirs (Path.concat [path, fp]) others
          Nil -> pure path
  mkdirs basepath $ List.fromFoldable paths 

writeComponent :: FilePath -> ComponentModule -> Effect Unit 
writeComponent basepath cm@{name ,"module": mod@PSModule {name:moduleName}} = do 
  let
    componentFFI = "exports." <> cm.classFuncName <> " = " <> " require('" <> cm.classRequire  <> "')." <> cm.classProperty <> "\n"

  moduleBase <- createDirsForModule basepath moduleName
  writeTextFile UTF8 (moduleBase <> ".purs") $ writeModule mod
  writeTextFile UTF8 (moduleBase <> ".js") $ componentFFI

writeEnumModule :: FilePath -> String -> Array ComponentModule -> Effect Unit 
writeEnumModule basepath moduleName allModules = do
  let 
    allStrings = foldMap _.stringEnums allModules
    declarations = mkEnumFunction <$> Array.fromFoldable allStrings
    enumModule = PSModule {name: moduleName, declarations }
  moduleBase <- createDirsForModule basepath moduleName 
  writeTextFile UTF8 (Path.concat $ [moduleBase <> ".purs"]) $ writeModule enumModule