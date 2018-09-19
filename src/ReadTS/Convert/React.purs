module ReadTS.Convert.React where 

import Prelude

import Data.Array (foldMap, null, partition)
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
import ReadTS (NamedTSType, PSDeclaration(..), PSModule(..), PSName(..), PSTypeDecl(..), TSType(..), dataTypeRef, localType, localType')
import ReadTS.CommonPS (arrayType, funcType, recordRefType)
import ReadTS.Convert (collectStrings, escapeFunc, isTSRecordSymbol, mkEnumFunction, optionalType, referenceMapping, simplified, standardMappings)
import ReadTS.WritePS (writeModule)

type Property = { name:: String, optional::Boolean, t:: PSTypeDecl, stringEnums :: Set.Set String }

type ComponentModule = {
  name :: String, 
  module :: PSModule, 
  elemFuncName :: String,
  classRequire :: String, 
  classProperty :: String,
  componentType :: ComponentType,
  stringEnums :: Set.Set String
}

data ComponentType = PropsAndChildren | PropsOnly | ChildrenOnly

reactCompat :: String -> PSName
reactCompat = PSName "Data.TSCompat.React"

reactRefMapping :: String -> Array TSType -> Maybe PSTypeDecl
reactRefMapping "React.ReactElement" [Any] = Just $ reactType "ReactElement"
reactRefMapping "React.ReactNode" [] = Just $ dataTypeRef (reactCompat "ReactNode") []
reactRefMapping _ _ = Nothing

reactComponentMapper :: (String -> Array TSType -> Maybe PSTypeDecl) -> TSType -> PSTypeDecl
reactComponentMapper refMapper = 
  let mapRefs = referenceMapping refMapper
      mapType t = fromMaybe' (\_ -> standardMappings mapType t) $ mapRefs t
  in mapType

reactName :: String -> PSName
reactName = PSName "React"

isReactNodeChildren :: Property -> Boolean
isReactNodeChildren {name:"children", t} = true
isReactNodeChildren _ = false

reactType :: String -> PSTypeDecl 
reactType n = reactType' n []

reactType' :: String -> (Array PSTypeDecl) -> PSTypeDecl 
reactType' n a = dataTypeRef (reactName n) a

reactElementType :: PSTypeDecl
reactElementType = reactType "ReactElement"

convertProperty :: (TSType -> PSTypeDecl) -> NamedTSType -> Property
convertProperty f {name,t,optional} = 
  let optType = fst $ optionalType t 
      stringEnums = Set.fromFoldable $ collectStrings t 
  in {name, optional, t: f $ simplified optType, stringEnums}

detectComponentType :: Array Property -> { componentType :: ComponentType, props :: Array Property }
detectComponentType [] = {componentType: ChildrenOnly, props:[]}
detectComponentType props = {componentType: PropsAndChildren, props}

propertiesToModule :: {moduleName :: String, classRequire :: String, classProperty :: String } 
  -> String -> Array Property -> ComponentType -> ComponentModule
propertiesToModule {moduleName,classRequire,classProperty} componentName props componentType =
  let 
    elemFuncName = escapeFunc componentName
    declarations = case componentType of
      ChildrenOnly -> [ DForeignFunction elemFuncName $ 
        funcType (arrayType reactElementType) reactElementType ]
      _ -> let 
          {yes:opts, no:mand} = partition _.optional props 
          toRowMember {name,t} = Tuple name t
          optionalName = componentName <> "PropsO"
          mandName = componentName <> "PropsM"
          optionalType = DTypeAlias optionalName ["r"] (TRow (toRowMember <$> opts) $ Just (TVariable "r"))
          mandType = DTypeAlias mandName [] (TRow (toRowMember <$> mand) $ Nothing)
          mandRef = localType mandName
          withChildren = case componentType of 
            PropsAndChildren -> funcType (arrayType reactElementType) reactElementType
            _ -> reactElementType
          underscoreFunc = case componentType of 
            PropsAndChildren | null mand -> let name = elemFuncName <> "_"
              in [DFunction {
                name, 
                ftype: withChildren, bodySyms:[],
                body: \qual -> name <> " = " <> elemFuncName <> " {}"
                }]
            _  -> []
          ftype = TVariables ["a"] (TConstraint isTSRecordSymbol 
            [TVariable "a", (localType' optionalName) [mandRef], mandRef] $ 
              funcType (recordRefType $ TVariable "a") withChildren )
        in [optionalType, mandType, DForeignFunction elemFuncName ftype] <> underscoreFunc
  in { 
    name: componentName,  
    "module": PSModule { name: moduleName, declarations }, 
    classRequire,
    classProperty,
    elemFuncName, 
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
    propPart = "function() { return function(p) {"
    funcJs = case cm.componentType of 
      PropsAndChildren -> propPart <> "return function(c) { return R.createElement(clz, p, c); } } }"
      PropsOnly -> propPart <> "return R.createElement(clz, p); } }"
      ChildrenOnly -> "function (c) { return R.createElement(clz, {}, c); }"
      
    componentFFI = "const clz = require('" <> cm.classRequire  <> "')." <> cm.classProperty <> ";\nconst R = require('react');\n" <>
          "exports." <> cm.elemFuncName <> " = " <> funcJs

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