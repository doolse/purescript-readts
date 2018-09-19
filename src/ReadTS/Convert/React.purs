module ReadTS.Convert.React where 

import Prelude

import Data.Array (foldMap, partition)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import ReadTS (PSDeclaration(..), PSModule(..), PSName(..), PSTypeDecl(..), TSType(..), NamedTSType, dataTypeRef, localType, localType')
import ReadTS.CommonPS (arrayType, funcType, recordRefType)
import ReadTS.Convert (collectStrings, escapeFunc, isTSRecordSymbol, optionalType, simplified)

type Property = { name:: String, optional::Boolean, t:: PSTypeDecl, stringEnums :: Set.Set String }

data ComponentProps = ComponentProps String (Array Property)

type ComponentModule = {
  name :: String, 
  module :: PSModule, 
  elemFuncName :: String, 
  componentType :: ComponentType,
  stringEnums :: Set.Set String
}

data ComponentType = PropsAndChildren | PropsOnly | ChildrenOnly

reactCompat :: String -> PSName
reactCompat = PSName "Data.TSCompat.React"

namedRefs :: String -> Array TSType -> Maybe PSTypeDecl
namedRefs "React.ReactElement" [Any] = Just $ reactType "ReactElement"
namedRefs "React.ReactNode" [] = Just $ dataTypeRef (reactCompat "ReactNode") []
namedRefs _ _ = Nothing

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
  let (Tuple optType _) = optionalType t 
      stringEnums = Set.fromFoldable $ collectStrings t 
  in {name, optional, t: f $ simplified optType, stringEnums}

stripProps :: String -> String 
stripProps cname = fromMaybe cname $ String.stripSuffix (Pattern "Props") cname

makeDeclarations :: ComponentType -> { opts :: Array Property, mand :: Array Property, 
  basename :: String, elemFuncName :: String } -> Array PSDeclaration
makeDeclarations fft o = case fft of
  ChildrenOnly -> [ DForeignFunction o.elemFuncName $ 
    funcType (arrayType reactElementType) reactElementType ]
  _ ->
    let 
      {basename, elemFuncName, opts, mand} = o
      toRowMember {name,t} = Tuple name t
      optionalName = basename <> "PropsO"
      mandName = basename <> "PropsM"
      optionalType = DTypeAlias optionalName ["r"] (TRow (toRowMember <$> opts) $ Just (TVariable "r"))
      mandType = DTypeAlias mandName [] (TRow (toRowMember <$> mand) $ Nothing)
      mandRef = localType mandName
      withChildren = case fft of 
        PropsAndChildren -> funcType (arrayType reactElementType) reactElementType
        _ -> reactElementType
      ftype = TVariables ["a"] (TConstraint isTSRecordSymbol 
        [TVariable "a", (localType' optionalName) [mandRef], mandRef] $ 
          funcType (recordRefType $ TVariable "a") withChildren )
    in [optionalType, mandType, DForeignFunction elemFuncName ftype]

detectComponentType :: Array Property -> { ctype :: ComponentType, props :: Array Property }
detectComponentType [] = {ctype: ChildrenOnly, props:[]}
detectComponentType props = {ctype: PropsAndChildren, props}

propertiesToModule :: String -> ComponentType -> ComponentProps -> ComponentModule
propertiesToModule modulePrefix componentType (ComponentProps basename props) = 
  let 
    {yes:opts, no:mand} = partition _.optional props 
    elemFuncName = escapeFunc basename
    declarations = makeDeclarations componentType{opts, mand, basename, elemFuncName} 
  in { name: basename,  
    "module": PSModule { name: modulePrefix <> basename, declarations }, 
    elemFuncName, componentType, stringEnums: foldMap _.stringEnums props
  }


-- @material-ui/core/" <> basename <> "
componentFFI :: String -> String -> String -> ComponentType -> String
componentFFI classReq classProp funcName fft = let 
  propPart = "function() { return function(p) {"
  funcJs = case fft of 
    PropsAndChildren -> propPart <> "return function(c) { return R.createElement(clz, p, c); } } }"
    PropsOnly -> propPart <> "return R.createElement(clz, p); } }"
    ChildrenOnly -> "function (c) { return R.createElement(clz, {}, c); }"
    in "const clz = require('" <> classReq  <> "')." <> classProp <> ";\nconst R = require('react');\n" <>
        "exports." <> funcName <> " = " <> funcJs

