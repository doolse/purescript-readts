module ReadTS where 
import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson, (.:), (.:?))
import Data.Array (foldMap)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe, maybe)
import Data.String (joinWith)
import Data.Traversable (traverse)
import Data.Tuple (Tuple, snd)
import Effect (Effect)
import Effect.Exception (error, throwException)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Foreign.Object (Object)

type NamedTSType = {name:: String, optional::Boolean, t:: TSType }

type TypeRefParams = {name::String, typeArgs:: Array TSType}

foreign import readTypes :: EffectFn2 String (String -> Boolean) Json
 
data TSType = Interface {name :: String, members::Array NamedTSType}
  | StringLiteral String 
  | NumberLiteral Number
  | BooleanLiteral Boolean
  | TSBoolean
  | TSString
  | TSNumber
  | Void
  | Null
  | Undefined
  | Any
  | Unknown
  | TypeParam String
  | TypeReference TypeRefParams
  | InterfaceReference String
  | Union (Maybe TypeRefParams) (Array TSType) 
  | AnonymousObject (Array NamedTSType)
  | Function {params::Array NamedTSType, return::TSType}

data PSName = PSName String String
data PSSymbolType = SymFunction | SymData | SymConstructor String | SymClass
data PSSymbol = PSSymbol PSSymbolType PSName 
data PSTypeDecl = TConstraint PSName (Array PSTypeDecl) PSTypeDecl
  | TVariable String 
  | TTypeRef PSName (Array PSTypeDecl) 
  | TVariables (Array String) PSTypeDecl
  | TStringConstant String
  | TRow (Array (Tuple String PSTypeDecl)) (Maybe PSTypeDecl)
  | TCommented String PSTypeDecl

type FunctionDecl = {name :: String, ftype :: PSTypeDecl, bodySyms::Array PSSymbol, body :: (PSName -> String) -> String }

data PSDeclaration = DForeignFunction String PSTypeDecl 
  | DTypeAlias String (Array String) PSTypeDecl
  | DFunction FunctionDecl

newtype PSModule = PSModule {name :: String, declarations :: Array PSDeclaration }

derive instance eqTS :: Eq TSType
derive instance eqName :: Eq PSName
derive instance ordName :: Ord PSName
derive instance eqSymType :: Eq PSSymbolType
derive instance ordSymType :: Ord PSSymbolType
derive instance eqSym :: Eq PSSymbol
derive instance ordSym :: Ord PSSymbol
derive instance eqPST :: Eq PSTypeDecl

dataSymbol :: PSName -> PSSymbol
dataSymbol = PSSymbol SymData 

classSymbol :: PSName -> PSSymbol
classSymbol = PSSymbol SymClass 

functionSymbol :: PSName -> PSSymbol 
functionSymbol = PSSymbol SymFunction

localName :: String -> PSName 
localName = PSName ""

localType :: String -> PSTypeDecl 
localType n = localType' n []

localType' :: String -> Array PSTypeDecl -> PSTypeDecl 
localType' n = TTypeRef $ localName n

dataTypeRef :: PSName -> Array PSTypeDecl -> PSTypeDecl
dataTypeRef = TTypeRef

symbolsForType :: PSTypeDecl -> Array PSSymbol 
symbolsForType = case _ of  
  TConstraint n args next -> [classSymbol n] <> (args >>= symbolsForType) <> symbolsForType next
  TVariable _ -> []
  TTypeRef n args -> [dataSymbol n] <> (args >>= symbolsForType) 
  TVariables _ t -> symbolsForType t
  TStringConstant _ -> []
  TRow members ext ->  ((snd <$> members) >>= symbolsForType) <> (maybe [] symbolsForType ext)
  TCommented _ t -> symbolsForType t

symbolsForDeclaration :: PSDeclaration -> Array PSSymbol
symbolsForDeclaration = case _ of 
  DForeignFunction name t -> symbolsForType t
  DTypeAlias _ _ t -> symbolsForType t
  DFunction {ftype, bodySyms} -> symbolsForType ftype <> bodySyms

decodeAlias :: Object Json -> Either String TypeRefParams
decodeAlias o = do 
   name <- o .: "typeReference"
   typeArgs <- o .: "typeParams" >>= traverse decodeTSType
   pure {name, typeArgs}

decodeTSType :: Json -> Either String TSType
decodeTSType v = do
  o <- decodeJson v
  let decodeMembers = o .: "members" >>= traverse decodeMember  
  o .: "type" >>= case _ of 
    "any" -> pure Any
    "boolean" -> pure TSBoolean
    "string" -> pure TSString
    "number" -> pure TSNumber
    "null" -> pure Null
    "false" -> pure $ BooleanLiteral false
    "true" -> pure $ BooleanLiteral true
    "void" -> pure Void 
    "undefined" -> pure Undefined
    "union" -> do 
      alias <- o .:? "alias" >>= traverse decodeAlias
      Union alias <$> (o .: "types" >>= traverse decodeTSType)
    "stringLiteral" -> StringLiteral <$> o .: "value"
    "numberLiteral" -> NumberLiteral <$> o .: "value"
    "unknownObject" -> pure Unknown
    "typeparam" -> TypeParam <$> o .: "name" 
    "interfaceReference" -> InterfaceReference <$> o .: "name"
    "object" -> AnonymousObject <$> decodeMembers
    "function" -> do 
      params <- o .: "params" >>= traverse decodeMember
      return <- o .: "return" >>= decodeTSType
      pure $ Function {params, return}
    "interface" -> do 
      name <- o .: "name"
      members <- decodeMembers
      pure $ Interface {name,members}
    "typeReference" -> do 
      name <- o .: "name"
      typeArgs <- o .: "typeParams" >>= traverse decodeTSType
      pure $ TypeReference {name,typeArgs}
    t -> Left $ "Unknown type" <> show t
  where
  decodeMember o = do 
    name <- o .: "name"
    optional <- o .: "optional"
    t <- o .: "type" >>= decodeTSType
    pure $ {name, t, optional}

showNamed :: NamedTSType -> String 
showNamed {name, t} = name <> ": " <> show t

instance showTSType :: Show TSType where 
  show = case _ of 
    Interface {name, members} -> "Interface"
    StringLiteral l -> "\"" <> l <> "\""
    NumberLiteral n -> show n
    BooleanLiteral b -> show b
    TSBoolean -> "boolean"
    TSString -> "string"
    TSNumber -> "number"
    Void -> "void"
    Null -> "null"
    Undefined -> "undefined"
    Any -> "any"
    Unknown -> "unknown"
    TypeParam tp -> "<" <> tp <> ">"
    TypeReference {name, typeArgs} -> name <> "<" <> (joinWith ", " $ show <$> typeArgs) <> ">"
    InterfaceReference ref -> "interface " <> ref
    Union ref types -> joinWith " | " $ show <$> types
    AnonymousObject members -> "{" <> (joinWith ", " $ showNamed <$> members) <> "}"
    Function {params, return} -> "(" <> (joinWith ", " $ showNamed <$> params) <> " => " <> show return <> ")"

visitTypes :: forall a. Monoid a => (TSType -> a) -> TSType -> a 
visitTypes f = 
  let rec t = f t <> case t of 
        Interface {members} -> foldMap (rec <<< _.t) members
        Union _ types -> foldMap rec types
        AnonymousObject members -> foldMap (rec <<< _.t) members
        Function {params, return} -> rec return <> foldMap (rec <<< _.t) params
        _ -> mempty
  in rec 


readInterfaceTypes :: String -> (String -> Boolean) -> Effect (Array TSType)
readInterfaceTypes tsconfig include = do 
  result <- (decodeJson >=> traverse decodeTSType) <$> runEffectFn2 readTypes tsconfig include
  either (throwException <<< error) pure result
