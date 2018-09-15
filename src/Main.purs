module Main where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson, (.?), (.??))
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (isJust, isNothing)
import Data.String (Pattern(..), indexOf)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Debug.Trace (traceM)
import Effect (Effect)
import Effect.Console (log)
import Foreign.Object (Object, lookup)

foreign import readTypes :: Fn2 String (String -> Boolean) Json

type NamedTsType = Tuple String TsType
data TsType = Interface {name :: String, members::Array NamedTsType}
  | StringLiteral String 
  | NumberLiteral Number
  | BooleanLiteral Boolean
  | TsBoolean
  | TsString
  | TsNumber
  | Void
  | Null
  | Undefined
  | Any
  | Unknown
  | TypeParam String
  | TypeReference {name::String, typeArgs:: Array TsType}
  | InterfaceReference String
  | Union (Array TsType) 
  | AnonymousObject (Array NamedTsType)
  | Function {params::Array NamedTsType, return::TsType}

main :: Effect Unit
main = do
  let propsJson = runFn2 readTypes "/home/jolz/git/material-ui/tsconfig.json" (isJust <<< indexOf (Pattern "Props"))
      decls = (decodeJson propsJson >>= traverse decodeTsType) :: Either String (Array TsType)
  traceM decls
  log "Hello sailor!"



decodeTsType :: Json -> Either String TsType
decodeTsType v = do
  o <- decodeJson v
  let decodeMembers = o .? "members" >>= traverse decodeMember  
  o .? "type" >>= case _ of 
    "any" -> pure Any
    "boolean" -> pure TsBoolean
    "string" -> pure TsString
    "number" -> pure TsNumber
    "null" -> pure Null
    "false" -> pure $ BooleanLiteral false
    "true" -> pure $ BooleanLiteral true
    "void" -> pure Void 
    "undefined" -> pure Undefined
    "union" -> Union <$> (o .? "types" >>= traverse decodeTsType)
    "stringLiteral" -> StringLiteral <$> o .? "value"
    "numberLiteral" -> NumberLiteral <$> o .? "value"
    "unknownObject" -> pure Unknown
    "typeparam" -> TypeParam <$> o .? "name" 
    "interfaceReference" -> InterfaceReference <$> o .? "name"
    "object" -> AnonymousObject <$> decodeMembers
    "function" -> do 
      params <- o .? "params" >>= traverse decodeParam
      return <- o .? "return" >>= decodeTsType
      pure $ Function {params, return}
    "interface" -> do 
      name <- o .? "name"
      members <- decodeMembers
      pure $ Interface {name,members}
    "typeReference" -> do 
      name <- o .? "name"
      typeArgs <- o .? "typeParams" >>= traverse decodeTsType
      pure $ TypeReference {name,typeArgs}
    t -> Left $ "Unknown type" <> show t
  where
  decodeParam o = do 
    name <- o .? "name"
    paramType <- o .? "paramType" >>= decodeTsType
    pure $ Tuple name paramType
  
  decodeMember o = do 
    member <- o .? "member"
    memberType <- o .? "memberType" >>= decodeTsType
    pure $ Tuple member memberType
