module ReadTS.Convert where 

import Prelude

import Control.Alt ((<|>))
import Data.Array (filter, mapMaybe, null)
import Data.Either (fromRight)
import Data.Foldable (any)
import Data.Maybe (Maybe(..), fromMaybe')
import Data.String (Pattern(..), indexOf, toLower, toUpper)
import Data.String as String
import Data.String.Regex (Regex, parseFlags, regex, replace')
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import ReadTS (PSDeclaration(..), PSName(..), PSTypeDecl(..), TSType(..), NamedTSType, dataTypeRef, functionSymbol, visitTypes)
import ReadTS.CommonPS (arrayType, booleanType, effectFnType, funcType, numberType, recordType, stringType, unitType, unsafeCoerceFunc)
import ReadTS.WritePS (escapeString)

isBooleanType :: TSType -> Boolean
isBooleanType = case _ of 
  BooleanLiteral _ -> true 
  TSBoolean -> true 
  _ -> false

isFalseTSType :: TSType -> Boolean
isFalseTSType = eq (BooleanLiteral false)

isTrueTSType :: TSType -> Boolean
isTrueTSType = eq (BooleanLiteral true)

undefinedOrNull :: TSType -> Boolean 
undefinedOrNull = case _ of
  Undefined -> true 
  Null -> true
  _ -> false 

optionalType :: TSType -> Tuple TSType Boolean
optionalType = case _ of 
  Union a types | any undefinedOrNull types -> Tuple (Union a $ filter (not <<< undefinedOrNull) types) true 
  t -> Tuple t false

simpleMapping :: TSType -> Maybe PSTypeDecl
simpleMapping = case _ of 
  TSString -> Just stringType
  TSNumber -> Just numberType
  TSBoolean -> Just booleanType
  StringLiteral a -> Just $ stringConstType a
  Any -> Just anyType
  _ -> Nothing 

simplified :: TSType -> TSType
simplified = case _ of 
  Union _ [o] -> o
  Union a types | any isFalseTSType types && any isTrueTSType types -> 
    simplified (Union a $ [TSBoolean] <>  filter (not <<< isBooleanType) types)
  t -> t

isUnionable :: TSType -> Boolean 
isUnionable = case _ of 
  TSString -> true 
  TSBoolean -> true 
  TSNumber -> true 
  StringLiteral _ -> true 
  _ -> false

isTSEQSymbol :: PSName 
isTSEQSymbol = PSName "Data.TSCompat.Class" "IsTSEq"

tsCompat :: String -> PSName 
tsCompat = PSName "Data.TSCompat"
 
unionSymbol :: PSName
unionSymbol = tsCompat "OneOf"

unionType :: Array PSTypeDecl -> PSTypeDecl
unionType types = dataTypeRef unionSymbol [ TRow (Tuple "typed" <$> types) Nothing ]

stringConstType :: String -> PSTypeDecl
stringConstType s = dataTypeRef (tsCompat "StringConst") [TStringConstant s]

anyType :: PSTypeDecl 
anyType = dataTypeRef (tsCompat "Any") []

optionRecordType :: PSTypeDecl -> PSTypeDecl -> PSTypeDecl
optionRecordType a b = dataTypeRef (tsCompat "OptionRecord") [a, b]

unionMapping :: (TSType -> PSTypeDecl) -> TSType -> Maybe PSTypeDecl
unionMapping f = case _  of 
  (Union _ types) -> do 
    let toMember t = Tuple "typed" $ f t 
        psTypes = toMember <$> types
    Just $ dataTypeRef unionSymbol [ TRow psTypes Nothing ]
  _ -> Nothing
 
standardMappings :: (TSType -> PSTypeDecl) -> TSType -> PSTypeDecl
standardMappings f t =
  let m = unionMapping f t <|>
  simpleMapping t <|> arrayMapping f t <|>
  functionMapping f t <|> objectMapping f t
  in fromMaybe' (\_ -> TCommented (show t) anyType) m

referenceMapping :: (String -> Array TSType -> Maybe PSTypeDecl) -> TSType -> Maybe PSTypeDecl
referenceMapping f = case _ of 
  Union (Just {name,typeArgs}) _ -> f name typeArgs <|> (Just $ TCommented (show name) anyType)
  TypeReference {name,typeArgs} -> f name typeArgs
  _ -> Nothing

arrayMapping :: (TSType -> PSTypeDecl) -> TSType -> Maybe PSTypeDecl
arrayMapping f = case _ of 
  TypeReference {name:"Array", typeArgs:[one]} -> Just $ arrayType (f one)
  _ -> Nothing 

functionMapping :: (TSType -> PSTypeDecl) -> TSType -> Maybe PSTypeDecl
functionMapping f = case _ of 
  Function {params, return: Void} -> effectFnType (_.t >>> f <$> params) unitType
  Function {params:[one], return} -> Just $ funcType (f $ one.t) (f return)
  _ -> Nothing

collectStrings :: TSType -> Array String 
collectStrings = visitTypes $ case _ of 
  StringLiteral s -> [s]
  _ -> []

camelHyphen :: Regex
camelHyphen = unsafePartial fromRight $ regex "-([a-z])" $ parseFlags "ig"

escapeFunc :: String -> String 
escapeFunc l = lowerFirst $ unsafePartial $ replace' camelHyphen (\_ [a] -> toUpper a) l

upperFirst :: String -> String
upperFirst name = toUpper (String.take 1 name) <> String.drop 1 name

lowerFirst :: String -> String
lowerFirst name = toLower (String.take 1 name) <> String.drop 1 name

mkEnumFunction :: String -> PSDeclaration
mkEnumFunction a = let name = escapeFunc a in DFunction {name, ftype: stringConstType a, 
  bodySyms: [functionSymbol unsafeCoerceFunc], body: \qual -> name <> " = " <> qual unsafeCoerceFunc <> " " <> escapeString a}

startsWithAny :: Array String -> String -> Boolean
startsWithAny patterns s = any (\p -> (indexOf (Pattern p) s) == Just 0) patterns

membersWhich :: (TSType -> PSTypeDecl) -> Boolean -> Array NamedTSType -> Array (Tuple String PSTypeDecl)
membersWhich f b = mapMaybe member 
  where 
  member {optional} | optional /= b = Nothing
  member {name,t} = Just $ Tuple name $ f t

objectMapping :: (TSType -> PSTypeDecl) -> TSType -> Maybe PSTypeDecl
objectMapping f = case _ of 
  AnonymousObject members -> 
    let optional = membersWhich f true members
        mand = membersWhich f false members
    in Just $ if null optional then recordType mand Nothing 
           else optionRecordType (TRow (optional <> mand) Nothing) (TRow mand Nothing)
  _ -> Nothing