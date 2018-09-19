module ReadTS.CommonPS where 

import Prelude

import Data.Array (length)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import ReadTS (PSName(..), PSTypeDecl(..), dataTypeRef)

primData :: String -> PSName
primData = PSName "Prim"

primType :: String -> PSTypeDecl
primType n = dataTypeRef (primData n) []

maybeSymbol :: PSName
maybeSymbol = PSName "Data.Maybe" "Maybe"

maybeType :: PSTypeDecl -> PSTypeDecl
maybeType = dataTypeRef maybeSymbol <<< pure

stringType :: PSTypeDecl 
stringType = primType "String"

intType :: PSTypeDecl
intType = primType "Int"

booleanType :: PSTypeDecl
booleanType = primType "Boolean"

numberType :: PSTypeDecl
numberType = primType "Number"

funcSymbol :: PSName
funcSymbol = primData "Function"

recordSymbol :: PSName 
recordSymbol = primData "Record"

arraySymbol :: PSName 
arraySymbol = primData "Array"

unitType :: PSTypeDecl 
unitType = dataTypeRef (PSName "Data.Unit" "Unit") []
 
arrayType :: PSTypeDecl -> PSTypeDecl 
arrayType e = dataTypeRef arraySymbol [e]

funcType :: PSTypeDecl -> PSTypeDecl -> PSTypeDecl
funcType inType outType = dataTypeRef funcSymbol [inType, outType]

recordType :: Array (Tuple String PSTypeDecl) -> Maybe PSTypeDecl -> PSTypeDecl 
recordType members ext = TTypeRef recordSymbol [TRow members ext]

recordRefType :: PSTypeDecl -> PSTypeDecl
recordRefType a = dataTypeRef recordSymbol [a]

unsafeCoerceFunc :: PSName 
unsafeCoerceFunc = PSName "Unsafe.Coerce" "unsafeCoerce"

effectFnType :: Array PSTypeDecl -> PSTypeDecl -> Maybe PSTypeDecl
effectFnType types return | length types > 0 && length types < 11 = 
    Just $ dataTypeRef (PSName "Effect.Uncurried" $ "EffectFn" <> (show $ length types)) $ types <> [return]
effectFnType _ _ = Nothing
