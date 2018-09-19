module ReadTS.WritePS where 

import Prelude

import Data.Array (mapMaybe, null)
import Data.Either (fromRight)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Set as Set
import Data.String (joinWith)
import Data.String.Regex (Regex, regex, test)
import Data.String.Regex.Flags as Flags
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import ReadTS (PSDeclaration(..), PSModule(..), PSName(..), PSSymbol(..), PSSymbolType(..), PSTypeDecl(..), symbolsForDeclaration)

needImport :: String -> Boolean 
needImport "Prim" = false 
needImport "" = false 
needImport _ = true

legalMemberCharRegex :: Regex
legalMemberCharRegex = unsafePartial $ fromRight $ regex "^[a-z]\\w*$" Flags.noFlags

memberNeedsEscaping :: String -> Boolean 
memberNeedsEscaping = not <<< test legalMemberCharRegex

escapeString :: String -> String 
escapeString s = "\"" <> s <> "\""

escapedMember :: String -> String
escapedMember s = if memberNeedsEscaping s then escapeString s  else s 

writeModule :: PSModule -> String 
writeModule (PSModule mod) = 
  let 
    allSymbols = mod.declarations >>= symbolsForDeclaration
    symByModule s@(PSSymbol _ (PSName m _)) = Tuple m (Set.singleton s)

    importMap :: Map.Map String (Set.Set PSSymbol)
    importMap = Map.fromFoldableWith Set.union (map symByModule allSymbols)
    importStatements = Map.toUnfoldable importMap
    qual (PSName moduleName name) = name
    importDecl (PSSymbol t (PSName _ n)) = case t of 
      SymClass -> "class " <> n
      _ -> n
    writeImport (Tuple moduleName imps) | needImport moduleName = 
      let imported = joinWith ", " $ importDecl <$> Set.toUnfoldable imps
      in Just $ "import " <> moduleName <> " (" <> imported <> ")"
    writeImport _ = Nothing 

    shouldBracket = case _ of 
        TTypeRef _ args | null args -> false
        TVariable _ -> false
        TConstraint _ args _ | null args -> false 
        TVariables _ t -> shouldBracket t
        _ -> true

    writeSpaces 0 = ""
    writeSpaces i = " " <> writeSpaces (i - 1)
    writeMember indent (Tuple m t) = writeSpaces indent <> escapedMember m <> " :: " <> writeTypeIndent indent t
    writeTypeIndent indent = 
      let 
        writeTypeBracketed t = 
          let withoutBracket = writeType t
          in if shouldBracket t then "(" <> withoutBracket <> ")" else withoutBracket
        writeArgs a = joinWith " " $ writeTypeBracketed <$> a
        writeType = case _ of 
            TTypeRef typeName [] -> qual typeName
            TTypeRef typeName args -> qual typeName <> " " <> writeArgs args
            TConstraint className [] next -> qual className <> " => " <> writeType next
            TConstraint className args next -> qual className <> " " <> writeArgs args <> " => " <> writeType next
            TVariable v -> v
            TStringConstant sc -> escapeString sc
            TVariables vars t -> "forall " <> (joinWith " " vars) <> ". " <> writeType t
            TRow members ext -> 
              let extender = maybe "" (\et -> " | " <> writeType et <> " ") ext
              in "(\n" <> (joinWith ",\n" $ writeMember (indent + 2) <$> members) <> extender <> ")"
            TCommented com t -> writeType t <> " {--" <> com <> "--}"
      in writeType
    writeDeclaration = case _ of 
      DForeignFunction name typ -> "foreign import " <> name <> " :: " <> writeTypeIndent 0 typ
      DTypeAlias name vars typ -> "type "  <> name <> " " <> (joinWith " " vars) <> " = " <> writeTypeIndent 0 typ
      DFunction {name, ftype, body } -> name <> " :: " <> writeTypeIndent 0 ftype <> "\n" <> body qual
  in "module " <> mod.name <> " where\n" <> 
     (joinWith "\n" (mapMaybe writeImport importStatements)) <> "\n\n" <>
     (joinWith "\n" (writeDeclaration <$> mod.declarations))
