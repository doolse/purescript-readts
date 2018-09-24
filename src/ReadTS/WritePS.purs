module ReadTS.WritePS where 

import Prelude

import Data.Array (foldMap, mapMaybe, uncons)
import Data.Either (fromRight)
import Data.Foldable (maximum)
import Data.List (List(..))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set as Set
import Data.String (joinWith)
import Data.String.Regex (Regex, regex, test)
import Data.String.Regex.Flags as Flags
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import ReadTS (PSDeclaration(..), PSModule(..), PSName(..), PSSymbol(..), PSSymbolType(..), PSTypeDecl(..), symbolsForDeclaration)

data Token = Term String 
  | Infix Int String Boolean Token Token 
  | Commented String Token 
  | Tokens (Array Token) 
  | Indented Token

writeSpaces :: Int -> String
writeSpaces 0 = ""
writeSpaces i = " " <> writeSpaces (i - 1)

levelForToken :: Token -> Int
levelForToken = case _ of 
  Infix l _ _ _ _ -> l 
  Commented _ t -> levelForToken t 
  Tokens tokens -> fromMaybe 0 $ maximum $ map levelForToken tokens
  _ -> 0

showTokenIndent :: Int -> Token -> String 
showTokenIndent ind = let 
  bracketed l t | levelForToken t >= l = "(" <> showToken t <> ")"
  bracketed _ t = showToken t
  showToken = case _ of 
    Term t -> t
    Infix level op true l r -> showToken l <> op <> bracketed level r
    Infix level op false l r -> bracketed level l <> op <> showToken r
    Commented com t -> showToken t <> "{-- " <> com <> "--}"
    Tokens tokens -> foldMap showToken tokens
    Indented t -> showTokenIndent (ind + 2) t
  in showToken

instance showTokenInst :: Show Token where 
  show = showTokenIndent 0 

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

    writeMember :: Tuple String PSTypeDecl -> Token 
    writeMember (Tuple member t) = Tokens [Term $ escapedMember member, Term " :: ", writeType t]

    infixed :: Int -> String -> Boolean -> Token -> List Token -> Token
    infixed level op left first = let 
      rec = case _ of 
        Cons head tail -> Infix level op left (rec tail) head 
        Nil -> first
      in rec

    maybeInfix :: Int -> String -> Boolean -> List Token -> Token 
    maybeInfix level op left tokens = case List.reverse tokens of 
      Cons h t -> infixed level op left h t 
      Nil -> Term ""

    writeType :: PSTypeDecl -> Token
    writeType = case _ of 
        TTypeRef (PSName "Prim" "Function") [a, b] -> Infix 2 " -> " false (writeType a) (writeType b)
        TTypeRef typeName args -> infixed 1 " " true (Term $ qual typeName) $ List.reverse $ map writeType $ List.fromFoldable args
        TConstraint typeName args next -> Infix 5 "  => " true (writeType $ TTypeRef typeName args) $ writeType next
        TVariable v -> Term v
        TStringConstant sc -> Term (escapeString sc)
        TVariables vars t -> Tokens [Term $ "forall " <> (joinWith " " vars) <> ".", writeType t]
        TRow members ext -> 
          let extender = maybe ([]) (\et ->  [Term " | ", writeType et]) ext
          in Tokens $ [Term "(", maybeInfix 3 ",\n  " true $ List.fromFoldable $ writeMember <$> members] <> extender <> [Term ")"]
        TCommented com t -> Commented com (writeType t)
    writeDeclaration = case _ of 
      DForeignFunction name typ -> "foreign import " <> name <> " :: " <> (show $ writeType typ)
      DTypeAlias name vars typ -> "type "  <> name <> " " <> (joinWith " " vars) <> " = " <> (show $ writeType typ)
      DFunction {name, ftype, body } -> name <> " :: " <> (show $ writeType ftype) <> "\n" <> body qual
  in "module " <> mod.name <> " where\n" <> 
     (joinWith "\n" (mapMaybe writeImport importStatements)) <> "\n\n" <>
     (joinWith "\n\n" (writeDeclaration <$> mod.declarations))
