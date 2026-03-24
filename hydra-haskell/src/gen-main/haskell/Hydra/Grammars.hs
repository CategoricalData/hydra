-- Note: this is an automatically generated file. Do not edit.

-- | A utility for converting a BNF grammar to a Hydra module.

module Hydra.Grammars where

import qualified Hydra.Core as Core
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Grammar as Grammar
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Module as Module
import qualified Hydra.Names as Names
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Generate child name
childName :: String -> String -> String
childName lname n =
    Strings.cat [
      lname,
      "_",
      (Formatting.capitalize n)]

-- | Find unique names for patterns
findNames :: [Grammar.Pattern] -> [String]
findNames pats =

      let nextName =
              \acc -> \pat ->
                let names = Pairs.first acc
                    nameMap = Pairs.second acc
                    rn = rawName pat
                    nameAndIndex =
                            Maybes.maybe (rn, 1) (\i -> (Strings.cat2 rn (Literals.showInt32 (Math.add i 1)), (Math.add i 1))) (Maps.lookup rn nameMap)
                    nn = Pairs.first nameAndIndex
                    ni = Pairs.second nameAndIndex
                in (Lists.cons nn names, (Maps.insert rn ni nameMap))
      in (Lists.reverse (Pairs.first (Lists.foldl nextName ([], Maps.empty) pats)))

-- | Convert a BNF grammar to a Hydra module
grammarToModule :: Module.Namespace -> Grammar.Grammar -> Maybe String -> Module.Module
grammarToModule ns grammar desc =

      let prodPairs =
              Lists.map (\prod -> (Grammar.unSymbol (Grammar.productionSymbol prod), (Grammar.productionPattern prod))) (Grammar.unGrammar grammar)
          capitalizedNames = Lists.map (\pair -> Formatting.capitalize (Pairs.first pair)) prodPairs
          patterns = Lists.map (\pair -> Pairs.second pair) prodPairs
          elementPairs = Lists.concat (Lists.zipWith (makeElements False ns) capitalizedNames patterns)
          wrapType t = case t of
                Core.TypeRecord _ -> t
                Core.TypeUnion _ -> t
                Core.TypeWrap _ -> t
                _ -> Core.TypeWrap t
          typeDefs =
                  Lists.map (\pair ->
                    let lname = Pairs.first pair
                        elName = toName ns lname
                        typ = wrapType (Pairs.second pair)
                    in (Module.DefinitionType (Module.TypeDefinition {
                      Module.typeDefinitionName = elName,
                      Module.typeDefinitionType = typ}))) elementPairs
      in Module.Module {
        Module.moduleNamespace = ns,
        Module.moduleDefinitions = typeDefs,
        Module.moduleTermDependencies = [],
        Module.moduleTypeDependencies = [],
        Module.moduleDescription = desc}

-- | Check if pattern is complex
isComplex :: Grammar.Pattern -> Bool
isComplex pat =
    case pat of
      Grammar.PatternLabeled v0 -> isComplex (Grammar.labeledPatternPattern v0)
      Grammar.PatternSequence v0 -> isNontrivial True v0
      Grammar.PatternAlternatives v0 -> isNontrivial False v0
      _ -> False

-- | Check if patterns are nontrivial
isNontrivial :: Bool -> [Grammar.Pattern] -> Bool
isNontrivial isRecord pats =

      let minPats = simplify isRecord pats
          isLabeled =
                  \p -> case p of
                    Grammar.PatternLabeled _ -> True
                    _ -> False
      in (Logic.ifElse (Equality.equal (Lists.length minPats) 1) (isLabeled (Lists.head minPats)) True)

-- | Create elements from pattern
makeElements :: Bool -> Module.Namespace -> String -> Grammar.Pattern -> [(String, Core.Type)]
makeElements omitTrivial ns lname pat =

      let trivial = Logic.ifElse omitTrivial [] [
            (lname, Core.TypeUnit)]
          descend =
                  \n -> \f -> \p ->
                    let cpairs = makeElements False ns (childName lname n) p
                    in (f (Logic.ifElse (isComplex p) (Lists.cons (lname, (Core.TypeVariable (toName ns (Pairs.first (Lists.head cpairs))))) cpairs) (Logic.ifElse (Lists.null cpairs) [
                      (lname, Core.TypeUnit)] (Lists.cons (lname, (Pairs.second (Lists.head cpairs))) (Lists.tail cpairs)))))
          mod =
                  \n -> \f -> \p -> descend n (\pairs -> Lists.cons (lname, (f (Pairs.second (Lists.head pairs)))) (Lists.tail pairs)) p
          forPat =
                  \pat -> case pat of
                    Grammar.PatternAlternatives v0 -> forRecordOrUnion False (\fields -> Core.TypeUnion fields) v0
                    Grammar.PatternConstant _ -> trivial
                    Grammar.PatternIgnored _ -> []
                    Grammar.PatternLabeled v0 -> forPat (Grammar.labeledPatternPattern v0)
                    Grammar.PatternNil -> trivial
                    Grammar.PatternNonterminal v0 -> [
                      (lname, (Core.TypeVariable (toName ns (Grammar.unSymbol v0))))]
                    Grammar.PatternOption v0 -> mod "Option" (\x -> Core.TypeMaybe x) v0
                    Grammar.PatternPlus v0 -> mod "Elmt" (\x -> Core.TypeList x) v0
                    Grammar.PatternRegex _ -> [
                      (lname, (Core.TypeLiteral Core.LiteralTypeString))]
                    Grammar.PatternSequence v0 -> forRecordOrUnion True (\fields -> Core.TypeRecord fields) v0
                    Grammar.PatternStar v0 -> mod "Elmt" (\x -> Core.TypeList x) v0
          forRecordOrUnion =
                  \isRecord -> \construct -> \pats ->
                    let minPats = simplify isRecord pats
                        fieldNames = findNames minPats
                        toField =
                                \n -> \p -> descend n (\pairs -> (Core.FieldType {
                                  Core.fieldTypeName = (Core.Name n),
                                  Core.fieldTypeType = (Pairs.second (Lists.head pairs))}, (Lists.tail pairs))) p
                        fieldPairs = Lists.zipWith toField fieldNames minPats
                        fields = Lists.map Pairs.first fieldPairs
                        els = Lists.concat (Lists.map Pairs.second fieldPairs)
                    in (Logic.ifElse (isNontrivial isRecord pats) (Lists.cons (lname, (construct fields)) els) (forPat (Lists.head minPats)))
      in (forPat pat)

-- | Get raw name from pattern
rawName :: Grammar.Pattern -> String
rawName pat =
    case pat of
      Grammar.PatternAlternatives _ -> "alts"
      Grammar.PatternConstant v0 -> Formatting.capitalize (Formatting.withCharacterAliases (Grammar.unConstant v0))
      Grammar.PatternIgnored _ -> "ignored"
      Grammar.PatternLabeled v0 -> Grammar.unLabel (Grammar.labeledPatternLabel v0)
      Grammar.PatternNil -> "none"
      Grammar.PatternNonterminal v0 -> Formatting.capitalize (Grammar.unSymbol v0)
      Grammar.PatternOption v0 -> Formatting.capitalize (rawName v0)
      Grammar.PatternPlus v0 -> Strings.cat2 "listOf" (Formatting.capitalize (rawName v0))
      Grammar.PatternRegex _ -> "regex"
      Grammar.PatternSequence _ -> "sequence"
      Grammar.PatternStar v0 -> Strings.cat2 "listOf" (Formatting.capitalize (rawName v0))

-- | Remove trivial patterns from records
simplify :: Bool -> [Grammar.Pattern] -> [Grammar.Pattern]
simplify isRecord pats =

      let isConstant =
              \p -> case p of
                Grammar.PatternConstant _ -> True
                _ -> False
      in (Logic.ifElse isRecord (Lists.filter (\p -> Logic.not (isConstant p)) pats) pats)

-- | Convert local name to qualified name
toName :: Module.Namespace -> String -> Core.Name
toName ns local =
    Names.unqualifyName (Module.QualifiedName {
      Module.qualifiedNameNamespace = (Just ns),
      Module.qualifiedNameLocal = local})
