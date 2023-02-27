-- | A utility for converting a BNF grammar to a Hydra module

module Hydra.Util.GrammarToModule where

import Hydra.Kernel
import Hydra.Dsl.Annotations
import Hydra.Dsl.Types as Types
import Hydra.Dsl.Terms as Terms

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Y


grammarToModule :: Namespace -> Grammar -> Maybe String -> Module Kv
grammarToModule ns (Grammar prods) desc = Module ns elements [] desc
  where
    elements = pairToElement <$> L.concat (L.zipWith (makeElements False) (capitalize . fst <$> prodPairs) (snd <$> prodPairs))
      where
        prodPairs = (\(Production (Symbol s) pat) -> (s, pat)) <$> prods
        pairToElement (lname, typ) = Element (toName lname) (Terms.element _Type) (epsilonEncodeType typ)

    toName = fromQname ns

    findNames pats = L.reverse $ fst (L.foldl nextName ([], M.empty) pats)
      where
        nextName (names, nameMap) pat = (nn:names, M.insert rn ni nameMap)
          where
            rn = rawName pat
            (nn, ni) = case M.lookup rn nameMap of
              Nothing -> (rn, 1)
              Just i -> (rn ++ show (i+1), i+1)

        rawName pat = case pat of
          PatternNil -> "none"
          PatternIgnored _ -> "ignored"
          PatternLabeled (LabeledPattern (Label l) _) -> l
          PatternConstant (Constant c) -> decapitalize $ withCharacterAliases c
          PatternRegex _ -> "regex"
          PatternNonterminal (Symbol s) -> decapitalize s
          PatternSequence _ -> "sequence"
          PatternAlternatives _ -> "alts"
          PatternOption p -> decapitalize (rawName p)
          PatternStar p -> "listOf" ++ capitalize (rawName p)
          PatternPlus p -> "listOf" ++ capitalize (rawName p)

    isComplex pat = case pat of
      PatternLabeled (LabeledPattern _ p) -> isComplex p
      PatternSequence pats -> isNontrivial True pats
      PatternAlternatives pats -> isNontrivial False pats
      _ -> False

    isNontrivial isRecord pats = if L.length minPats == 1
        then case L.head minPats of
          PatternLabeled _ -> True
          _ -> False
        else True
      where
        minPats = simplify isRecord pats

    -- Remove trivial patterns from records
    simplify isRecord pats = if isRecord then L.filter (not . isConstant) pats else pats
      where
        isConstant p = case p of
          PatternConstant _ -> True
          _ -> False

    makeElements omitTrivial lname pat = forPat pat
      where
        forPat pat = case pat of
          PatternNil -> trivial
          PatternIgnored _ -> []
          PatternLabeled (LabeledPattern (Label l) p) -> forPat p
          PatternConstant _ -> trivial
          PatternRegex _ -> [(lname, Types.string)]
          PatternNonterminal (Symbol other) -> [(lname, Types.wrap $ toName other)]
          PatternSequence pats -> forRecordOrUnion True Types.record pats
          PatternAlternatives pats -> forRecordOrUnion False Types.union pats
          PatternOption p -> mod "Option" Types.optional p
          PatternStar p -> mod "Elmt" Types.list p
          PatternPlus p -> mod "Elmt" nonemptyList p

        trivial = if omitTrivial then [] else [(lname, Types.unit)]

        forRecordOrUnion isRecord construct pats = if isNontrivial isRecord pats
            then (lname, construct fields):els
            -- Eliminate single-field record and union types, unless the field has a user-defined name
            else forPat $ L.head minPats
          where
            fieldPairs = L.zipWith toField (findNames minPats) minPats
            fields = fst <$> fieldPairs
            els = L.concat (snd <$> fieldPairs)
            minPats = simplify isRecord pats

        toField n p = descend n f2 p
          where
            f2 ((lname, typ):rest) = (FieldType (FieldName n) typ, rest)

        mod n f p = descend n f2 p
          where
            f2 ((lname, typ):rest) = (lname, f typ):rest

        descend n f p = f $ if isComplex p
            then (lname, Types.wrap (toName $ fst $ L.head cpairs)):cpairs
            else if L.null cpairs
              then [(lname, Types.unit)]
              else (lname, snd (L.head cpairs)):L.tail cpairs
          where
            cpairs = makeElements False (childName lname n) p

    childName lname n = lname ++ "." ++ capitalize n
