-- Note: this is an automatically generated file. Do not edit.

-- | Utilities for working with Haskell syntax trees

module Hydra.Haskell.Utils where

import qualified Hydra.Analysis as Analysis
import qualified Hydra.Core as Core
import qualified Hydra.Errors as Errors
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Graph as Graph
import qualified Hydra.Haskell.Language as Language
import qualified Hydra.Haskell.Syntax as Syntax
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Names as Names
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Strip as Strip
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Set as S

-- | Create an application pattern from a name and argument patterns
applicationPattern :: Syntax.Name -> [Syntax.Pattern] -> Syntax.Pattern
applicationPattern name args =
    Syntax.PatternApplication (Syntax.ApplicationPattern {
      Syntax.applicationPatternName = name,
      Syntax.applicationPatternArgs = args})

-- | Generate a Haskell name reference for a Hydra element
elementReference :: Packaging.Namespaces Syntax.ModuleName -> Core.Name -> Syntax.Name
elementReference namespaces name =

      let namespacePair = Packaging.namespacesFocus namespaces
          gname = Pairs.first namespacePair
          gmod = Syntax.unModuleName (Pairs.second namespacePair)
          namespacesMap = Packaging.namespacesMapping namespaces
          qname = Names.qualifyName name
          local = Packaging.qualifiedNameLocal qname
          escLocal = sanitizeHaskellName local
          mns = Packaging.qualifiedNameNamespace qname
      in (Maybes.cases (Packaging.qualifiedNameNamespace qname) (simpleName local) (\ns -> Maybes.cases (Maps.lookup ns namespacesMap) (simpleName local) (\mn ->
        let aliasStr = Syntax.unModuleName mn
        in (Logic.ifElse (Equality.equal ns gname) (simpleName escLocal) (rawName (Strings.cat [
          aliasStr,
          ".",
          (sanitizeHaskellName local)]))))))

-- | Create a Haskell function application expression
hsapp :: Syntax.Expression -> Syntax.Expression -> Syntax.Expression
hsapp l r =
    Syntax.ExpressionApplication (Syntax.ApplicationExpression {
      Syntax.applicationExpressionFunction = l,
      Syntax.applicationExpressionArgument = r})

-- | Create a Haskell lambda expression
hslambda :: Syntax.Name -> Syntax.Expression -> Syntax.Expression
hslambda name rhs =
    Syntax.ExpressionLambda (Syntax.LambdaExpression {
      Syntax.lambdaExpressionBindings = [
        Syntax.PatternName name],
      Syntax.lambdaExpressionInner = rhs})

-- | Create a Haskell literal expression
hslit :: Syntax.Literal -> Syntax.Expression
hslit lit = Syntax.ExpressionLiteral lit

-- | Create a Haskell variable expression from a string
hsvar :: String -> Syntax.Expression
hsvar s = Syntax.ExpressionVariable (rawName s)

-- | Compute the Haskell module namespaces for a Hydra module
namespacesForModule :: Packaging.Module -> t0 -> Graph.Graph -> Either Errors.Error (Packaging.Namespaces Syntax.ModuleName)
namespacesForModule mod cx g =
    Eithers.bind (Analysis.moduleDependencyNamespaces cx g True True True True mod) (\nss ->
      let ns = Packaging.moduleNamespace mod
          segmentsOf = \namespace -> Strings.splitOn "." (Packaging.unNamespace namespace)
          aliasFromSuffix =
                  \segs -> \n ->
                    let dropCount = Math.sub (Lists.length segs) n
                        suffix = Lists.drop dropCount segs
                        capitalizedSuffix = Lists.map Formatting.capitalize suffix
                    in (Syntax.ModuleName (Strings.cat capitalizedSuffix))
          toModuleName = \namespace -> aliasFromSuffix (segmentsOf namespace) 1
          focusPair = (ns, (toModuleName ns))
          nssAsList = Sets.toList nss
          segsMap = Maps.fromList (Lists.map (\nm -> (nm, (segmentsOf nm))) nssAsList)
          maxSegs =
                  Lists.foldl (\a -> \b -> Logic.ifElse (Equality.gt a b) a b) 1 (Lists.map (\nm -> Lists.length (segmentsOf nm)) nssAsList)
          initialState = Maps.fromList (Lists.map (\nm -> (nm, 1)) nssAsList)
          segsFor = \nm -> Maybes.fromMaybe [] (Maps.lookup nm segsMap)
          takenFor = \state -> \nm -> Maybes.fromMaybe 1 (Maps.lookup nm state)
          growStep =
                  \state -> \_ign ->
                    let aliasEntries =
                            Lists.map (\nm ->
                              let segs = segsFor nm
                                  n = takenFor state nm
                                  segCount = Lists.length segs
                                  aliasStr = Syntax.unModuleName (aliasFromSuffix segs n)
                              in (nm, (n, (segCount, aliasStr)))) nssAsList
                        aliasCounts =
                                Lists.foldl (\m -> \e ->
                                  let k = Pairs.second (Pairs.second (Pairs.second e))
                                  in (Maps.insert k (Math.add 1 (Maybes.fromMaybe 0 (Maps.lookup k m))) m)) Maps.empty aliasEntries
                        aliasMinSegs =
                                Lists.foldl (\m -> \e ->
                                  let segCount = Pairs.first (Pairs.second (Pairs.second e))
                                      k = Pairs.second (Pairs.second (Pairs.second e))
                                      existing = Maps.lookup k m
                                  in (Maps.insert k (Maybes.cases existing segCount (\prev -> Logic.ifElse (Equality.lt segCount prev) segCount prev)) m)) Maps.empty aliasEntries
                        aliasMinSegsCount =
                                Lists.foldl (\m -> \e ->
                                  let segCount = Pairs.first (Pairs.second (Pairs.second e))
                                      k = Pairs.second (Pairs.second (Pairs.second e))
                                      minSegs = Maybes.fromMaybe segCount (Maps.lookup k aliasMinSegs)
                                  in (Logic.ifElse (Equality.equal segCount minSegs) (Maps.insert k (Math.add 1 (Maybes.fromMaybe 0 (Maps.lookup k m))) m) m)) Maps.empty aliasEntries
                    in (Maps.fromList (Lists.map (\e ->
                      let nm = Pairs.first e
                          n = Pairs.first (Pairs.second e)
                          segCount = Pairs.first (Pairs.second (Pairs.second e))
                          aliasStr = Pairs.second (Pairs.second (Pairs.second e))
                          count = Maybes.fromMaybe 0 (Maps.lookup aliasStr aliasCounts)
                          minSegs = Maybes.fromMaybe segCount (Maps.lookup aliasStr aliasMinSegs)
                          minSegsCount = Maybes.fromMaybe 0 (Maps.lookup aliasStr aliasMinSegsCount)
                          canGrow =
                                  Logic.and (Equality.gt count 1) (Logic.and (Equality.gt segCount n) (Logic.or (Equality.gt segCount minSegs) (Equality.gt minSegsCount 1)))
                          newN = Logic.ifElse canGrow (Math.add n 1) n
                      in (nm, newN)) aliasEntries))
          finalState = Lists.foldl growStep initialState (Lists.replicate maxSegs ())
          resultMap = Maps.fromList (Lists.map (\nm -> (nm, (aliasFromSuffix (segsFor nm) (takenFor finalState nm)))) nssAsList)
      in (Right (Packaging.Namespaces {
        Packaging.namespacesFocus = focusPair,
        Packaging.namespacesMapping = resultMap})))

-- | Generate an accessor name for a newtype wrapper (e.g., 'unFoo' for Foo)
newtypeAccessorName :: Core.Name -> String
newtypeAccessorName name = Strings.cat2 "un" (Names.localNameOf name)

-- | Create a raw Haskell name from a string without sanitization
rawName :: String -> Syntax.Name
rawName n =
    Syntax.NameNormal (Syntax.QualifiedName {
      Syntax.qualifiedNameQualifiers = [],
      Syntax.qualifiedNameUnqualified = (Syntax.NamePart n)})

-- | Generate a Haskell name for a record field accessor
recordFieldReference :: Packaging.Namespaces Syntax.ModuleName -> Core.Name -> Core.Name -> Syntax.Name
recordFieldReference namespaces sname fname =

      let fnameStr = Core.unName fname
          qname = Names.qualifyName sname
          ns = Packaging.qualifiedNameNamespace qname
          typeNameStr = typeNameForRecord sname
          decapitalized = Formatting.decapitalize typeNameStr
          capitalized = Formatting.capitalize fnameStr
          nm = Strings.cat2 decapitalized capitalized
          qualName =
                  Packaging.QualifiedName {
                    Packaging.qualifiedNameNamespace = ns,
                    Packaging.qualifiedNameLocal = nm}
          unqualName = Names.unqualifyName qualName
      in (elementReference namespaces unqualName)

-- | Sanitize a string to be a valid Haskell identifier, escaping reserved words
sanitizeHaskellName :: String -> String
sanitizeHaskellName = Formatting.sanitizeWithUnderscores Language.reservedWords

-- | Create a sanitized Haskell name from a string
simpleName :: String -> Syntax.Name
simpleName arg_ = rawName (sanitizeHaskellName arg_)

-- | Create a simple value binding (e.g., 'foo = expr' or 'foo = expr where ...')
simpleValueBinding :: Syntax.Name -> Syntax.Expression -> Maybe Syntax.LocalBindings -> Syntax.ValueBinding
simpleValueBinding hname rhs bindings =

      let pat =
              Syntax.PatternApplication (Syntax.ApplicationPattern {
                Syntax.applicationPatternName = hname,
                Syntax.applicationPatternArgs = []})
          rightHandSide = Syntax.RightHandSide rhs
      in (Syntax.ValueBindingSimple (Syntax.SimpleValueBinding {
        Syntax.simpleValueBindingPattern = pat,
        Syntax.simpleValueBindingRhs = rightHandSide,
        Syntax.simpleValueBindingLocalBindings = bindings}))

-- | Convert a list of types into a nested type application
toTypeApplication :: [Syntax.Type] -> Syntax.Type
toTypeApplication types =

      let app =
              \l -> Logic.ifElse (Equality.gt (Lists.length l) 1) (Syntax.TypeApplication (Syntax.ApplicationType {
                Syntax.applicationTypeContext = (app (Lists.tail l)),
                Syntax.applicationTypeArgument = (Lists.head l)})) (Lists.head l)
      in (app (Lists.reverse types))

-- | Extract the local type name from a fully qualified record type name
typeNameForRecord :: Core.Name -> String
typeNameForRecord sname =

      let snameStr = Core.unName sname
          parts = Strings.splitOn "." snameStr
      in (Lists.last parts)

-- | Generate a Haskell name for a union variant constructor, with disambiguation
unionFieldReference :: S.Set Core.Name -> Packaging.Namespaces Syntax.ModuleName -> Core.Name -> Core.Name -> Syntax.Name
unionFieldReference boundNames namespaces sname fname =

      let fnameStr = Core.unName fname
          qname = Names.qualifyName sname
          ns = Packaging.qualifiedNameNamespace qname
          typeNameStr = typeNameForRecord sname
          capitalizedTypeName = Formatting.capitalize typeNameStr
          capitalizedFieldName = Formatting.capitalize fnameStr
          deconflict =
                  \name ->
                    let tname =
                            Names.unqualifyName (Packaging.QualifiedName {
                              Packaging.qualifiedNameNamespace = ns,
                              Packaging.qualifiedNameLocal = name})
                    in (Logic.ifElse (Sets.member tname boundNames) (deconflict (Strings.cat2 name "_")) name)
          nm = deconflict (Strings.cat2 capitalizedTypeName capitalizedFieldName)
          qualName =
                  Packaging.QualifiedName {
                    Packaging.qualifiedNameNamespace = ns,
                    Packaging.qualifiedNameLocal = nm}
          unqualName = Names.unqualifyName qualName
      in (elementReference namespaces unqualName)

-- | Unpack nested forall types into a list of type variables and the inner type
unpackForallType :: Core.Type -> ([Core.Name], Core.Type)
unpackForallType t =
    case (Strip.deannotateType t) of
      Core.TypeForall v0 ->
        let v = Core.forallTypeParameter v0
            tbody = Core.forallTypeBody v0
            recursiveResult = unpackForallType tbody
            vars = Pairs.first recursiveResult
            finalType = Pairs.second recursiveResult
        in (Lists.cons v vars, finalType)
      _ -> ([], t)
