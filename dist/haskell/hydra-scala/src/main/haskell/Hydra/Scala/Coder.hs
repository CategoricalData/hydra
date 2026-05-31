-- Note: this is an automatically generated file. Do not edit.
-- | Scala code generator: converts Hydra modules to Scala source code

module Hydra.Scala.Coder where
import qualified Hydra.Analysis as Analysis
import qualified Hydra.Annotations as Annotations
import qualified Hydra.Arity as Arity
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Constants as Constants
import qualified Hydra.Core as Core
import qualified Hydra.Environment as Environment
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Graph as Graph
import qualified Hydra.Inference as Inference
import qualified Hydra.Json.Model as Model
import qualified Hydra.Haskell.Lib.Eithers as Eithers
import qualified Hydra.Haskell.Lib.Equality as Equality
import qualified Hydra.Haskell.Lib.Lists as Lists
import qualified Hydra.Haskell.Lib.Literals as Literals
import qualified Hydra.Haskell.Lib.Logic as Logic
import qualified Hydra.Haskell.Lib.Maps as Maps
import qualified Hydra.Haskell.Lib.Math as Math
import qualified Hydra.Haskell.Lib.Maybes as Maybes
import qualified Hydra.Haskell.Lib.Pairs as Pairs
import qualified Hydra.Haskell.Lib.Sets as Sets
import qualified Hydra.Haskell.Lib.Strings as Strings
import qualified Hydra.Names as Names
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Predicates as Predicates
import qualified Hydra.Query as Query
import qualified Hydra.Reduction as Reduction
import qualified Hydra.Relational as Relational
import qualified Hydra.Resolution as Resolution
import qualified Hydra.Scala.Language as Language
import qualified Hydra.Scala.Serde as Serde
import qualified Hydra.Scala.Syntax as Syntax
import qualified Hydra.Scala.Utils as Utils
import qualified Hydra.Scoping as Scoping
import qualified Hydra.Serialization as Serialization
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Sorting as Sorting
import qualified Hydra.Strip as Strip
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Topology as Topology
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variables as Variables
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
import qualified Data.Set as S
-- | Apply a variable to a term, performing substitution for lambdas
applyVar :: Core.Term -> Core.Name -> Core.Term
applyVar fterm avar =

      let v = Core.unName avar
      in case (Strip.deannotateAndDetypeTerm fterm) of
        Core.TermLambda v0 ->
          let lamParam = Core.lambdaParameter v0
              lamBody = Core.lambdaBody v0
          in (Logic.ifElse (Variables.isFreeVariableInTerm lamParam lamBody) lamBody (Variables.substituteVariable lamParam avar lamBody))
        _ -> Core.TermApplication (Core.Application {
          Core.applicationFunction = fterm,
          Core.applicationArgument = (Core.TermVariable avar)})
-- | Construct a Scala package from a Hydra module and its definitions
constructModule :: t0 -> Graph.Graph -> Packaging.Module -> [Packaging.Definition] -> Either Errors.Error Syntax.Pkg
constructModule cx g mod defs =

      let partitioned = Environment.partitionDefinitions defs
          typeDefs = Pairs.first partitioned
          termDefs = Pairs.second partitioned
          nsName = Packaging.unModuleName (Packaging.moduleName mod)
          pname =
                  Syntax.NameData {
                    Syntax.nameDataValue = (Syntax.PredefString (Strings.intercalate "." (Strings.splitOn "." nsName)))}
          pref = Syntax.RefDataName pname
      in (Eithers.bind (Eithers.mapList (\td -> encodeTypeDefinition cx g td) typeDefs) (\typeDeclStats -> Eithers.bind (Eithers.mapList (\td -> encodeTermDefinition cx g td) termDefs) (\termDeclStats -> Eithers.bind (findImports cx g mod) (\imports -> Right (Syntax.Pkg {
        Syntax.pkgName = pname,
        Syntax.pkgRef = pref,
        Syntax.pkgStats = (Lists.concat [
          imports,
          typeDeclStats,
          termDeclStats])})))))
-- | Drop N domain types from a function type, returning the remaining type
dropDomains :: Int -> Core.Type -> Core.Type
dropDomains n t =
    Logic.ifElse (Equality.lte n 0) t (case (Strip.deannotateType t) of
      Core.TypeFunction v0 -> dropDomains (Math.sub n 1) (Core.functionTypeCodomain v0)
      Core.TypeForall v0 -> dropDomains n (Core.forallTypeBody v0)
      _ -> t)
-- | Encode a case branch
encodeCase :: t0 -> Graph.Graph -> M.Map Core.Name Core.Type -> Maybe Core.Name -> Core.Field -> Either Errors.Error Syntax.Case
encodeCase cx g ftypes sn f =

      let fname = Core.fieldName f
          fterm = Core.fieldTerm f
          isUnit =
                  Maybes.maybe (case (Strip.deannotateAndDetypeTerm fterm) of
                    Core.TermLambda v0 ->
                      let lamParam = Core.lambdaParameter v0
                          lamBody = Core.lambdaBody v0
                          domIsUnit = Maybes.maybe False (\dom -> Equality.equal dom Core.TypeUnit) (Core.lambdaDomain v0)
                          bodyIgnoresParam = Variables.isFreeVariableInTerm lamParam lamBody
                      in (Logic.or domIsUnit bodyIgnoresParam)
                    Core.TermRecord v0 -> Equality.equal (Lists.length (Core.recordFields v0)) 0
                    Core.TermUnit -> True
                    _ -> False) (\dom -> case (Strip.deannotateType dom) of
                    Core.TypeUnit -> True
                    Core.TypeRecord v0 -> Equality.equal (Lists.length v0) 0
                    _ -> False) (Maps.lookup fname ftypes)
          shortTypeName = Maybes.fromMaybe "x" (Lists.maybeLast (Strings.splitOn "." (Maybes.maybe "x" (\n -> Core.unName n) sn)))
          lamParamSuffix =
                  case (Strip.deannotateAndDetypeTerm fterm) of
                    Core.TermLambda v0 ->
                      let rawName = Core.unName (Core.lambdaParameter v0)
                          safeName = Strings.fromList (Lists.map (\c -> Logic.ifElse (Equality.equal c 39) 95 c) (Strings.toList rawName))
                      in (Strings.cat2 "_" safeName)
                    _ -> ""
          v =
                  Core.Name (Strings.cat [
                    "v_",
                    shortTypeName,
                    "_",
                    (Core.unName fname),
                    lamParamSuffix])
          domainIsUnit =
                  case (Strip.deannotateAndDetypeTerm fterm) of
                    Core.TermLambda v0 -> Maybes.maybe True (\dom -> Equality.equal dom Core.TypeUnit) (Core.lambdaDomain v0)
                    _ -> True
          patArgs = Logic.ifElse isUnit (Logic.ifElse domainIsUnit [] [
                Syntax.PatWildcard]) [
                Utils.svar v]
          pat =
                  Syntax.PatExtract (Syntax.ExtractPat {
                    Syntax.extractPatFun = (Utils.sname (Utils.qualifyUnionFieldName "MATCHED." sn fname)),
                    Syntax.extractPatArgs = patArgs})
          applied = applyVar fterm v
      in (Eithers.bind (encodeTerm cx g applied) (\body -> Right (Syntax.Case {
        Syntax.casePat = pat,
        Syntax.caseCond = Nothing,
        Syntax.caseBody = body})))
-- | Encode a complex term definition with proper parameter types from the type signature
encodeComplexTermDef :: t0 -> Graph.Graph -> String -> Core.Term -> Core.Type -> Either Errors.Error Syntax.Stat
encodeComplexTermDef cx g lname term typ =

      let doms = extractDomains typ
          paramNames = extractParams term
          paramCount = Math.min (Lists.length paramNames) (Lists.length doms)
          cod = dropDomains paramCount typ
          zippedParams = Lists.zip (Lists.take paramCount paramNames) (Lists.take paramCount doms)
          freeTypeVars =
                  Lists.filter (\v -> Logic.not (Lists.elem 46 (Strings.toList (Core.unName v)))) (Sets.toList (Variables.freeVariablesInType typ))
          tparams = Lists.map (\tv -> Utils.stparam tv) freeTypeVars
          letBindings = extractLetBindings term
          gWithTypeVars =
                  Graph.Graph {
                    Graph.graphBoundTerms = (Graph.graphBoundTerms g),
                    Graph.graphBoundTypes = (Graph.graphBoundTypes g),
                    Graph.graphClassConstraints = (Graph.graphClassConstraints g),
                    Graph.graphLambdaVariables = (Graph.graphLambdaVariables g),
                    Graph.graphMetadata = (Graph.graphMetadata g),
                    Graph.graphPrimitives = (Graph.graphPrimitives g),
                    Graph.graphSchemaTypes = (Graph.graphSchemaTypes g),
                    Graph.graphTypeVariables = (Sets.union (Sets.fromList freeTypeVars) (Graph.graphTypeVariables g))}
      in (Eithers.bind (Eithers.mapList (encodeTypedParam cx gWithTypeVars) zippedParams) (\sparams -> Eithers.bind (encodeTerm cx gWithTypeVars (extractBody term)) (\sbody -> Eithers.bind (encodeType cx g cod) (\scod ->
        let gForLets =
                Logic.ifElse (Lists.null letBindings) gWithTypeVars (Scoping.extendGraphForLet (\g2 -> \b -> Logic.ifElse (Predicates.isComplexBinding g2 b) (Just (Core.TermLiteral (Core.LiteralBoolean True))) Nothing) gWithTypeVars (Core.Let {
                  Core.letBindings = letBindings,
                  Core.letBody = (Core.TermVariable (Core.Name "dummy"))}))
        in (Eithers.bind (Eithers.mapList (encodeLetBinding cx gForLets (Sets.fromList freeTypeVars)) letBindings) (\sbindings ->
          let defBody =
                  Logic.ifElse (Lists.null sbindings) sbody (Syntax.DataBlock (Syntax.BlockData {
                    Syntax.blockDataStats = (Lists.concat2 sbindings [
                      Syntax.StatTerm sbody])}))
          in (Right (Syntax.StatDefn (Syntax.DefnDef (Syntax.DefDefn {
            Syntax.defDefnMods = [],
            Syntax.defDefnName = Syntax.NameData {
              Syntax.nameDataValue = (Syntax.PredefString lname)},
            Syntax.defDefnTparams = tparams,
            Syntax.defDefnParamss = (Lists.map (\p -> [
              p]) sparams),
            Syntax.defDefnDecltpe = (Just scod),
            Syntax.defDefnBody = defBody}))))))))))
-- | Encode a Hydra function-valued term (lambda, project, cases, or unwrap) as a Scala expression
encodeFunction :: t0 -> Graph.Graph -> M.Map Core.Name Core.Term -> Core.Term -> Maybe Core.Term -> Either Errors.Error Syntax.Data
encodeFunction cx g meta funTerm arg =
    case (Strip.deannotateAndDetypeTerm funTerm) of
      Core.TermLambda v0 ->
        let param = Core.lambdaParameter v0
            v = Utils.scalaEscapeName (Core.unName param)
            body = Core.lambdaBody v0
            rawMdom = Core.lambdaDomain v0
            mdom =
                    Maybes.bind rawMdom (\dom ->
                      let freeVars = Variables.freeVariablesInType dom
                          unqualifiedFreeVars =
                                  Sets.fromList (Lists.filter (\n -> Logic.not (Lists.elem 46 (Strings.toList (Core.unName n)))) (Sets.toList freeVars))
                          unresolvedVars = Sets.difference unqualifiedFreeVars (Graph.graphTypeVariables g)
                      in (Logic.ifElse (Sets.null unresolvedVars) (Just dom) Nothing))
        in (Eithers.bind (encodeTerm cx g body) (\sbody -> Eithers.bind (Maybes.maybe (findSdom cx g meta) (\dom -> Eithers.bind (encodeType cx g dom) (\sdom -> Right (Just sdom))) mdom) (\sdom -> Right (Utils.slambda v sbody sdom))))
      Core.TermUnwrap _ -> Maybes.maybe (Eithers.bind (findSdom cx g meta) (\sdom -> Right (Utils.slambda "x" (Utils.sname "x") sdom))) (\a -> encodeTerm cx g a) arg
      Core.TermProject v0 ->
        let fname = Utils.scalaEscapeName (Core.unName (Core.projectionFieldName v0))
            typeName = Core.projectionTypeName v0
            pv = "x"
        in (Maybes.maybe (Eithers.bind (Eithers.either (\_ -> Eithers.bind (encodeType cx g (Core.TypeVariable typeName)) (\st -> Right (Just st))) (\msdom -> Maybes.maybe (Eithers.bind (encodeType cx g (Core.TypeVariable typeName)) (\st -> Right (Just st))) (\sdom -> Right (Just sdom)) msdom) (findSdom cx g meta)) (\msdom -> Right (Utils.slambda pv (Syntax.DataRef (Syntax.RefDataSelect (Syntax.SelectData {
          Syntax.selectDataQual = (Utils.sname pv),
          Syntax.selectDataName = Syntax.NameData {
            Syntax.nameDataValue = (Syntax.PredefString fname)}}))) msdom))) (\a -> Eithers.bind (encodeTerm cx g a) (\sa -> Right (Syntax.DataRef (Syntax.RefDataSelect (Syntax.SelectData {
          Syntax.selectDataQual = sa,
          Syntax.selectDataName = Syntax.NameData {
            Syntax.nameDataValue = (Syntax.PredefString fname)}}))))) arg)
      Core.TermCases v0 ->
        let v = "v"
            tname = Core.caseStatementTypeName v0
            dom = Core.TypeVariable tname
            sn = Utils.nameOfType g dom
            cases = Core.caseStatementCases v0
            dflt = Core.caseStatementDefault v0
            ftypes = Eithers.either (\_ -> Maps.empty) (\x_ -> x_) (Resolution.fieldTypes cx g dom)
        in (Eithers.bind (Eithers.mapList (\f -> encodeCase cx g ftypes sn f) cases) (\fieldCases -> Eithers.bind (Maybes.maybe (Right fieldCases) (\dfltTerm -> Eithers.bind (encodeTerm cx g dfltTerm) (\sdflt -> Right (Lists.concat2 fieldCases [
          Syntax.Case {
            Syntax.casePat = Syntax.PatWildcard,
            Syntax.caseCond = Nothing,
            Syntax.caseBody = sdflt}]))) dflt) (\scases -> Maybes.maybe (Eithers.bind (findSdom cx g meta) (\sdom -> Right (Utils.slambda v (Syntax.DataMatch (Syntax.MatchData {
          Syntax.matchDataExpr = (Utils.sname v),
          Syntax.matchDataCases = scases})) sdom))) (\a -> Eithers.bind (encodeTerm cx g a) (\sa -> Right (Syntax.DataMatch (Syntax.MatchData {
          Syntax.matchDataExpr = sa,
          Syntax.matchDataCases = scases})))) arg)))
      _ -> Left (Errors.ErrorOther (Errors.OtherError "unsupported function"))
-- | Encode a let binding as a val or def declaration. outerTypeVars are type params from the enclosing scope.
encodeLetBinding :: t0 -> Graph.Graph -> S.Set Core.Name -> Core.Binding -> Either Errors.Error Syntax.Stat
encodeLetBinding cx g outerTypeVars b =

      let bname = Utils.scalaEscapeName (Core.unName (Core.bindingName b))
          bterm = Core.bindingTerm b
          mts = Maybes.maybe (Maps.lookup (Core.bindingName b) (Graph.graphBoundTypes g)) (\ts -> Just ts) (Core.bindingTypeScheme b)
          isFn =
                  Maybes.maybe False (\ts -> case (Strip.deannotateType (Core.typeSchemeBody ts)) of
                    Core.TypeFunction _ -> True
                    Core.TypeForall v0 -> case (Strip.deannotateType (Core.forallTypeBody v0)) of
                      Core.TypeFunction _ -> True
                      _ -> False
                    _ -> False) mts
      in (Maybes.maybe (Eithers.bind (encodeTerm cx g bterm) (\srhs -> Right (Syntax.StatDefn (Syntax.DefnVal (Syntax.ValDefn {
        Syntax.valDefnMods = [
          Syntax.ModLazy],
        Syntax.valDefnPats = [
          Syntax.PatVar (Syntax.VarPat {
            Syntax.varPatName = Syntax.NameData {
              Syntax.nameDataValue = (Syntax.PredefString bname)}})],
        Syntax.valDefnDecltpe = Nothing,
        Syntax.valDefnRhs = srhs}))))) (\ts ->
        let newVars = Lists.filter (\v -> Logic.not (Sets.member v outerTypeVars)) (Core.typeSchemeVariables ts)
            useDef = Logic.or isFn (Logic.not (Lists.null newVars))
        in (Logic.ifElse useDef (encodeLocalDef cx g outerTypeVars bname bterm (Core.typeSchemeBody ts)) (Eithers.bind (encodeTerm cx g bterm) (\srhs -> Eithers.bind (encodeType cx g (Core.typeSchemeBody ts)) (\styp -> Right (Syntax.StatDefn (Syntax.DefnVal (Syntax.ValDefn {
          Syntax.valDefnMods = [
            Syntax.ModLazy],
          Syntax.valDefnPats = [
            Syntax.PatVar (Syntax.VarPat {
              Syntax.varPatName = Syntax.NameData {
                Syntax.nameDataValue = (Syntax.PredefString bname)}})],
          Syntax.valDefnDecltpe = (Just styp),
          Syntax.valDefnRhs = srhs})))))))) mts)
-- | Encode a literal value as a Scala literal
encodeLiteral :: t0 -> t1 -> Core.Literal -> Either Errors.Error Syntax.Lit
encodeLiteral cx g av =
    case av of
      Core.LiteralBinary v0 -> Right (Syntax.LitBytes (Literals.binaryToBytes v0))
      Core.LiteralBoolean v0 -> Right (Syntax.LitBoolean v0)
      Core.LiteralDecimal v0 -> Right (Syntax.LitString (Literals.showDecimal v0))
      Core.LiteralFloat v0 -> case v0 of
        Core.FloatValueFloat32 v1 -> Right (Syntax.LitFloat v1)
        Core.FloatValueFloat64 v1 -> Right (Syntax.LitDouble v1)
        _ -> Left (Errors.ErrorOther (Errors.OtherError "unexpected float value"))
      Core.LiteralInteger v0 -> case v0 of
        Core.IntegerValueBigint v1 -> Right (Syntax.LitLong (Literals.bigintToInt64 v1))
        Core.IntegerValueInt8 v1 -> Right (Syntax.LitByte v1)
        Core.IntegerValueInt16 v1 -> Right (Syntax.LitShort v1)
        Core.IntegerValueInt32 v1 -> Right (Syntax.LitInt v1)
        Core.IntegerValueInt64 v1 -> Right (Syntax.LitLong v1)
        Core.IntegerValueUint8 v1 -> Right (Syntax.LitByte (Literals.bigintToInt8 (Literals.uint8ToBigint v1)))
        Core.IntegerValueUint16 v1 -> Right (Syntax.LitInt (Literals.bigintToInt32 (Literals.uint16ToBigint v1)))
        Core.IntegerValueUint32 v1 -> Right (Syntax.LitLong (Literals.bigintToInt64 (Literals.uint32ToBigint v1)))
        Core.IntegerValueUint64 v1 -> Right (Syntax.LitLong (Literals.bigintToInt64 (Literals.uint64ToBigint v1)))
        _ -> Left (Errors.ErrorOther (Errors.OtherError "unexpected integer value"))
      Core.LiteralString v0 -> Right (Syntax.LitString v0)
      _ -> Left (Errors.ErrorOther (Errors.OtherError "unexpected literal"))
-- | Encode a local def. outerTypeVars are type params already in scope (don't redeclare them).
encodeLocalDef :: t0 -> Graph.Graph -> S.Set Core.Name -> String -> Core.Term -> Core.Type -> Either Errors.Error Syntax.Stat
encodeLocalDef cx g outerTypeVars lname term typ =

      let freeTypeVars =
              Lists.filter (\v -> Logic.and (Logic.not (Lists.elem 46 (Strings.toList (Core.unName v)))) (Logic.not (Sets.member v outerTypeVars))) (Sets.toList (Variables.freeVariablesInType typ))
          doms = extractDomains typ
          paramNames = extractParams term
          paramCount = Math.min (Lists.length paramNames) (Lists.length doms)
          cod = dropDomains paramCount typ
          zippedParams = Lists.zip (Lists.take paramCount paramNames) (Lists.take paramCount doms)
          letBindings = extractLetBindings term
          tparams = Lists.map (\tv -> Utils.stparam tv) freeTypeVars
          allTypeVars = Sets.union outerTypeVars (Sets.fromList freeTypeVars)
          gWithTypeVars =
                  Graph.Graph {
                    Graph.graphBoundTerms = (Graph.graphBoundTerms g),
                    Graph.graphBoundTypes = (Graph.graphBoundTypes g),
                    Graph.graphClassConstraints = (Graph.graphClassConstraints g),
                    Graph.graphLambdaVariables = (Graph.graphLambdaVariables g),
                    Graph.graphMetadata = (Graph.graphMetadata g),
                    Graph.graphPrimitives = (Graph.graphPrimitives g),
                    Graph.graphSchemaTypes = (Graph.graphSchemaTypes g),
                    Graph.graphTypeVariables = (Sets.union allTypeVars (Graph.graphTypeVariables g))}
      in (Eithers.bind (Eithers.mapList (encodeTypedParam cx gWithTypeVars) zippedParams) (\sparams -> Eithers.bind (encodeTerm cx gWithTypeVars (extractBody term)) (\sbody -> Eithers.bind (encodeType cx gWithTypeVars cod) (\scod ->
        let gForLets =
                Logic.ifElse (Lists.null letBindings) gWithTypeVars (Scoping.extendGraphForLet (\g2 -> \b -> Logic.ifElse (Predicates.isComplexBinding g2 b) (Just (Core.TermLiteral (Core.LiteralBoolean True))) Nothing) gWithTypeVars (Core.Let {
                  Core.letBindings = letBindings,
                  Core.letBody = (Core.TermVariable (Core.Name "dummy"))}))
        in (Eithers.bind (Eithers.mapList (encodeLetBinding cx gForLets allTypeVars) letBindings) (\sbindings ->
          let defBody =
                  Logic.ifElse (Lists.null sbindings) sbody (Syntax.DataBlock (Syntax.BlockData {
                    Syntax.blockDataStats = (Lists.concat2 sbindings [
                      Syntax.StatTerm sbody])}))
          in (Right (Syntax.StatDefn (Syntax.DefnDef (Syntax.DefDefn {
            Syntax.defDefnMods = [],
            Syntax.defDefnName = Syntax.NameData {
              Syntax.nameDataValue = (Syntax.PredefString lname)},
            Syntax.defDefnTparams = tparams,
            Syntax.defDefnParamss = (Lists.map (\p -> [
              p]) sparams),
            Syntax.defDefnDecltpe = (Just scod),
            Syntax.defDefnBody = defBody}))))))))))
-- | Encode a Hydra term as a Scala expression
encodeTerm :: t0 -> Graph.Graph -> Core.Term -> Either Errors.Error Syntax.Data
encodeTerm cx g term0 =

      let term = stripWrapEliminations term0
      in case (Strip.deannotateTerm term) of
        Core.TermTypeApplication v0 ->
          let collectTypeArgs =
                  \t -> \acc -> case (Strip.deannotateTerm t) of
                    Core.TermTypeApplication v1 -> collectTypeArgs (Core.typeApplicationTermBody v1) (Lists.cons (Core.typeApplicationTermType v1) acc)
                    _ -> (acc, t)
              collected = collectTypeArgs (Core.typeApplicationTermBody v0) [
                    Core.typeApplicationTermType v0]
              typeArgs = Pairs.first collected
              innerTerm = Pairs.second collected
              collectTypeLambdas =
                      \t -> \acc -> case (Strip.deannotateTerm t) of
                        Core.TermTypeLambda v1 -> collectTypeLambdas (Core.typeLambdaBody v1) (Lists.cons (Core.typeLambdaParameter v1) acc)
                        _ -> (acc, t)
              tlCollected = collectTypeLambdas innerTerm []
              typeParams = Pairs.first tlCollected
              bodyAfterTypeLambdas = Pairs.second tlCollected
              substitutedBody = bodyAfterTypeLambdas
          in case (Strip.deannotateTerm substitutedBody) of
            Core.TermProject _ -> encodeTerm cx g substitutedBody
            Core.TermCases _ -> encodeTerm cx g substitutedBody
            Core.TermUnwrap _ -> encodeTerm cx g substitutedBody
            Core.TermVariable v1 -> Maybes.cases (Maps.lookup v1 (Graph.graphPrimitives g)) (encodeTerm cx g substitutedBody) (\_prim -> Eithers.bind (Eithers.mapList (\targ -> encodeType cx g targ) typeArgs) (\stypeArgs ->
              let inScopeTypeVarNames =
                      Sets.fromList (Lists.map (\n -> Formatting.capitalize (Core.unName n)) (Sets.toList (Graph.graphTypeVariables g)))
                  hasForallResidual =
                          Logic.not (Lists.null (Lists.filter (\st -> case st of
                            Syntax.TypeVar v2 ->
                              let tvName = Syntax.nameTypeValue (Syntax.varTypeName v2)
                              in (Logic.and (Logic.not (Lists.elem 46 (Strings.toList tvName))) (Logic.not (Sets.member tvName inScopeTypeVarNames)))
                            _ -> False) stypeArgs))
              in (Logic.ifElse hasForallResidual (Right (Utils.sprim v1)) (Right (Utils.sapplyTypes (Utils.sprim v1) stypeArgs)))))
            _ -> encodeTerm cx g substitutedBody
        Core.TermTypeLambda v0 -> encodeTerm cx (Scoping.extendGraphForTypeLambda g v0) (Core.typeLambdaBody v0)
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in case (Strip.deannotateAndDetypeTerm fun) of
            Core.TermLambda v1 ->
              let lamBody = Core.lambdaBody v1
              in case (Strip.deannotateAndDetypeTerm lamBody) of
                Core.TermApplication v2 ->
                  let innerFun = Core.applicationFunction v2
                  in case (Strip.deannotateAndDetypeTerm innerFun) of
                    Core.TermCases _ -> encodeFunction cx g (Annotations.termAnnotationInternal innerFun) innerFun (Just arg)
                    _ -> Eithers.bind (encodeTerm cx g fun) (\sfun -> Eithers.bind (encodeTerm cx g arg) (\sarg -> Right (Utils.sapply sfun [
                      sarg])))
                _ -> Eithers.bind (encodeTerm cx g fun) (\sfun -> Eithers.bind (encodeTerm cx g arg) (\sarg -> Right (Utils.sapply sfun [
                  sarg])))
            Core.TermProject v1 ->
              let fname = Utils.scalaEscapeName (Core.unName (Core.projectionFieldName v1))
              in (Eithers.bind (encodeTerm cx g arg) (\sarg -> Right (Syntax.DataRef (Syntax.RefDataSelect (Syntax.SelectData {
                Syntax.selectDataQual = sarg,
                Syntax.selectDataName = Syntax.NameData {
                  Syntax.nameDataValue = (Syntax.PredefString fname)}})))))
            Core.TermCases _ -> encodeFunction cx g (Annotations.termAnnotationInternal fun) fun (Just arg)
            _ -> Eithers.bind (encodeTerm cx g fun) (\sfun -> Eithers.bind (encodeTerm cx g arg) (\sarg -> Right (Utils.sapply sfun [
              sarg])))
        Core.TermLambda _ -> encodeFunction cx g (Annotations.termAnnotationInternal term) term Nothing
        Core.TermProject _ -> encodeFunction cx g (Annotations.termAnnotationInternal term) term Nothing
        Core.TermCases _ -> encodeFunction cx g (Annotations.termAnnotationInternal term) term Nothing
        Core.TermUnwrap _ -> encodeFunction cx g (Annotations.termAnnotationInternal term) term Nothing
        Core.TermList v0 -> Eithers.bind (Eithers.mapList (\e -> encodeTerm cx g e) v0) (\sels -> Right (Utils.sapply (Utils.sname "Seq") sels))
        Core.TermLiteral v0 -> Eithers.bind (encodeLiteral cx g v0) (\slit ->
          let litData = Syntax.DataLit slit
          in case v0 of
            Core.LiteralDecimal _ -> Right (Utils.sapply (Utils.sname "BigDecimal") [
              litData])
            Core.LiteralInteger v1 -> case v1 of
              Core.IntegerValueBigint v2 -> Right (Utils.sapply (Utils.sname "BigInt") [
                Syntax.DataLit (Syntax.LitString (Literals.showBigint v2))])
              Core.IntegerValueUint64 v2 -> Right (Utils.sapply (Utils.sname "BigInt") [
                Syntax.DataLit (Syntax.LitString (Literals.showBigint (Literals.uint64ToBigint v2)))])
              _ -> Right litData
            _ -> Right litData)
        Core.TermMap v0 -> Eithers.bind (Eithers.mapList (\kv -> Eithers.bind (encodeTerm cx g (Pairs.first kv)) (\sk -> Eithers.bind (encodeTerm cx g (Pairs.second kv)) (\sv -> Right (Utils.sassign sk sv)))) (Maps.toList v0)) (\spairs -> Right (Utils.sapply (Utils.sname "Map") spairs))
        Core.TermWrap v0 -> encodeTerm cx g (Core.wrappedTermBody v0)
        Core.TermMaybe v0 -> Maybes.maybe (Right (Utils.sname "None")) (\t -> Eithers.bind (encodeTerm cx g t) (\s -> Right (Utils.sapply (Utils.sname "Some") [
          s]))) v0
        Core.TermRecord v0 ->
          let rname = Core.recordTypeName v0
              fields = Core.recordFields v0
              n = Utils.scalaTypeName True rname
          in (Eithers.bind (Eithers.mapList (\f -> encodeTerm cx g (Core.fieldTerm f)) fields) (\args -> Right (Utils.sapply (Utils.sname n) args)))
        Core.TermSet v0 -> Eithers.bind (Eithers.mapList (\e -> encodeTerm cx g e) (Sets.toList v0)) (\sels -> Right (Utils.sapply (Utils.sname "scala.collection.immutable.Set") sels))
        Core.TermInject v0 ->
          let sn = Core.injectionTypeName v0
              fn = Core.fieldName (Core.injectionField v0)
              ft = Core.fieldTerm (Core.injectionField v0)
              lhs = Utils.sname (Utils.qualifyUnionFieldName "UNION." (Just sn) fn)
              unionFtypes = Eithers.either (\_ -> Maps.empty) (\x_ -> x_) (Resolution.fieldTypes cx g (Core.TypeVariable sn))
          in (Logic.ifElse (Maybes.maybe (case (Strip.deannotateAndDetypeTerm ft) of
            Core.TermUnit -> True
            Core.TermRecord v1 -> Equality.equal (Lists.length (Core.recordFields v1)) 0
            _ -> False) (\dom -> case (Strip.deannotateType dom) of
            Core.TypeUnit -> True
            Core.TypeRecord v1 -> Equality.equal (Lists.length v1) 0
            _ -> False) (Maps.lookup fn unionFtypes)) (Right lhs) (Eithers.bind (encodeTerm cx g ft) (\sarg -> Right (Utils.sapply lhs [
            sarg]))))
        Core.TermVariable v0 ->
          let fullName = Core.unName v0
              localName = Names.localNameOf v0
              parts = Strings.splitOn "." fullName
              numParts = Lists.length parts
              escaped =
                      Logic.ifElse (Equality.lte numParts 1) (Utils.scalaEscapeName fullName) (Logic.ifElse (Equality.equal numParts 2) (Strings.cat2 (Maybes.fromMaybe fullName (Lists.maybeHead parts)) (Strings.cat2 "." (Utils.scalaEscapeName localName))) (Strings.intercalate "." (Lists.concat2 (Lists.take (Math.sub numParts 1) parts) [
                        Utils.scalaEscapeName localName])))
          in (Right (Utils.sname escaped))
        Core.TermAnnotated v0 -> encodeTerm cx g (Core.annotatedTermBody v0)
        Core.TermEither v0 -> Eithers.either (\l -> Eithers.bind (encodeTerm cx g l) (\sl -> Right (Utils.sapply (Utils.sname "Left") [
          sl]))) (\r -> Eithers.bind (encodeTerm cx g r) (\sr -> Right (Utils.sapply (Utils.sname "Right") [
          sr]))) v0
        Core.TermPair v0 -> Eithers.bind (encodeTerm cx g (Pairs.first v0)) (\sf -> Eithers.bind (encodeTerm cx g (Pairs.second v0)) (\ss -> Right (Utils.sapply (Utils.sname "Tuple2") [
          sf,
          ss])))
        Core.TermUnit -> Right (Syntax.DataLit Syntax.LitUnit)
        Core.TermLet v0 ->
          let bindings = Core.letBindings v0
              body = Core.letBody v0
              gLet =
                      Scoping.extendGraphForLet (\g2 -> \b -> Logic.ifElse (Predicates.isComplexBinding g2 b) (Just (Core.TermLiteral (Core.LiteralBoolean True))) Nothing) g v0
          in (Eithers.bind (Eithers.mapList (encodeLetBinding cx gLet (Graph.graphTypeVariables gLet)) bindings) (\sbindings -> Eithers.bind (encodeTerm cx gLet body) (\sbody -> Right (Syntax.DataBlock (Syntax.BlockData {
            Syntax.blockDataStats = (Lists.concat2 sbindings [
              Syntax.StatTerm sbody])})))))
        _ -> Left (Errors.ErrorOther (Errors.OtherError "unexpected term"))
-- | Encode a term definition as a Scala statement
encodeTermDefinition :: t0 -> Graph.Graph -> Packaging.TermDefinition -> Either Errors.Error Syntax.Stat
encodeTermDefinition cx g td =

      let name = Packaging.termDefinitionName td
          term = Packaging.termDefinitionTerm td
          lname = Utils.scalaEscapeName (Names.localNameOf name)
          typ_ =
                  Maybes.maybe (Core.TypeVariable (Core.Name "hydra.core.Unit")) Core.typeSchemeBody (Maybes.map Scoping.termSignatureToTypeScheme (Packaging.termDefinitionSignature td))
          isFunctionType =
                  case (Strip.deannotateType typ_) of
                    Core.TypeFunction _ -> True
                    Core.TypeForall v0 -> case (Strip.deannotateType (Core.forallTypeBody v0)) of
                      Core.TypeFunction _ -> True
                      _ -> False
                    _ -> False
      in (Logic.ifElse isFunctionType (encodeComplexTermDef cx g lname term typ_) (Eithers.bind (encodeType cx g typ_) (\stype -> Eithers.bind (encodeTerm cx g term) (\rhs -> Right (Syntax.StatDefn (Syntax.DefnVal (Syntax.ValDefn {
        Syntax.valDefnMods = [
          Syntax.ModLazy],
        Syntax.valDefnPats = [
          Syntax.PatVar (Syntax.VarPat {
            Syntax.varPatName = Syntax.NameData {
              Syntax.nameDataValue = (Syntax.PredefString lname)}})],
        Syntax.valDefnDecltpe = (Just stype),
        Syntax.valDefnRhs = rhs})))))))
-- | Encode a Hydra type as a Scala type
encodeType :: t0 -> t1 -> Core.Type -> Either Errors.Error Syntax.Type
encodeType cx g t =
    case (Strip.deannotateType t) of
      Core.TypeApplication v0 ->
        let collectTypeArgs =
                \t2 -> \acc -> case (Strip.deannotateType t2) of
                  Core.TypeApplication v1 ->
                    let f2 = Core.applicationTypeFunction v1
                        a2 = Core.applicationTypeArgument v1
                    in (collectTypeArgs f2 (Lists.cons a2 acc))
                  _ -> (t2, acc)
            collected = collectTypeArgs (Core.TypeApplication v0) []
            baseFun = Pairs.first collected
            allArgs = Pairs.second collected
        in (Eithers.bind (encodeType cx g baseFun) (\sfun -> Eithers.bind (Eithers.mapList (\a -> encodeType cx g a) allArgs) (\sargs -> Right (Utils.stapply sfun sargs))))
      Core.TypeUnit -> Right (Syntax.TypeRef (Syntax.RefTypeName (Syntax.NameType {
        Syntax.nameTypeValue = "Unit"})))
      Core.TypeEither v0 ->
        let lt = Core.eitherTypeLeft v0
            rt = Core.eitherTypeRight v0
        in (Eithers.bind (encodeType cx g lt) (\slt -> Eithers.bind (encodeType cx g rt) (\srt -> Right (Utils.stapply2 (Syntax.TypeRef (Syntax.RefTypeName (Syntax.NameType {
          Syntax.nameTypeValue = "Either"}))) slt srt))))
      Core.TypeFunction v0 ->
        let dom = Core.functionTypeDomain v0
            cod = Core.functionTypeCodomain v0
        in (Eithers.bind (encodeType cx g dom) (\sdom -> Eithers.bind (encodeType cx g cod) (\scod -> Right (Syntax.TypeFunction (Syntax.FunctionType {
          Syntax.functionTypeParams = [
            sdom],
          Syntax.functionTypeRes = scod})))))
      Core.TypeList v0 -> Eithers.bind (encodeType cx g v0) (\slt -> Right (Utils.stapply1 (Syntax.TypeRef (Syntax.RefTypeName (Syntax.NameType {
        Syntax.nameTypeValue = "Seq"}))) slt))
      Core.TypeLiteral v0 -> case v0 of
        Core.LiteralTypeBinary -> Right (Utils.stapply (Syntax.TypeRef (Syntax.RefTypeName (Syntax.NameType {
          Syntax.nameTypeValue = "Array"}))) [
          Syntax.TypeRef (Syntax.RefTypeName (Syntax.NameType {
            Syntax.nameTypeValue = "Byte"}))])
        Core.LiteralTypeBoolean -> Right (Syntax.TypeRef (Syntax.RefTypeName (Syntax.NameType {
          Syntax.nameTypeValue = "Boolean"})))
        Core.LiteralTypeDecimal -> Right (Syntax.TypeRef (Syntax.RefTypeName (Syntax.NameType {
          Syntax.nameTypeValue = "BigDecimal"})))
        Core.LiteralTypeFloat v1 -> case v1 of
          Core.FloatTypeFloat32 -> Right (Syntax.TypeRef (Syntax.RefTypeName (Syntax.NameType {
            Syntax.nameTypeValue = "Float"})))
          Core.FloatTypeFloat64 -> Right (Syntax.TypeRef (Syntax.RefTypeName (Syntax.NameType {
            Syntax.nameTypeValue = "Double"})))
          _ -> Left (Errors.ErrorOther (Errors.OtherError "unsupported float type"))
        Core.LiteralTypeInteger v1 -> case v1 of
          Core.IntegerTypeBigint -> Right (Syntax.TypeRef (Syntax.RefTypeName (Syntax.NameType {
            Syntax.nameTypeValue = "BigInt"})))
          Core.IntegerTypeInt8 -> Right (Syntax.TypeRef (Syntax.RefTypeName (Syntax.NameType {
            Syntax.nameTypeValue = "Byte"})))
          Core.IntegerTypeInt16 -> Right (Syntax.TypeRef (Syntax.RefTypeName (Syntax.NameType {
            Syntax.nameTypeValue = "Short"})))
          Core.IntegerTypeInt32 -> Right (Syntax.TypeRef (Syntax.RefTypeName (Syntax.NameType {
            Syntax.nameTypeValue = "Int"})))
          Core.IntegerTypeInt64 -> Right (Syntax.TypeRef (Syntax.RefTypeName (Syntax.NameType {
            Syntax.nameTypeValue = "Long"})))
          Core.IntegerTypeUint8 -> Right (Syntax.TypeRef (Syntax.RefTypeName (Syntax.NameType {
            Syntax.nameTypeValue = "Byte"})))
          Core.IntegerTypeUint16 -> Right (Syntax.TypeRef (Syntax.RefTypeName (Syntax.NameType {
            Syntax.nameTypeValue = "Int"})))
          Core.IntegerTypeUint32 -> Right (Syntax.TypeRef (Syntax.RefTypeName (Syntax.NameType {
            Syntax.nameTypeValue = "Long"})))
          Core.IntegerTypeUint64 -> Right (Syntax.TypeRef (Syntax.RefTypeName (Syntax.NameType {
            Syntax.nameTypeValue = "BigInt"})))
          _ -> Left (Errors.ErrorOther (Errors.OtherError "unsupported integer type"))
        Core.LiteralTypeString -> Right (Syntax.TypeRef (Syntax.RefTypeName (Syntax.NameType {
          Syntax.nameTypeValue = "scala.Predef.String"})))
        _ -> Left (Errors.ErrorOther (Errors.OtherError "unsupported literal type"))
      Core.TypeMap v0 ->
        let kt = Core.mapTypeKeys v0
            vt = Core.mapTypeValues v0
        in (Eithers.bind (encodeType cx g kt) (\skt -> Eithers.bind (encodeType cx g vt) (\svt -> Right (Utils.stapply2 (Syntax.TypeRef (Syntax.RefTypeName (Syntax.NameType {
          Syntax.nameTypeValue = "Map"}))) skt svt))))
      Core.TypeMaybe v0 -> Eithers.bind (encodeType cx g v0) (\sot -> Right (Utils.stapply1 (Syntax.TypeRef (Syntax.RefTypeName (Syntax.NameType {
        Syntax.nameTypeValue = "Option"}))) sot))
      Core.TypePair v0 ->
        let ft = Core.pairTypeFirst v0
            st = Core.pairTypeSecond v0
        in (Eithers.bind (encodeType cx g ft) (\sft -> Eithers.bind (encodeType cx g st) (\sst -> Right (Utils.stapply2 (Syntax.TypeRef (Syntax.RefTypeName (Syntax.NameType {
          Syntax.nameTypeValue = "Tuple2"}))) sft sst))))
      Core.TypeRecord _ -> Left (Errors.ErrorOther (Errors.OtherError "unexpected anonymous record type"))
      Core.TypeSet v0 -> Eithers.bind (encodeType cx g v0) (\sst -> Right (Utils.stapply1 (Syntax.TypeRef (Syntax.RefTypeName (Syntax.NameType {
        Syntax.nameTypeValue = "scala.collection.immutable.Set"}))) sst))
      Core.TypeUnion _ -> Left (Errors.ErrorOther (Errors.OtherError "unexpected anonymous union type"))
      Core.TypeWrap _ -> Left (Errors.ErrorOther (Errors.OtherError "unexpected anonymous wrap type"))
      Core.TypeForall v0 ->
        let v = Core.forallTypeParameter v0
            body = Core.forallTypeBody v0
        in (Eithers.bind (encodeType cx g body) (\sbody -> Right (Syntax.TypeLambda (Syntax.LambdaType {
          Syntax.lambdaTypeTparams = [
            Utils.stparam v],
          Syntax.lambdaTypeTpe = sbody}))))
      Core.TypeVariable v0 ->
        let rawName = Core.unName v0
            typeName = Logic.ifElse (Lists.elem 46 (Strings.toList rawName)) rawName (Formatting.capitalize rawName)
        in (Right (Syntax.TypeVar (Syntax.VarType {
          Syntax.varTypeName = Syntax.NameType {
            Syntax.nameTypeValue = typeName}})))
      _ -> Left (Errors.ErrorOther (Errors.OtherError "unsupported type"))
-- | Encode a type definition as a Scala statement
encodeTypeDefinition :: t0 -> t1 -> Packaging.TypeDefinition -> Either Errors.Error Syntax.Stat
encodeTypeDefinition cx g td =

      let name = Packaging.typeDefinitionName td
          typ = Core.typeSchemeBody (Packaging.typeDefinitionTypeScheme td)
          lname = Names.localNameOf name
          tname = Syntax.NameType {
                Syntax.nameTypeValue = lname}
          dname = Syntax.NameData {
                Syntax.nameDataValue = (Syntax.PredefString lname)}
          freeVars =
                  Lists.filter (\v -> Logic.not (Lists.elem 46 (Strings.toList (Core.unName v)))) (Sets.toList (Variables.freeVariablesInType typ))
          tparams =
                  Lists.map (\_v ->
                    let vn = Formatting.capitalize (Core.unName _v)
                    in Syntax.ParamType {
                      Syntax.paramTypeMods = [],
                      Syntax.paramTypeName = (Syntax.NameValue vn),
                      Syntax.paramTypeTparams = [],
                      Syntax.paramTypeTbounds = [],
                      Syntax.paramTypeVbounds = [],
                      Syntax.paramTypeCbounds = []}) freeVars
      in case (Strip.deannotateType typ) of
        Core.TypeForall v0 ->
          let forallBody = Core.forallTypeBody v0
              forallParam = Core.forallTypeParameter v0
              collectForallParams =
                      \t -> \acc -> case (Strip.deannotateType t) of
                        Core.TypeForall v1 -> collectForallParams (Core.forallTypeBody v1) (Lists.cons (Core.forallTypeParameter v1) acc)
                        _ -> (acc, t)
              collected = collectForallParams forallBody [
                    forallParam]
              allForallParams = Lists.reverse (Pairs.first collected)
              innerBody = Pairs.second collected
              allTparams =
                      Lists.map (\_v ->
                        let vn = Formatting.capitalize (Core.unName _v)
                        in Syntax.ParamType {
                          Syntax.paramTypeMods = [],
                          Syntax.paramTypeName = (Syntax.NameValue vn),
                          Syntax.paramTypeTparams = [],
                          Syntax.paramTypeTbounds = [],
                          Syntax.paramTypeVbounds = [],
                          Syntax.paramTypeCbounds = []}) allForallParams
          in case (Strip.deannotateType innerBody) of
            Core.TypeRecord v1 -> Eithers.bind (Eithers.mapList (\f -> fieldToParam cx g f) v1) (\params -> Right (Syntax.StatDefn (Syntax.DefnClass (Syntax.ClassDefn {
              Syntax.classDefnMods = [
                Syntax.ModCase],
              Syntax.classDefnName = tname,
              Syntax.classDefnTparams = allTparams,
              Syntax.classDefnCtor = Syntax.PrimaryCtor {
                Syntax.primaryCtorMods = [],
                Syntax.primaryCtorName = (Syntax.NameValue ""),
                Syntax.primaryCtorParamss = [
                  params]},
              Syntax.classDefnTemplate = Syntax.Template {
                Syntax.templateEarly = [],
                Syntax.templateInits = [],
                Syntax.templateSelf = (Syntax.Self ()),
                Syntax.templateStats = []}}))))
            Core.TypeUnion v1 -> Eithers.bind (Eithers.mapList (\f -> fieldToEnumCase cx g lname allTparams f) v1) (\cases -> Right (Syntax.StatDefn (Syntax.DefnEnum (Syntax.EnumDefn {
              Syntax.enumDefnMods = [],
              Syntax.enumDefnName = tname,
              Syntax.enumDefnTparams = allTparams,
              Syntax.enumDefnCtor = Syntax.PrimaryCtor {
                Syntax.primaryCtorMods = [],
                Syntax.primaryCtorName = (Syntax.NameValue ""),
                Syntax.primaryCtorParamss = []},
              Syntax.enumDefnTemplate = Syntax.Template {
                Syntax.templateEarly = [],
                Syntax.templateInits = [],
                Syntax.templateSelf = (Syntax.Self ()),
                Syntax.templateStats = cases}}))))
            Core.TypeWrap v1 -> Eithers.bind (encodeType cx g v1) (\styp -> Right (Syntax.StatDefn (Syntax.DefnType (Syntax.TypeDefn {
              Syntax.typeDefnMods = [],
              Syntax.typeDefnName = tname,
              Syntax.typeDefnTparams = allTparams,
              Syntax.typeDefnBody = styp}))))
            _ ->
              let mkAlias =
                      \styp -> Right (Syntax.StatDefn (Syntax.DefnType (Syntax.TypeDefn {
                        Syntax.typeDefnMods = [],
                        Syntax.typeDefnName = Syntax.NameType {
                          Syntax.nameTypeValue = lname},
                        Syntax.typeDefnTparams = allTparams,
                        Syntax.typeDefnBody = styp})))
              in (Eithers.either (\_ -> mkAlias (Utils.stref "Any")) mkAlias (encodeType cx g innerBody))
        Core.TypeRecord v0 -> Eithers.bind (Eithers.mapList (\f -> fieldToParam cx g f) v0) (\params -> Right (Syntax.StatDefn (Syntax.DefnClass (Syntax.ClassDefn {
          Syntax.classDefnMods = [
            Syntax.ModCase],
          Syntax.classDefnName = tname,
          Syntax.classDefnTparams = tparams,
          Syntax.classDefnCtor = Syntax.PrimaryCtor {
            Syntax.primaryCtorMods = [],
            Syntax.primaryCtorName = (Syntax.NameValue ""),
            Syntax.primaryCtorParamss = [
              params]},
          Syntax.classDefnTemplate = Syntax.Template {
            Syntax.templateEarly = [],
            Syntax.templateInits = [],
            Syntax.templateSelf = (Syntax.Self ()),
            Syntax.templateStats = []}}))))
        Core.TypeUnion v0 -> Eithers.bind (Eithers.mapList (\f -> fieldToEnumCase cx g lname tparams f) v0) (\cases -> Right (Syntax.StatDefn (Syntax.DefnEnum (Syntax.EnumDefn {
          Syntax.enumDefnMods = [],
          Syntax.enumDefnName = tname,
          Syntax.enumDefnTparams = tparams,
          Syntax.enumDefnCtor = Syntax.PrimaryCtor {
            Syntax.primaryCtorMods = [],
            Syntax.primaryCtorName = (Syntax.NameValue ""),
            Syntax.primaryCtorParamss = []},
          Syntax.enumDefnTemplate = Syntax.Template {
            Syntax.templateEarly = [],
            Syntax.templateInits = [],
            Syntax.templateSelf = (Syntax.Self ()),
            Syntax.templateStats = cases}}))))
        Core.TypeWrap v0 -> Eithers.bind (encodeType cx g v0) (\styp -> Right (Syntax.StatDefn (Syntax.DefnType (Syntax.TypeDefn {
          Syntax.typeDefnMods = [],
          Syntax.typeDefnName = tname,
          Syntax.typeDefnTparams = tparams,
          Syntax.typeDefnBody = styp}))))
        _ ->
          let mkAlias =
                  \styp -> Right (Syntax.StatDefn (Syntax.DefnType (Syntax.TypeDefn {
                    Syntax.typeDefnMods = [],
                    Syntax.typeDefnName = Syntax.NameType {
                      Syntax.nameTypeValue = lname},
                    Syntax.typeDefnTparams = tparams,
                    Syntax.typeDefnBody = styp})))
          in (Eithers.either (\_ -> mkAlias (Utils.stref "Any")) mkAlias (encodeType cx g typ))
-- | Encode a parameter with its type annotation
encodeTypedParam :: t0 -> t1 -> (Core.Name, Core.Type) -> Either Errors.Error Syntax.ParamData
encodeTypedParam cx g pair =

      let pname = Utils.scalaEscapeName (Names.localNameOf (Pairs.first pair))
          pdom = Pairs.second pair
      in (Eithers.bind (encodeType cx g pdom) (\sdom -> Right (Syntax.ParamData {
        Syntax.paramDataMods = [],
        Syntax.paramDataName = (Syntax.NameValue pname),
        Syntax.paramDataDecltpe = (Just sdom),
        Syntax.paramDataDefault = Nothing})))
-- | Encode an untyped application term by first inferring types
encodeUntypeApplicationTerm :: Typing.InferenceContext -> Graph.Graph -> Core.Term -> Either Errors.Error Syntax.Data
encodeUntypeApplicationTerm cx g term =
    Eithers.bind (Inference.inferInGraphContext cx g term) (\result -> encodeTerm cx g (Typing.inferenceResultTerm result))
-- | Extract the innermost body from a term
extractBody :: Core.Term -> Core.Term
extractBody t =
    case (Strip.deannotateAndDetypeTerm t) of
      Core.TermLambda v0 -> extractBody (Core.lambdaBody v0)
      Core.TermTypeLambda v0 -> extractBody (Core.typeLambdaBody v0)
      Core.TermTypeApplication v0 -> extractBody (Core.typeApplicationTermBody v0)
      Core.TermLet v0 -> extractBody (Core.letBody v0)
      _ -> t
-- | Extract the final return type from a function type
extractCodomain :: Core.Type -> Core.Type
extractCodomain t =
    case (Strip.deannotateType t) of
      Core.TypeFunction v0 -> extractCodomain (Core.functionTypeCodomain v0)
      Core.TypeForall v0 -> extractCodomain (Core.forallTypeBody v0)
      _ -> t
-- | Extract domain types from a function type
extractDomains :: Core.Type -> [Core.Type]
extractDomains t =
    case (Strip.deannotateType t) of
      Core.TypeFunction v0 -> Lists.cons (Core.functionTypeDomain v0) (extractDomains (Core.functionTypeCodomain v0))
      Core.TypeForall v0 -> extractDomains (Core.forallTypeBody v0)
      _ -> []
-- | Extract let bindings from a term
extractLetBindings :: Core.Term -> [Core.Binding]
extractLetBindings t =
    case (Strip.deannotateAndDetypeTerm t) of
      Core.TermLambda v0 -> extractLetBindings (Core.lambdaBody v0)
      Core.TermTypeLambda v0 -> extractLetBindings (Core.typeLambdaBody v0)
      Core.TermTypeApplication v0 -> extractLetBindings (Core.typeApplicationTermBody v0)
      Core.TermLet v0 -> Lists.concat2 (Core.letBindings v0) (extractLetBindings (Core.letBody v0))
      _ -> []
-- | Extract parameter names from a term
extractParams :: Core.Term -> [Core.Name]
extractParams t =
    case (Strip.deannotateAndDetypeTerm t) of
      Core.TermLambda v0 -> Lists.cons (Core.lambdaParameter v0) (extractParams (Core.lambdaBody v0))
      Core.TermTypeLambda v0 -> extractParams (Core.typeLambdaBody v0)
      Core.TermTypeApplication v0 -> extractParams (Core.typeApplicationTermBody v0)
      Core.TermLet v0 -> extractParams (Core.letBody v0)
      _ -> []
-- | Convert a field type to a Scala enum case
fieldToEnumCase :: t0 -> t1 -> String -> [Syntax.ParamType] -> Core.FieldType -> Either Errors.Error Syntax.Stat
fieldToEnumCase cx g parentName tparams ft =

      let fname = Utils.scalaEscapeName (Core.unName (Core.fieldTypeName ft))
          ftyp = Core.fieldTypeType ft
          caseName = Syntax.NameData {
                Syntax.nameDataValue = (Syntax.PredefString fname)}
          isUnit =
                  case (Strip.deannotateType ftyp) of
                    Core.TypeUnit -> True
                    Core.TypeRecord v0 -> Equality.equal (Lists.length v0) 0
                    _ -> False
          parentType =
                  Logic.ifElse (Lists.null tparams) (Syntax.TypeRef (Syntax.RefTypeName (Syntax.NameType {
                    Syntax.nameTypeValue = parentName}))) (Syntax.TypeApply (Syntax.ApplyType {
                    Syntax.applyTypeTpe = (Syntax.TypeRef (Syntax.RefTypeName (Syntax.NameType {
                      Syntax.nameTypeValue = parentName}))),
                    Syntax.applyTypeArgs = (Lists.map typeParamToTypeVar tparams)}))
      in (Eithers.bind (encodeType cx g ftyp) (\sftyp -> Right (Syntax.StatDefn (Syntax.DefnEnumCase (Syntax.EnumCaseDefn {
        Syntax.enumCaseDefnMods = [],
        Syntax.enumCaseDefnName = caseName,
        Syntax.enumCaseDefnTparams = [],
        Syntax.enumCaseDefnCtor = Syntax.PrimaryCtor {
          Syntax.primaryCtorMods = [],
          Syntax.primaryCtorName = (Syntax.NameValue ""),
          Syntax.primaryCtorParamss = [
            Logic.ifElse isUnit [] [
              Syntax.ParamData {
                Syntax.paramDataMods = [],
                Syntax.paramDataName = (Syntax.NameValue "value"),
                Syntax.paramDataDecltpe = (Just sftyp),
                Syntax.paramDataDefault = Nothing}]]},
        Syntax.enumCaseDefnInits = [
          Syntax.Init {
            Syntax.initTpe = parentType,
            Syntax.initName = (Syntax.NameValue ""),
            Syntax.initArgss = []}]})))))
-- | Convert a field type to a Scala parameter
fieldToParam :: t0 -> t1 -> Core.FieldType -> Either Errors.Error Syntax.ParamData
fieldToParam cx g ft =

      let fname = Utils.scalaEscapeName (Core.unName (Core.fieldTypeName ft))
          ftyp = Core.fieldTypeType ft
      in (Eithers.bind (encodeType cx g ftyp) (\sftyp -> Right (Syntax.ParamData {
        Syntax.paramDataMods = [],
        Syntax.paramDataName = (Syntax.NameValue fname),
        Syntax.paramDataDecltpe = (Just sftyp),
        Syntax.paramDataDefault = Nothing})))
-- | Find the domain type from annotations
findDomain :: t0 -> Graph.Graph -> M.Map Core.Name Core.Term -> Either Errors.Error Core.Type
findDomain cx g meta =
    Eithers.bind (Eithers.bimap (\_de -> Errors.ErrorOther (Errors.OtherError (Errors.unDecodingError _de))) (\_a -> _a) (Annotations.getType g meta)) (\r -> Maybes.maybe (Left (Errors.ErrorOther (Errors.OtherError "expected a typed term"))) (\t -> case (Strip.deannotateType t) of
      Core.TypeFunction v0 -> Right (Core.functionTypeDomain v0)
      _ -> Left (Errors.ErrorOther (Errors.OtherError "expected a function type"))) r)
-- | Find import statements for the module
findImports :: t0 -> Graph.Graph -> Packaging.Module -> Either Errors.Error [Syntax.Stat]
findImports cx g mod =
    Eithers.bind (Analysis.moduleDependencyModuleNames cx g False False True False mod) (\elImps -> Eithers.bind (Analysis.moduleDependencyModuleNames cx g False True False False mod) (\primImps -> Right (Lists.concat [
      Lists.map toElImport (Sets.toList elImps),
      (Lists.map toPrimImport (Sets.toList primImps))])))
-- | Find the Scala domain type for a function from annotations
findSdom :: t0 -> Graph.Graph -> M.Map Core.Name Core.Term -> Either Errors.Error (Maybe Syntax.Type)
findSdom cx g meta =
    Eithers.bind (Eithers.bimap (\_de -> Errors.ErrorOther (Errors.OtherError (Errors.unDecodingError _de))) (\_a -> _a) (Annotations.getType g meta)) (\mtyp -> Maybes.maybe (Right Nothing) (\t -> case (Strip.deannotateType t) of
      Core.TypeFunction v0 ->
        let dom = Core.functionTypeDomain v0
        in (Eithers.bind (encodeType cx g dom) (\sdom -> Right (Just sdom)))
      Core.TypeForall v0 -> case (Strip.deannotateType (Core.forallTypeBody v0)) of
        Core.TypeFunction v1 ->
          let dom2 = Core.functionTypeDomain v1
          in (Eithers.bind (encodeType cx g dom2) (\sdom2 -> Right (Just sdom2)))
        _ -> Right Nothing
      _ -> Eithers.bind (encodeType cx g t) (\st -> Right (Just st))) mtyp)
-- | Convert a Hydra module to Scala source code
moduleToScala :: Packaging.Module -> [Packaging.Definition] -> t0 -> Graph.Graph -> Either Errors.Error (M.Map String String)
moduleToScala mod defs cx g =
    Eithers.bind (constructModule cx g mod defs) (\pkg ->
      let s = Serialization.printExpr (Serialization.parenthesize (Serde.pkgToExpr pkg))
      in (Right (Maps.singleton (Names.moduleNameToFilePath Util.CaseConventionCamel (Packaging.FileExtension "scala") (Packaging.moduleName mod)) s)))
-- | Strip wrap eliminations from terms (newtypes are erased in Scala)
stripWrapEliminations :: Core.Term -> Core.Term
stripWrapEliminations t =
    case (Strip.deannotateAndDetypeTerm t) of
      Core.TermApplication v0 ->
        let appFun = Core.applicationFunction v0
            appArg = Core.applicationArgument v0
        in case (Strip.deannotateAndDetypeTerm appFun) of
          Core.TermUnwrap _ -> stripWrapEliminations appArg
          Core.TermApplication v1 ->
            let innerFun = Core.applicationFunction v1
                innerArg = Core.applicationArgument v1
            in case (Strip.deannotateAndDetypeTerm innerFun) of
              Core.TermUnwrap _ -> stripWrapEliminations (Core.TermApplication (Core.Application {
                Core.applicationFunction = innerArg,
                Core.applicationArgument = appArg}))
              _ -> t
          _ -> t
      _ -> t
-- | Create an element import statement
toElImport :: Packaging.ModuleName -> Syntax.Stat
toElImport ns =
    Syntax.StatImportExport (Syntax.ImportExportStatImport (Syntax.Import {
      Syntax.importImporters = [
        Syntax.Importer {
          Syntax.importerRef = (Syntax.RefDataName (Syntax.NameData {
            Syntax.nameDataValue = (Syntax.PredefString (Strings.intercalate "." (Strings.splitOn "." (Packaging.unModuleName ns))))})),
          Syntax.importerImportees = [
            Syntax.ImporteeWildcard]}]}))
-- | Create a primitive import statement
toPrimImport :: Packaging.ModuleName -> Syntax.Stat
toPrimImport ns =
    Syntax.StatImportExport (Syntax.ImportExportStatImport (Syntax.Import {
      Syntax.importImporters = [
        Syntax.Importer {
          Syntax.importerRef = (Syntax.RefDataName (Syntax.NameData {
            Syntax.nameDataValue = (Syntax.PredefString (Strings.intercalate "." (Strings.splitOn "." (Packaging.unModuleName ns))))})),
          Syntax.importerImportees = []}]}))
-- | Convert a type parameter to a type variable reference
typeParamToTypeVar :: Syntax.ParamType -> Syntax.Type
typeParamToTypeVar tp =

      let n = Syntax.paramTypeName tp
          s =
                  case n of
                    Syntax.NameValue v0 -> v0
                    _ -> ""
      in (Syntax.TypeVar (Syntax.VarType {
        Syntax.varTypeName = Syntax.NameType {
          Syntax.nameTypeValue = s}}))
