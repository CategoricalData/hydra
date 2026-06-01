-- Note: this is an automatically generated file. Do not edit.
-- | Graph context extension and type scheme conversion

module Hydra.Scoping where
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Haskell.Lib.Lists as Lists
import qualified Hydra.Haskell.Lib.Literals as Literals
import qualified Hydra.Haskell.Lib.Logic as Logic
import qualified Hydra.Haskell.Lib.Maps as Maps
import qualified Hydra.Haskell.Lib.Math as Math
import qualified Hydra.Haskell.Lib.Maybes as Maybes
import qualified Hydra.Haskell.Lib.Pairs as Pairs
import qualified Hydra.Haskell.Lib.Sets as Sets
import qualified Hydra.Haskell.Lib.Strings as Strings
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Topology as Topology
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Extend a graph by descending into a lambda body
extendGraphForLambda :: Graph.Graph -> Core.Lambda -> Graph.Graph
extendGraphForLambda g lam =

      let var = Core.lambdaParameter lam
      in Graph.Graph {
        Graph.graphBoundTerms = (Graph.graphBoundTerms g),
        Graph.graphBoundTypes = (Maybes.maybe (Graph.graphBoundTypes g) (\dom -> Maps.insert var (fTypeToTypeScheme dom) (Graph.graphBoundTypes g)) (Core.lambdaDomain lam)),
        Graph.graphClassConstraints = (Graph.graphClassConstraints g),
        Graph.graphLambdaVariables = (Sets.insert var (Graph.graphLambdaVariables g)),
        Graph.graphMetadata = (Maps.delete var (Graph.graphMetadata g)),
        Graph.graphPrimitives = (Graph.graphPrimitives g),
        Graph.graphSchemaTypes = (Graph.graphSchemaTypes g),
        Graph.graphTypeVariables = (Graph.graphTypeVariables g)}
-- | Extend a graph by descending into a let body
extendGraphForLet :: (Graph.Graph -> Core.Binding -> Maybe Core.Term) -> Graph.Graph -> Core.Let -> Graph.Graph
extendGraphForLet forBinding g letrec =

      let bindings = Core.letBindings letrec
          g2 = extendGraphWithBindings bindings g
      in Graph.Graph {
        Graph.graphBoundTerms = (Maps.union (Maps.fromList (Lists.map (\b -> (Core.bindingName b, (Core.bindingTerm b))) bindings)) (Graph.graphBoundTerms g)),
        Graph.graphBoundTypes = (Maps.union (Maps.fromList (Maybes.cat (Lists.map (\b -> Maybes.map (\ts -> (Core.bindingName b, ts)) (Core.bindingTypeScheme b)) bindings))) (Graph.graphBoundTypes g)),
        Graph.graphClassConstraints = (Graph.graphClassConstraints g),
        Graph.graphLambdaVariables = (Lists.foldl (\s -> \b -> Sets.delete (Core.bindingName b) s) (Graph.graphLambdaVariables g) bindings),
        Graph.graphMetadata = (Graph.graphMetadata (Lists.foldl (\gAcc -> \b ->
          let m = Graph.graphMetadata gAcc
              newMeta = Maybes.maybe (Maps.delete (Core.bindingName b) m) (\t -> Maps.insert (Core.bindingName b) t m) (forBinding gAcc b)
          in Graph.Graph {
            Graph.graphBoundTerms = (Graph.graphBoundTerms gAcc),
            Graph.graphBoundTypes = (Graph.graphBoundTypes gAcc),
            Graph.graphClassConstraints = (Graph.graphClassConstraints gAcc),
            Graph.graphLambdaVariables = (Graph.graphLambdaVariables gAcc),
            Graph.graphMetadata = newMeta,
            Graph.graphPrimitives = (Graph.graphPrimitives gAcc),
            Graph.graphSchemaTypes = (Graph.graphSchemaTypes gAcc),
            Graph.graphTypeVariables = (Graph.graphTypeVariables gAcc)}) g2 bindings)),
        Graph.graphPrimitives = (Graph.graphPrimitives g),
        Graph.graphSchemaTypes = (Graph.graphSchemaTypes g),
        Graph.graphTypeVariables = (Graph.graphTypeVariables g)}
-- | Extend a graph by descending into a type lambda body
extendGraphForTypeLambda :: Graph.Graph -> Core.TypeLambda -> Graph.Graph
extendGraphForTypeLambda g tlam =

      let name = Core.typeLambdaParameter tlam
      in Graph.Graph {
        Graph.graphBoundTerms = (Graph.graphBoundTerms g),
        Graph.graphBoundTypes = (Graph.graphBoundTypes g),
        Graph.graphClassConstraints = (Graph.graphClassConstraints g),
        Graph.graphLambdaVariables = (Graph.graphLambdaVariables g),
        Graph.graphMetadata = (Graph.graphMetadata g),
        Graph.graphPrimitives = (Graph.graphPrimitives g),
        Graph.graphSchemaTypes = (Graph.graphSchemaTypes g),
        Graph.graphTypeVariables = (Sets.insert name (Graph.graphTypeVariables g))}
-- | Add bindings to an existing graph
extendGraphWithBindings :: [Core.Binding] -> Graph.Graph -> Graph.Graph
extendGraphWithBindings bindings g =

      let newTerms = Maps.fromList (Lists.map (\b -> (Core.bindingName b, (Core.bindingTerm b))) bindings)
          newTypes =
                  Maps.fromList (Maybes.cat (Lists.map (\b -> Maybes.map (\ts -> (Core.bindingName b, ts)) (Core.bindingTypeScheme b)) bindings))
      in Graph.Graph {
        Graph.graphBoundTerms = (Maps.union newTerms (Graph.graphBoundTerms g)),
        Graph.graphBoundTypes = (Maps.union newTypes (Graph.graphBoundTypes g)),
        Graph.graphClassConstraints = (Graph.graphClassConstraints g),
        Graph.graphLambdaVariables = (Graph.graphLambdaVariables g),
        Graph.graphMetadata = (Graph.graphMetadata g),
        Graph.graphPrimitives = (Graph.graphPrimitives g),
        Graph.graphSchemaTypes = (Graph.graphSchemaTypes g),
        Graph.graphTypeVariables = (Graph.graphTypeVariables g)}
-- | Convert a forall type to a type scheme
fTypeToTypeScheme :: Core.Type -> Core.TypeScheme
fTypeToTypeScheme typ =

      let stripAnnotations =
              \t -> case t of
                Core.TypeAnnotated v0 -> stripAnnotations (Core.annotatedTypeBody v0)
                _ -> t
          gatherForall =
                  \vars -> \typ2 -> case (stripAnnotations typ2) of
                    Core.TypeForall v0 -> gatherForall (Lists.cons (Core.forallTypeParameter v0) vars) (Core.forallTypeBody v0)
                    _ -> Core.TypeScheme {
                      Core.typeSchemeVariables = (Lists.reverse vars),
                      Core.typeSchemeBody = typ2,
                      Core.typeSchemeConstraints = Nothing}
      in (gatherForall [] typ)
-- | Convert a TermSignature to a TypeScheme, erasing parameter names, descriptions, and laziness flags.
termSignatureToTypeScheme :: Typing.TermSignature -> Core.TypeScheme
termSignatureToTypeScheme sig =

      let typeParams = Typing.termSignatureTypeParameters sig
          params = Typing.termSignatureParameters sig
          result = Typing.termSignatureResult sig
          variables = Lists.map (\tp -> Typing.typeParameterName tp) typeParams
          body =
                  Lists.foldl (\acc -> \p -> Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Typing.parameterType p),
                    Core.functionTypeCodomain = acc})) (Typing.resultType result) (Lists.reverse params)
          hasConstraints =
                  Lists.foldl (\acc -> \tp -> Logic.or acc (Logic.not (Lists.null (Typing.typeParameterConstraints tp)))) False typeParams
          constraints =
                  Logic.ifElse hasConstraints (Just (Maps.fromList (Lists.map (\tp -> (
                    Typing.typeParameterName tp,
                    Core.TypeVariableConstraints {
                      Core.typeVariableConstraintsClasses = (Typing.typeParameterConstraints tp)})) typeParams))) Nothing
      in Core.TypeScheme {
        Core.typeSchemeVariables = variables,
        Core.typeSchemeBody = body,
        Core.typeSchemeConstraints = constraints}
-- | Convert a type scheme to a forall type
typeSchemeToFType :: Core.TypeScheme -> Core.Type
typeSchemeToFType ts =

      let vars = Core.typeSchemeVariables ts
          body = Core.typeSchemeBody ts
      in (Lists.foldl (\t -> \v -> Core.TypeForall (Core.ForallType {
        Core.forallTypeParameter = v,
        Core.forallTypeBody = t})) body (Lists.reverse vars))
-- | Convert a TypeScheme to a TermSignature. Type variables and class constraints are preserved exactly. Value-parameter names are synthesized as arg0, arg1, .... Per-parameter descriptions are nothing and isLazy defaults to false.
typeSchemeToTermSignature :: Core.TypeScheme -> Typing.TermSignature
typeSchemeToTermSignature ts =

      let variables = Core.typeSchemeVariables ts
          body = Core.typeSchemeBody ts
          constraintsMap = Maybes.fromMaybe Maps.empty (Core.typeSchemeConstraints ts)
          typeParams =
                  Lists.map (\v -> Typing.TypeParameter {
                    Typing.typeParameterName = v,
                    Typing.typeParameterConstraints = (Maybes.maybe [] (\tvm -> Core.typeVariableConstraintsClasses tvm) (Maps.lookup v constraintsMap))}) variables
          peel =
                  \acc -> \t -> case t of
                    Core.TypeFunction v0 -> peel (Lists.cons (Core.functionTypeDomain v0) acc) (Core.functionTypeCodomain v0)
                    _ -> (Lists.reverse acc, t)
          peeled = peel [] body
          paramTypes = Pairs.first peeled
          resultType = Pairs.second peeled
          params =
                  Lists.reverse (Pairs.first (Lists.foldl (\acc -> \ty ->
                    let pairAcc = Pairs.first acc
                        i = Pairs.second acc
                    in (
                      Lists.cons (Typing.Parameter {
                        Typing.parameterName = (Core.Name (Strings.cat [
                          "arg",
                          (Literals.showInt32 i)])),
                        Typing.parameterDescription = Nothing,
                        Typing.parameterType = ty,
                        Typing.parameterIsLazy = False}) pairAcc,
                      (Math.add i 1))) ([], 0) paramTypes))
          result =
                  Typing.Result {
                    Typing.resultDescription = Nothing,
                    Typing.resultType = resultType}
      in Typing.TermSignature {
        Typing.termSignatureTypeParameters = typeParams,
        Typing.termSignatureParameters = params,
        Typing.termSignatureResult = result}
