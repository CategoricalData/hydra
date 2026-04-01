-- Note: this is an automatically generated file. Do not edit.

-- | Simple, one-way adapters for types and terms

module Hydra.Adapt where

import qualified Hydra.Coders as Coders
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Hoisting as Hoisting
import qualified Hydra.Inference as Inference
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Literals as Literals_
import qualified Hydra.Module as Module
import qualified Hydra.Names as Names
import qualified Hydra.Reduction as Reduction
import qualified Hydra.Reflect as Reflect
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Schemas as Schemas
import qualified Hydra.Show.Core as Core_
import qualified Hydra.Show.Errors as Errors_
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Adapt a graph and its schema to the given language constraints. The doExpand flag controls eta expansion of partial applications. Adaptation is type-preserving: binding-level TypeSchemes are adapted (not stripped). Note: case statement hoisting is done separately, prior to adaptation. The els0 parameter provides the original ordered bindings. Returns both the adapted graph and the ordered adapted bindings.
adaptDataGraph :: Coders.LanguageConstraints -> Bool -> [Core.Binding] -> Context.Context -> Graph.Graph -> Either String (Graph.Graph, [Core.Binding])
adaptDataGraph constraints doExpand els0 cx graph0 =

      let transformTerm =
              \g -> \term ->
                let tx = g
                    t1 = Rewriting.unshadowVariables (pushTypeAppsInward term)
                    t2 = Rewriting.unshadowVariables (Logic.ifElse doExpand (pushTypeAppsInward (Reduction.etaExpandTermNew tx t1)) t1)
                in (Rewriting.liftLambdaAboveLet t2)
          transformBinding =
                  \g -> \el -> Core.Binding {
                    Core.bindingName = (Core.bindingName el),
                    Core.bindingTerm = (transformTerm g (Core.bindingTerm el)),
                    Core.bindingType = (Core.bindingType el)}
          litmap = adaptLiteralTypesMap constraints
          prims0 = Graph.graphPrimitives graph0
          schemaTypes0 = Graph.graphSchemaTypes graph0
          schemaBindings = Schemas.typesToElements (Maps.map (\ts -> Rewriting.typeSchemeToFType ts) schemaTypes0)
      in (Eithers.bind (Logic.ifElse (Maps.null schemaTypes0) (Right Maps.empty) (Eithers.bind (Eithers.bimap (\ic -> Errors.unDecodingError (Context.inContextObject ic)) (\x -> x) (Schemas.graphAsTypes cx graph0 schemaBindings)) (\tmap0 -> Eithers.bind (adaptGraphSchema constraints litmap tmap0) (\tmap1 -> Right (Maps.map (\t -> Schemas.typeToTypeScheme t) tmap1))))) (\schemaResult ->
        let adaptedSchemaTypes = schemaResult
            adaptBinding =
                    \el ->
                      let transformed = transformBinding graph0 el
                          wrapped =
                                  Core.TermLet (Core.Let {
                                    Core.letBindings = (Lists.pure transformed),
                                    Core.letBody = Core.TermUnit})
                      in (Eithers.bind (adaptTerm constraints litmap cx graph0 wrapped) (\adapted -> Rewriting.rewriteTermM (adaptLambdaDomains constraints litmap) adapted))
        in (Eithers.bind (Eithers.mapList adaptBinding els0) (\adaptedTerms ->
          let els1Raw = Lists.concat (Lists.map Schemas.termAsBindings adaptedTerms)
              processBinding =
                      \el -> Eithers.bind (Rewriting.rewriteTermM (adaptNestedTypes constraints litmap) (Core.bindingTerm el)) (\newTerm -> Eithers.bind (Maybes.maybe (Right Nothing) (\ts -> Eithers.bind (adaptTypeScheme constraints litmap ts) (\ts1 -> Right (Just ts1))) (Core.bindingType el)) (\adaptedType -> Right (Core.Binding {
                        Core.bindingName = (Core.bindingName el),
                        Core.bindingTerm = newTerm,
                        Core.bindingType = adaptedType})))
          in (Eithers.bind (Eithers.mapList processBinding els1Raw) (\els1 -> Eithers.bind (Eithers.mapList (\kv -> Eithers.bind (adaptPrimitive constraints litmap (Pairs.second kv)) (\prim1 -> Right (Pairs.first kv, prim1))) (Maps.toList prims0)) (\primPairs ->
            let prims1 = Maps.fromList primPairs
                adaptedGraphRaw = Lexical.buildGraph els1 Maps.empty prims1
                adaptedGraph =
                        Graph.Graph {
                          Graph.graphBoundTerms = (Graph.graphBoundTerms adaptedGraphRaw),
                          Graph.graphBoundTypes = (Graph.graphBoundTypes adaptedGraphRaw),
                          Graph.graphClassConstraints = (Graph.graphClassConstraints adaptedGraphRaw),
                          Graph.graphLambdaVariables = (Graph.graphLambdaVariables adaptedGraphRaw),
                          Graph.graphMetadata = (Graph.graphMetadata adaptedGraphRaw),
                          Graph.graphPrimitives = (Graph.graphPrimitives adaptedGraphRaw),
                          Graph.graphSchemaTypes = adaptedSchemaTypes,
                          Graph.graphTypeVariables = (Graph.graphTypeVariables adaptedGraphRaw)}
            in (Right (adaptedGraph, els1)))))))))

-- | Attempt to adapt a floating-point type using the given language constraints
adaptFloatType :: Coders.LanguageConstraints -> Core.FloatType -> Maybe Core.FloatType
adaptFloatType constraints ft =

      let supported = Sets.member ft (Coders.languageConstraintsFloatTypes constraints)
          alt = adaptFloatType constraints
          forUnsupported =
                  \ft2 -> case ft2 of
                    Core.FloatTypeBigfloat -> alt Core.FloatTypeFloat64
                    Core.FloatTypeFloat32 -> alt Core.FloatTypeFloat64
                    Core.FloatTypeFloat64 -> alt Core.FloatTypeBigfloat
      in (Logic.ifElse supported (Just ft) (forUnsupported ft))

-- | Adapt a schema graph to the given language constraints
adaptGraphSchema :: Ord t0 => (Coders.LanguageConstraints -> M.Map Core.LiteralType Core.LiteralType -> M.Map t0 Core.Type -> Either String (M.Map t0 Core.Type))
adaptGraphSchema constraints litmap types0 =

      let mapPair =
              \pair ->
                let name = Pairs.first pair
                    typ = Pairs.second pair
                in (Eithers.bind (adaptType constraints litmap typ) (\typ1 -> Right (name, typ1)))
      in (Eithers.bind (Eithers.mapList mapPair (Maps.toList types0)) (\pairs -> Right (Maps.fromList pairs)))

-- | Attempt to adapt an integer type using the given language constraints
adaptIntegerType :: Coders.LanguageConstraints -> Core.IntegerType -> Maybe Core.IntegerType
adaptIntegerType constraints it =

      let supported = Sets.member it (Coders.languageConstraintsIntegerTypes constraints)
          alt = adaptIntegerType constraints
          forUnsupported =
                  \it2 -> case it2 of
                    Core.IntegerTypeBigint -> Nothing
                    Core.IntegerTypeInt8 -> alt Core.IntegerTypeUint16
                    Core.IntegerTypeInt16 -> alt Core.IntegerTypeUint32
                    Core.IntegerTypeInt32 -> alt Core.IntegerTypeUint64
                    Core.IntegerTypeInt64 -> alt Core.IntegerTypeBigint
                    Core.IntegerTypeUint8 -> alt Core.IntegerTypeInt16
                    Core.IntegerTypeUint16 -> alt Core.IntegerTypeInt32
                    Core.IntegerTypeUint32 -> alt Core.IntegerTypeInt64
                    Core.IntegerTypeUint64 -> alt Core.IntegerTypeBigint
      in (Logic.ifElse supported (Just it) (forUnsupported it))

-- | Rewrite callback for adapting lambda domain types in a term
adaptLambdaDomains :: Coders.LanguageConstraints -> M.Map Core.LiteralType Core.LiteralType -> (t0 -> Either String Core.Term) -> t0 -> Either String Core.Term
adaptLambdaDomains constraints litmap recurse term =
    Eithers.bind (recurse term) (\rewritten -> case rewritten of
      Core.TermFunction v0 -> case v0 of
        Core.FunctionLambda v1 -> Eithers.bind (Maybes.maybe (Right Nothing) (\dom -> Eithers.bind (adaptType constraints litmap dom) (\dom1 -> Right (Just dom1))) (Core.lambdaDomain v1)) (\adaptedDomain -> Right (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
          Core.lambdaParameter = (Core.lambdaParameter v1),
          Core.lambdaDomain = adaptedDomain,
          Core.lambdaBody = (Core.lambdaBody v1)}))))
        _ -> Right (Core.TermFunction v0)
      _ -> Right rewritten)

-- | Convert a literal to a different type
adaptLiteral :: Core.LiteralType -> Core.Literal -> Core.Literal
adaptLiteral lt l =
    case l of
      Core.LiteralBinary v0 -> case lt of
        Core.LiteralTypeString -> Core.LiteralString (Literals.binaryToString v0)
      Core.LiteralBoolean v0 -> case lt of
        Core.LiteralTypeInteger v1 -> Core.LiteralInteger (Literals_.bigintToIntegerValue v1 (Logic.ifElse v0 1 0))
      Core.LiteralFloat v0 -> case lt of
        Core.LiteralTypeFloat v1 -> Core.LiteralFloat (Literals_.bigfloatToFloatValue v1 (Literals_.floatValueToBigfloat v0))
      Core.LiteralInteger v0 -> case lt of
        Core.LiteralTypeInteger v1 -> Core.LiteralInteger (Literals_.bigintToIntegerValue v1 (Literals_.integerValueToBigint v0))

-- | Attempt to adapt a literal type using the given language constraints
adaptLiteralType :: Coders.LanguageConstraints -> Core.LiteralType -> Maybe Core.LiteralType
adaptLiteralType constraints lt =

      let forUnsupported =
              \lt2 -> case lt2 of
                Core.LiteralTypeBinary -> Just Core.LiteralTypeString
                Core.LiteralTypeBoolean -> Maybes.map (\x -> Core.LiteralTypeInteger x) (adaptIntegerType constraints Core.IntegerTypeInt8)
                Core.LiteralTypeFloat v0 -> Maybes.map (\x -> Core.LiteralTypeFloat x) (adaptFloatType constraints v0)
                Core.LiteralTypeInteger v0 -> Maybes.map (\x -> Core.LiteralTypeInteger x) (adaptIntegerType constraints v0)
                _ -> Nothing
      in (Logic.ifElse (literalTypeSupported constraints lt) Nothing (forUnsupported lt))

-- | Derive a map of adapted literal types for the given language constraints
adaptLiteralTypesMap :: Coders.LanguageConstraints -> M.Map Core.LiteralType Core.LiteralType
adaptLiteralTypesMap constraints =

      let tryType = \lt -> Maybes.maybe Nothing (\lt2 -> Just (lt, lt2)) (adaptLiteralType constraints lt)
      in (Maps.fromList (Maybes.cat (Lists.map tryType Reflect.literalTypes)))

-- | Adapt a literal value using the given language constraints
adaptLiteralValue :: Ord t0 => (M.Map t0 Core.LiteralType -> t0 -> Core.Literal -> Core.Literal)
adaptLiteralValue litmap lt l =
    Maybes.maybe (Core.LiteralString (Core_.literal l)) (\lt2 -> adaptLiteral lt2 l) (Maps.lookup lt litmap)

-- | Rewrite callback for adapting nested let binding TypeSchemes in a term
adaptNestedTypes :: Coders.LanguageConstraints -> M.Map Core.LiteralType Core.LiteralType -> (t0 -> Either String Core.Term) -> t0 -> Either String Core.Term
adaptNestedTypes constraints litmap recurse term =
    Eithers.bind (recurse term) (\rewritten -> case rewritten of
      Core.TermLet v0 ->
        let adaptB =
                \b -> Eithers.bind (Maybes.maybe (Right Nothing) (\ts -> Eithers.bind (adaptTypeScheme constraints litmap ts) (\ts1 -> Right (Just ts1))) (Core.bindingType b)) (\adaptedBType -> Right (Core.Binding {
                  Core.bindingName = (Core.bindingName b),
                  Core.bindingTerm = (Core.bindingTerm b),
                  Core.bindingType = adaptedBType}))
        in (Eithers.bind (Eithers.mapList adaptB (Core.letBindings v0)) (\adaptedBindings -> Right (Core.TermLet (Core.Let {
          Core.letBindings = adaptedBindings,
          Core.letBody = (Core.letBody v0)}))))
      _ -> Right rewritten)

-- | Adapt a primitive to the given language constraints, prior to inference
adaptPrimitive :: Coders.LanguageConstraints -> M.Map Core.LiteralType Core.LiteralType -> Graph.Primitive -> Either String Graph.Primitive
adaptPrimitive constraints litmap prim0 =

      let ts0 = Graph.primitiveType prim0
      in (Eithers.bind (adaptTypeScheme constraints litmap ts0) (\ts1 -> Right (Graph.Primitive {
        Graph.primitiveName = (Graph.primitiveName prim0),
        Graph.primitiveType = ts1,
        Graph.primitiveImplementation = (Graph.primitiveImplementation prim0)})))

-- | Adapt a term using the given language constraints
adaptTerm :: Coders.LanguageConstraints -> M.Map Core.LiteralType Core.LiteralType -> Context.Context -> Graph.Graph -> Core.Term -> Either String Core.Term
adaptTerm constraints litmap cx graph term0 =

      let rewrite =
              \recurse -> \term02 ->
                let forSupported =
                        \term -> case term of
                          Core.TermLiteral v0 ->
                            let lt = Reflect.literalType v0
                            in (Right (Just (Logic.ifElse (literalTypeSupported constraints lt) term (Core.TermLiteral (adaptLiteralValue litmap lt v0)))))
                          _ -> Right (Just term)
                    forUnsupported =
                            \term ->
                              let forNonNull =
                                      \alts -> Eithers.bind (tryTerm (Lists.head alts)) (\mterm -> Maybes.maybe (tryAlts (Lists.tail alts)) (\t -> Right (Just t)) mterm)
                                  tryAlts = \alts -> Logic.ifElse (Lists.null alts) (Right Nothing) (forNonNull alts)
                              in (Eithers.bind (termAlternatives cx graph term) (\alts0 -> tryAlts alts0))
                    tryTerm =
                            \term ->
                              let supportedVariant = Sets.member (Reflect.termVariant term) (Coders.languageConstraintsTermVariants constraints)
                              in (Logic.ifElse supportedVariant (forSupported term) (forUnsupported term))
                in (Eithers.bind (recurse term02) (\term1 -> case term1 of
                  Core.TermTypeApplication v0 -> Eithers.bind (adaptType constraints litmap (Core.typeApplicationTermType v0)) (\atyp -> Right (Core.TermTypeApplication (Core.TypeApplicationTerm {
                    Core.typeApplicationTermBody = (Core.typeApplicationTermBody v0),
                    Core.typeApplicationTermType = atyp})))
                  Core.TermTypeLambda _ -> Right term1
                  _ -> Eithers.bind (tryTerm term1) (\mterm -> Maybes.maybe (Left (Strings.cat2 "no alternatives for term: " (Core_.term term1))) (\term2 -> Right term2) mterm)))
      in (Rewriting.rewriteTermM rewrite term0)

-- | Adapt a term using the constraints of a given language
adaptTermForLanguage :: Coders.Language -> Context.Context -> Graph.Graph -> Core.Term -> Either String Core.Term
adaptTermForLanguage lang cx g term =

      let constraints = Coders.languageConstraints lang
          litmap = adaptLiteralTypesMap constraints
      in (adaptTerm constraints litmap cx g term)

-- | Adapt a type using the given language constraints
adaptType :: Coders.LanguageConstraints -> M.Map Core.LiteralType Core.LiteralType -> Core.Type -> Either String Core.Type
adaptType constraints litmap type0 =

      let forSupported =
              \typ -> case typ of
                Core.TypeLiteral v0 -> Logic.ifElse (literalTypeSupported constraints v0) (Just typ) (Maybes.maybe (Just (Core.TypeLiteral Core.LiteralTypeString)) (\lt2 -> Just (Core.TypeLiteral lt2)) (Maps.lookup v0 litmap))
                _ -> Just typ
          forUnsupported =
                  \typ ->
                    let tryAlts =
                            \alts -> Logic.ifElse (Lists.null alts) Nothing (Maybes.maybe (tryAlts (Lists.tail alts)) (\t -> Just t) (tryType (Lists.head alts)))
                        alts0 = typeAlternatives typ
                    in (tryAlts alts0)
          tryType =
                  \typ ->
                    let supportedVariant = Sets.member (Reflect.typeVariant typ) (Coders.languageConstraintsTypeVariants constraints)
                    in (Logic.ifElse supportedVariant (forSupported typ) (forUnsupported typ))
          rewrite =
                  \recurse -> \typ -> Eithers.bind (recurse typ) (\type1 -> Maybes.maybe (Left (Strings.cat2 "no alternatives for type: " (Core_.type_ typ))) (\type2 -> Right type2) (tryType type1))
      in (Rewriting.rewriteTypeM rewrite type0)

-- | Adapt a type using the constraints of a given language
adaptTypeForLanguage :: Coders.Language -> Core.Type -> Either String Core.Type
adaptTypeForLanguage lang typ =

      let constraints = Coders.languageConstraints lang
          litmap = adaptLiteralTypesMap constraints
      in (adaptType constraints litmap typ)

-- | Adapt a type scheme to the given language constraints, prior to inference
adaptTypeScheme :: Coders.LanguageConstraints -> M.Map Core.LiteralType Core.LiteralType -> Core.TypeScheme -> Either String Core.TypeScheme
adaptTypeScheme constraints litmap ts0 =

      let vars0 = Core.typeSchemeVariables ts0
          t0 = Core.typeSchemeType ts0
      in (Eithers.bind (adaptType constraints litmap t0) (\t1 -> Right (Core.TypeScheme {
        Core.typeSchemeVariables = vars0,
        Core.typeSchemeType = t1,
        Core.typeSchemeConstraints = (Core.typeSchemeConstraints ts0)})))

-- | Compose two coders into a single coder
composeCoders :: Util.Coder t0 t1 -> Util.Coder t1 t2 -> Util.Coder t0 t2
composeCoders c1 c2 =
    Util.Coder {
      Util.coderEncode = (\cx -> \a -> Eithers.bind (Util.coderEncode c1 cx a) (\b1 -> Util.coderEncode c2 cx b1)),
      Util.coderDecode = (\cx -> \c -> Eithers.bind (Util.coderDecode c2 cx c) (\b2 -> Util.coderDecode c1 cx b2))}

-- | Given a data graph along with language constraints, original ordered bindings, and a designated list of namespaces, adapt the graph to the language constraints, then return the processed graph along with term definitions grouped by namespace (in the order of the input namespaces). Inference is performed before adaptation if bindings lack type annotations. Hoisting must preserve type schemes; if any binding loses its type scheme after hoisting, the pipeline fails. Adaptation preserves type application/lambda wrappers and adapts embedded types. Post-adaptation inference is performed to ensure binding TypeSchemes are fully consistent. The doExpand flag controls eta expansion. The doHoistCaseStatements flag controls case statement hoisting (needed for Python). The doHoistPolymorphicLetBindings flag controls polymorphic let binding hoisting (needed for Java). The originalBindings parameter provides the original ordered bindings (from module elements).
dataGraphToDefinitions :: Coders.LanguageConstraints -> Bool -> Bool -> Bool -> Bool -> [Core.Binding] -> Graph.Graph -> [Module.Namespace] -> Context.Context -> Either String (Graph.Graph, [[Module.TermDefinition]])
dataGraphToDefinitions constraints doInfer doExpand doHoistCaseStatements doHoistPolymorphicLetBindings originalBindings graph0 namespaces cx =

      let namespacesSet = Sets.fromList namespaces
          isParentBinding =
                  \b -> Maybes.maybe False (\ns -> Sets.member ns namespacesSet) (Names.namespaceOf (Core.bindingName b))
          hoistCases =
                  \bindings ->
                    let stripped =
                            Lists.map (\b -> Core.Binding {
                              Core.bindingName = (Core.bindingName b),
                              Core.bindingTerm = (Rewriting.stripTypeLambdas (Core.bindingTerm b)),
                              Core.bindingType = (Core.bindingType b)}) bindings
                        term0 =
                                Core.TermLet (Core.Let {
                                  Core.letBindings = stripped,
                                  Core.letBody = Core.TermUnit})
                        unshadowed0 = Schemas.termAsBindings (Rewriting.unshadowVariables term0)
                        hoisted = Hoisting.hoistCaseStatementsInGraph unshadowed0
                        term1 =
                                Core.TermLet (Core.Let {
                                  Core.letBindings = hoisted,
                                  Core.letBody = Core.TermUnit})
                    in (Schemas.termAsBindings (Rewriting.unshadowVariables term1))
          hoistPoly =
                  \bindings ->
                    let letBefore =
                            Core.Let {
                              Core.letBindings = bindings,
                              Core.letBody = Core.TermUnit}
                        letAfter = Hoisting.hoistPolymorphicLetBindings isParentBinding letBefore
                    in (Core.letBindings letAfter)
          checkBindingsTyped =
                  \debugLabel -> \bindings ->
                    let untypedBindings =
                            Lists.map (\b -> Core.unName (Core.bindingName b)) (Lists.filter (\b -> Logic.not (Maybes.isJust (Core.bindingType b))) bindings)
                    in (Logic.ifElse (Lists.null untypedBindings) (Right bindings) (Left (Strings.cat [
                      "Found untyped bindings (",
                      debugLabel,
                      "): ",
                      (Strings.intercalate ", " untypedBindings)])))
          normalizeBindings =
                  \bindings -> Lists.map (\b -> Core.Binding {
                    Core.bindingName = (Core.bindingName b),
                    Core.bindingTerm = (pushTypeAppsInward (Core.bindingTerm b)),
                    Core.bindingType = (Core.bindingType b)}) bindings
          rebuildGraph =
                  \bindings ->
                    let g = Lexical.buildGraph bindings Maps.empty (Graph.graphPrimitives graph0)
                    in Graph.Graph {
                      Graph.graphBoundTerms = (Graph.graphBoundTerms g),
                      Graph.graphBoundTypes = (Graph.graphBoundTypes g),
                      Graph.graphClassConstraints = (Graph.graphClassConstraints g),
                      Graph.graphLambdaVariables = (Graph.graphLambdaVariables g),
                      Graph.graphMetadata = (Graph.graphMetadata g),
                      Graph.graphPrimitives = (Graph.graphPrimitives g),
                      Graph.graphSchemaTypes = (Graph.graphSchemaTypes graph0),
                      Graph.graphTypeVariables = (Graph.graphTypeVariables g)}
          bins0 = originalBindings
          bins1 = Logic.ifElse doHoistCaseStatements (hoistCases bins0) bins0
      in (Eithers.bind (Logic.ifElse doInfer (Eithers.map (\result -> Pairs.second (Pairs.first result)) (Eithers.bimap (\ic -> Errors_.error (Context.inContextObject ic)) (\x -> x) (Inference.inferGraphTypes cx bins1 (rebuildGraph bins1)))) (checkBindingsTyped "after case hoisting" bins1)) (\bins2 -> Eithers.bind (Logic.ifElse doHoistPolymorphicLetBindings (checkBindingsTyped "after let hoisting" (hoistPoly bins2)) (Right bins2)) (\bins3 -> Eithers.bind (adaptDataGraph constraints doExpand bins3 cx (rebuildGraph bins3)) (\adaptResult ->
        let adapted = Pairs.first adaptResult
            adaptedBindings = Pairs.second adaptResult
        in (Eithers.bind (checkBindingsTyped "after adaptation" adaptedBindings) (\bins4 ->
          let bins5 = normalizeBindings bins4
              toDef =
                      \el -> Maybes.map (\ts -> Module.TermDefinition {
                        Module.termDefinitionName = (Core.bindingName el),
                        Module.termDefinitionTerm = (Core.bindingTerm el),
                        Module.termDefinitionType = (Just ts)}) (Core.bindingType el)
              selectedElements =
                      Lists.filter (\el -> Maybes.maybe False (\ns -> Sets.member ns namespacesSet) (Names.namespaceOf (Core.bindingName el))) bins5
              elementsByNamespace =
                      Lists.foldl (\acc -> \el -> Maybes.maybe acc (\ns ->
                        let existing = Maybes.maybe [] Equality.identity (Maps.lookup ns acc)
                        in (Maps.insert ns (Lists.concat2 existing [
                          el]) acc)) (Names.namespaceOf (Core.bindingName el))) Maps.empty selectedElements
              defsGrouped =
                      Lists.map (\ns ->
                        let elsForNs = Maybes.maybe [] Equality.identity (Maps.lookup ns elementsByNamespace)
                        in (Maybes.cat (Lists.map toDef elsForNs))) namespaces
              g = Lexical.buildGraph bins5 Maps.empty (Graph.graphPrimitives adapted)
          in (Right (Graph.Graph {
            Graph.graphBoundTerms = (Graph.graphBoundTerms g),
            Graph.graphBoundTypes = (Graph.graphBoundTypes g),
            Graph.graphClassConstraints = (Graph.graphClassConstraints g),
            Graph.graphLambdaVariables = (Graph.graphLambdaVariables g),
            Graph.graphMetadata = (Graph.graphMetadata g),
            Graph.graphPrimitives = (Graph.graphPrimitives g),
            Graph.graphSchemaTypes = (Graph.graphSchemaTypes adapted),
            Graph.graphTypeVariables = (Graph.graphTypeVariables g)}, defsGrouped))))))))

-- | Check if a literal type is supported by the given language constraints
literalTypeSupported :: Coders.LanguageConstraints -> Core.LiteralType -> Bool
literalTypeSupported constraints lt =

      let forType =
              \lt2 -> case lt2 of
                Core.LiteralTypeFloat v0 -> Sets.member v0 (Coders.languageConstraintsFloatTypes constraints)
                Core.LiteralTypeInteger v0 -> Sets.member v0 (Coders.languageConstraintsIntegerTypes constraints)
                _ -> True
      in (Logic.ifElse (Sets.member (Reflect.literalTypeVariant lt) (Coders.languageConstraintsLiteralVariants constraints)) (forType lt) False)

-- | Prepare a float type, substituting unsupported types
prepareFloatType :: Core.FloatType -> (Core.FloatType, ((Core.FloatValue -> Core.FloatValue), (S.Set String)))
prepareFloatType ft =
    case ft of
      Core.FloatTypeBigfloat -> (Core.FloatTypeFloat64, ((\v -> case v of
        Core.FloatValueBigfloat v1 -> Core.FloatValueFloat64 (Literals.bigfloatToFloat64 v1)
        _ -> v), (Sets.fromList [
        "replace arbitrary-precision floating-point numbers with 64-bit floating-point numbers (doubles)"])))
      _ -> prepareSame ft

-- | Prepare an integer type, substituting unsupported types
prepareIntegerType :: Core.IntegerType -> (Core.IntegerType, ((Core.IntegerValue -> Core.IntegerValue), (S.Set String)))
prepareIntegerType it =
    case it of
      Core.IntegerTypeBigint -> (Core.IntegerTypeInt64, ((\v -> case v of
        Core.IntegerValueBigint v1 -> Core.IntegerValueInt64 (Literals.bigintToInt64 v1)
        _ -> v), (Sets.fromList [
        "replace arbitrary-precision integers with 64-bit integers"])))
      Core.IntegerTypeUint8 -> (Core.IntegerTypeInt8, ((\v -> case v of
        Core.IntegerValueUint8 v1 -> Core.IntegerValueInt8 (Literals.bigintToInt8 (Literals.uint8ToBigint v1))
        _ -> v), (Sets.fromList [
        "replace unsigned 8-bit integers with signed 8-bit integers"])))
      Core.IntegerTypeUint32 -> (Core.IntegerTypeInt32, ((\v -> case v of
        Core.IntegerValueUint32 v1 -> Core.IntegerValueInt32 (Literals.bigintToInt32 (Literals.uint32ToBigint v1))
        _ -> v), (Sets.fromList [
        "replace unsigned 32-bit integers with signed 32-bit integers"])))
      Core.IntegerTypeUint64 -> (Core.IntegerTypeInt64, ((\v -> case v of
        Core.IntegerValueUint64 v1 -> Core.IntegerValueInt64 (Literals.bigintToInt64 (Literals.uint64ToBigint v1))
        _ -> v), (Sets.fromList [
        "replace unsigned 64-bit integers with signed 64-bit integers"])))
      _ -> prepareSame it

-- | Prepare a literal type, substituting unsupported types
prepareLiteralType :: Core.LiteralType -> (Core.LiteralType, ((Core.Literal -> Core.Literal), (S.Set String)))
prepareLiteralType at =
    case at of
      Core.LiteralTypeBinary -> (Core.LiteralTypeString, ((\v -> case v of
        Core.LiteralBinary v1 -> Core.LiteralString (Literals.binaryToString v1)
        _ -> v), (Sets.fromList [
        "replace binary strings with character strings"])))
      Core.LiteralTypeFloat v0 ->
        let result = prepareFloatType v0
            rtyp = Pairs.first result
            rep = Pairs.first (Pairs.second result)
            msgs = Pairs.second (Pairs.second result)
        in (Core.LiteralTypeFloat rtyp, ((\v -> case v of
          Core.LiteralFloat v1 -> Core.LiteralFloat (rep v1)
          _ -> v), msgs))
      Core.LiteralTypeInteger v0 ->
        let result = prepareIntegerType v0
            rtyp = Pairs.first result
            rep = Pairs.first (Pairs.second result)
            msgs = Pairs.second (Pairs.second result)
        in (Core.LiteralTypeInteger rtyp, ((\v -> case v of
          Core.LiteralInteger v1 -> Core.LiteralInteger (rep v1)
          _ -> v), msgs))
      _ -> prepareSame at

-- | Return a value unchanged with identity transform and no messages
prepareSame :: Ord t2 => (t0 -> (t0, ((t1 -> t1), (S.Set t2))))
prepareSame x = (x, ((\y -> y), Sets.empty))

-- | Prepare a type, substituting unsupported literal types
prepareType :: t0 -> Core.Type -> (Core.Type, ((Core.Term -> Core.Term), (S.Set String)))
prepareType cx typ =
    case (Rewriting.deannotateType typ) of
      Core.TypeLiteral v0 ->
        let result = prepareLiteralType v0
            rtyp = Pairs.first result
            rep = Pairs.first (Pairs.second result)
            msgs = Pairs.second (Pairs.second result)
        in (Core.TypeLiteral rtyp, ((\v -> case v of
          Core.TermLiteral v1 -> Core.TermLiteral (rep v1)
          _ -> v), msgs))
      _ -> prepareSame typ

-- | Normalize a term by pushing TermTypeApplication inward past TermApplication and TermFunction (Lambda). This corrects structures produced by poly-let hoisting and eta expansion, where type applications from inference end up wrapping term applications or lambda abstractions instead of being directly on the polymorphic variable.
pushTypeAppsInward :: Core.Term -> Core.Term
pushTypeAppsInward term =

      let push =
              \body -> \typ -> case body of
                Core.TermApplication v0 -> go (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                    Core.typeApplicationTermBody = (Core.applicationFunction v0),
                    Core.typeApplicationTermType = typ})),
                  Core.applicationArgument = (Core.applicationArgument v0)}))
                Core.TermFunction v0 -> case v0 of
                  Core.FunctionLambda v1 -> go (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.lambdaParameter v1),
                    Core.lambdaDomain = (Core.lambdaDomain v1),
                    Core.lambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.lambdaBody v1),
                      Core.typeApplicationTermType = typ}))})))
                  _ -> Core.TermTypeApplication (Core.TypeApplicationTerm {
                    Core.typeApplicationTermBody = (Core.TermFunction v0),
                    Core.typeApplicationTermType = typ})
                Core.TermLet v0 -> go (Core.TermLet (Core.Let {
                  Core.letBindings = (Core.letBindings v0),
                  Core.letBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                    Core.typeApplicationTermBody = (Core.letBody v0),
                    Core.typeApplicationTermType = typ}))}))
                _ -> Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = body,
                  Core.typeApplicationTermType = typ})
          go =
                  \t ->
                    let forField =
                            \fld -> Core.Field {
                              Core.fieldName = (Core.fieldName fld),
                              Core.fieldTerm = (go (Core.fieldTerm fld))}
                        forElimination =
                                \elm -> case elm of
                                  Core.EliminationRecord v0 -> Core.EliminationRecord v0
                                  Core.EliminationUnion v0 -> Core.EliminationUnion (Core.CaseStatement {
                                    Core.caseStatementTypeName = (Core.caseStatementTypeName v0),
                                    Core.caseStatementDefault = (Maybes.map go (Core.caseStatementDefault v0)),
                                    Core.caseStatementCases = (Lists.map forField (Core.caseStatementCases v0))})
                                  Core.EliminationWrap v0 -> Core.EliminationWrap v0
                        forFunction =
                                \fun -> case fun of
                                  Core.FunctionElimination v0 -> Core.FunctionElimination (forElimination v0)
                                  Core.FunctionLambda v0 -> Core.FunctionLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.lambdaParameter v0),
                                    Core.lambdaDomain = (Core.lambdaDomain v0),
                                    Core.lambdaBody = (go (Core.lambdaBody v0))})
                                  Core.FunctionPrimitive v0 -> Core.FunctionPrimitive v0
                        forLet =
                                \lt ->
                                  let mapBinding =
                                          \b -> Core.Binding {
                                            Core.bindingName = (Core.bindingName b),
                                            Core.bindingTerm = (go (Core.bindingTerm b)),
                                            Core.bindingType = (Core.bindingType b)}
                                  in Core.Let {
                                    Core.letBindings = (Lists.map mapBinding (Core.letBindings lt)),
                                    Core.letBody = (go (Core.letBody lt))}
                        forMap =
                                \m ->
                                  let forPair = \p -> (go (Pairs.first p), (go (Pairs.second p)))
                                  in (Maps.fromList (Lists.map forPair (Maps.toList m)))
                    in case t of
                      Core.TermAnnotated v0 -> Core.TermAnnotated (Core.AnnotatedTerm {
                        Core.annotatedTermBody = (go (Core.annotatedTermBody v0)),
                        Core.annotatedTermAnnotation = (Core.annotatedTermAnnotation v0)})
                      Core.TermApplication v0 -> Core.TermApplication (Core.Application {
                        Core.applicationFunction = (go (Core.applicationFunction v0)),
                        Core.applicationArgument = (go (Core.applicationArgument v0))})
                      Core.TermEither v0 -> Core.TermEither (Eithers.either (\l -> Left (go l)) (\r -> Right (go r)) v0)
                      Core.TermFunction v0 -> Core.TermFunction (forFunction v0)
                      Core.TermLet v0 -> Core.TermLet (forLet v0)
                      Core.TermList v0 -> Core.TermList (Lists.map go v0)
                      Core.TermLiteral v0 -> Core.TermLiteral v0
                      Core.TermMap v0 -> Core.TermMap (forMap v0)
                      Core.TermMaybe v0 -> Core.TermMaybe (Maybes.map go v0)
                      Core.TermPair v0 -> Core.TermPair (go (Pairs.first v0), (go (Pairs.second v0)))
                      Core.TermRecord v0 -> Core.TermRecord (Core.Record {
                        Core.recordTypeName = (Core.recordTypeName v0),
                        Core.recordFields = (Lists.map forField (Core.recordFields v0))})
                      Core.TermSet v0 -> Core.TermSet (Sets.fromList (Lists.map go (Sets.toList v0)))
                      Core.TermTypeApplication v0 ->
                        let body1 = go (Core.typeApplicationTermBody v0)
                        in (push body1 (Core.typeApplicationTermType v0))
                      Core.TermTypeLambda v0 -> Core.TermTypeLambda (Core.TypeLambda {
                        Core.typeLambdaParameter = (Core.typeLambdaParameter v0),
                        Core.typeLambdaBody = (go (Core.typeLambdaBody v0))})
                      Core.TermUnion v0 -> Core.TermUnion (Core.Injection {
                        Core.injectionTypeName = (Core.injectionTypeName v0),
                        Core.injectionField = (forField (Core.injectionField v0))})
                      Core.TermUnit -> Core.TermUnit
                      Core.TermVariable v0 -> Core.TermVariable v0
                      Core.TermWrap v0 -> Core.TermWrap (Core.WrappedTerm {
                        Core.wrappedTermTypeName = (Core.wrappedTermTypeName v0),
                        Core.wrappedTermBody = (go (Core.wrappedTermBody v0))})
      in (go term)

-- | Given a schema graph along with language constraints and a designated list of element names, adapt the graph to the language constraints, then return a corresponding type definition for each element name.
schemaGraphToDefinitions :: Coders.LanguageConstraints -> Graph.Graph -> [[Core.Name]] -> Context.Context -> Either String (M.Map Core.Name Core.Type, [[Module.TypeDefinition]])
schemaGraphToDefinitions constraints graph nameLists cx =

      let litmap = adaptLiteralTypesMap constraints
      in (Eithers.bind (Eithers.bimap (\ic -> Errors.unDecodingError (Context.inContextObject ic)) (\x -> x) (Schemas.graphAsTypes cx graph (Lexical.graphToBindings graph))) (\tmap0 -> Eithers.bind (adaptGraphSchema constraints litmap tmap0) (\tmap1 ->
        let toDef =
                \pair -> Module.TypeDefinition {
                  Module.typeDefinitionName = (Pairs.first pair),
                  Module.typeDefinitionType = (Pairs.second pair)}
        in (Right (tmap1, (Lists.map (\names -> Lists.map toDef (Lists.map (\n -> (n, (Maybes.fromJust (Maps.lookup n tmap1)))) names)) nameLists))))))

-- | Given a target language and a source type, produce an adapter which rewrites the type and its terms according to the language's constraints. The encode direction adapts terms; the decode direction is identity.
simpleLanguageAdapter :: Coders.Language -> t0 -> Graph.Graph -> Core.Type -> Either String (Util.Adapter Core.Type Core.Type Core.Term Core.Term)
simpleLanguageAdapter lang cx g typ =

      let constraints = Coders.languageConstraints lang
          litmap = adaptLiteralTypesMap constraints
      in (Eithers.bind (adaptType constraints litmap typ) (\adaptedType -> Right (Util.Adapter {
        Util.adapterIsLossy = False,
        Util.adapterSource = typ,
        Util.adapterTarget = adaptedType,
        Util.adapterCoder = Util.Coder {
          Util.coderEncode = (\cx2 -> \term -> Eithers.bimap (\_s -> Context.InContext {
            Context.inContextObject = (Errors.ErrorOther (Errors.OtherError _s)),
            Context.inContextContext = cx2}) (\_x -> _x) (adaptTerm constraints litmap cx2 g term)),
          Util.coderDecode = (\cx2 -> \term -> Right term)}})))

-- | Find a list of alternatives for a given term, if any
termAlternatives :: Context.Context -> Graph.Graph -> Core.Term -> Either String [Core.Term]
termAlternatives cx graph term =
    case term of
      Core.TermAnnotated v0 ->
        let term2 = Core.annotatedTermBody v0
        in (Right [
          term2])
      Core.TermMaybe v0 -> Right [
        Core.TermList (Maybes.maybe [] (\term2 -> [
          term2]) v0)]
      Core.TermTypeLambda v0 ->
        let term2 = Core.typeLambdaBody v0
        in (Right [
          term2])
      Core.TermTypeApplication v0 ->
        let term2 = Core.typeApplicationTermBody v0
        in (Right [
          term2])
      Core.TermUnion v0 ->
        let tname = Core.injectionTypeName v0
            field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            forFieldType =
                    \ft ->
                      let ftname = Core.fieldTypeName ft
                      in Core.Field {
                        Core.fieldName = fname,
                        Core.fieldTerm = (Core.TermMaybe (Logic.ifElse (Equality.equal ftname fname) (Just fterm) Nothing))}
        in (Eithers.bind (Eithers.bimap (\ic -> Errors_.error (Context.inContextObject ic)) (\x -> x) (Schemas.requireUnionType cx graph tname)) (\rt -> Right [
          Core.TermRecord (Core.Record {
            Core.recordTypeName = tname,
            Core.recordFields = (Lists.map forFieldType rt)})]))
      Core.TermUnit -> Right [
        Core.TermLiteral (Core.LiteralBoolean True)]
      Core.TermWrap v0 ->
        let term2 = Core.wrappedTermBody v0
        in (Right [
          term2])
      _ -> Right []

-- | Find a list of alternatives for a given type, if any
typeAlternatives :: Core.Type -> [Core.Type]
typeAlternatives type_ =
    case type_ of
      Core.TypeAnnotated v0 ->
        let type2 = Core.annotatedTypeBody v0
        in [
          type2]
      Core.TypeMaybe v0 -> [
        Core.TypeList v0]
      Core.TypeUnion v0 ->
        let toOptField =
                \f -> Core.FieldType {
                  Core.fieldTypeName = (Core.fieldTypeName f),
                  Core.fieldTypeType = (Core.TypeMaybe (Core.fieldTypeType f))}
            optFields = Lists.map toOptField v0
        in [
          Core.TypeRecord optFields]
      Core.TypeUnit -> [
        Core.TypeLiteral Core.LiteralTypeBoolean]
      Core.TypeVoid -> [
        Core.TypeUnit]
      _ -> []
