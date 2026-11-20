module Hydra.Sources.Kernel.Types.Constraints.Model where

-- Standard type-level kernel imports
import           Hydra.Kernel
import           Hydra.Core.Overlay.Haskell.Dsl.Annotations (doc)
import           Hydra.Core.Overlay.Haskell.Bootstrap
import           Hydra.Core.Overlay.Haskell.Dsl.Types ((>:))
import qualified Hydra.Core.Overlay.Haskell.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Hydra.Sources.Kernel.Types.Paths as Paths
import qualified Hydra.Sources.Kernel.Types.Regex as Regex


ns :: ModuleName
ns = ModuleName "hydra.core.constraints.model"

define :: String -> Type -> TypeDefinition
define = defineType ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = unqualifiedDep <$> [Core.ns, Paths.ns, Regex.ns],
            moduleMetadata = descriptionMetadata (Just $
              "A typed vocabulary of value constraints -- data-level restrictions the structural type"
              ++ " system cannot express (cardinality, length, range, pattern, membership, boolean"
              ++ " combinations, uniqueness, and an arbitrary-predicate escape hatch). Constraints are"
              ++ " author-supplied data that travel with a model, declared as freestanding, fully"
              ++ " typechecked ConstraintSet values and enforced by hydra.core.validate.model, which"
              ++ " emits hydra.core.error.model findings. The normative meaning of each TermConstraint"
              ++ " variant is its translation to a boolean predicate term (the #345 Stage-1 refinement"
              ++ " reading), shipped with a single reference implementation and conformance fixtures."
              ++ " The first (and, for 1.0, only) inhabited member of the hydra.<pkg>.constraints.model"
              ++ " family, paralleling hydra.<pkg>.validate.model and hydra.<pkg>.error.model. See"
              ++ " issue #734 (parent #712; #345 Stage 0).")}
  where
    definitions = [
      bounds,
      constraintSet,
      pathConstraint,
      termBound,
      termConstraint,
      termRange]

-- Definitions are listed alphabetically, per the coding-style rules.

bounds :: TypeDefinition
bounds = define "Bounds" $
  doc ("An inclusive count range with optional lower and upper endpoints. Shared by the cardinality and"
    ++ " length constraints, both of which restrict a count (of elements/entries, or of code points)."
    ++ " An absent endpoint imposes no bound on that side; both absent restricts nothing.") $
  T.record [
    "min">:
      doc "The minimum count, inclusive; absent means no lower bound." $
      T.optional T.int32,
    "max">:
      doc "The maximum count, inclusive; absent means no upper bound." $
      T.optional T.int32]

constraintSet :: TypeDefinition
constraintSet = define "ConstraintSet" $
  doc ("All value constraints declared for one named type: the freestanding, fully typechecked unit of"
    ++ " attachment. Unlike an annotation payload (an untyped term decoded by convention), a ConstraintSet"
    ++ " is ordinary graph data whose type is checked by the normal inference machinery. The typeName"
    ++ " resolves against the graph the same way nominal types resolve during validation; a ConstraintSet"
    ++ " naming an unknown type is a finding, not a skip. A constraint set can target only a NAMED type"
    ++ " (the same discipline SHACL and relational schemas impose). For a polymorphic target, paths are"
    ++ " relative to the body with quantifiers stripped, type variables are opaque leaves (no path"
    ++ " descends through a variable), and the constraints apply at every instantiation.") $
  T.record [
    "typeName">:
      doc "The name of the type whose instances these constraints restrict." $
      Core.name,
    "constraints">:
      doc "The path-located constraints that apply to instances of the named type." $
      T.list pathConstraint]

pathConstraint :: TypeDefinition
pathConstraint = define "PathConstraint" $
  doc ("A term constraint located at a position within a type, addressed by a subtype path. The path is"
    ++ " declared at the type level and interpreted against instances as a one-to-many map at validation"
    ++ " time: listElement/setElement fans out to every element, mapKeys/mapValues to every key/value,"
    ++ " optionalElement to the value iff present, unionField f to the injected value iff the instance is"
    ++ " variant f, and recordField f / wrapBody select the single corresponding subterm. The empty path"
    ++ " selects the instance itself. A non-data step (e.g. functionDomain, forallBody), or a step that"
    ++ " cannot apply to the instance's actual shape, is a malformed-constraint finding -- never a silent"
    ++ " skip.") $
  T.record [
    "path">:
      doc "The subtype path locating the focus position(s) the constraint restricts." $
      Paths.subtypePath,
    "constraint">:
      doc "The constraint tested against each focus value." $
      termConstraint]

termBound :: TypeDefinition
termBound = define "TermBound" $
  doc ("One endpoint of a term range: a bounding term together with whether the endpoint is inclusive."
    ++ " The bound is an arbitrary term, not only a literal, so a range over any ordered type is"
    ++ " expressible (e.g. a minimum day-of-week over a union whose variant order defines the ordering)."
    ++ " Literal ranges are the special case where the bound term is a literal. The bound term must match"
    ++ " the focus type and that type must satisfy the ordering class; a mismatch is a validation-time"
    ++ " malformed-constraint finding.") $
  T.record [
    "value">:
      doc "The bounding term." $
      Core.term,
    "inclusive">:
      doc "True if the endpoint is inclusive (<= / >=), false if exclusive (< / >)." $
      T.boolean]

termConstraint :: TypeDefinition
termConstraint = define "TermConstraint" $
  doc ("A restriction on a focus term. The normative meaning of each variant is its translation to a"
    ++ " boolean predicate term; a variant is well-formed at a focus only where that focus type satisfies"
    ++ " the variant's class gate (ordering for range; equality for equals/oneOf/uniqueness; only the"
    ++ " obvious shape for cardinality/length/pattern), checked with"
    ++ " hydra.core.classes.classIsSatisfiedByType. Nine variants are enforced by hydra.core.validate.model"
    ++ " in 1.0; uniqueness and predicate are reserved -- defined so that later enforcement does not widen"
    ++ " the union (a breaking change for exhaustive matchers), but encountering one when the rule is"
    ++ " enabled produces a \"reserved constraint not yet enforced\" finding, never a silent pass.") $
  T.union [
    "cardinality">:
      doc ("The element/entry count of a list, set, or map focus must fall within the bounds."
        ++ " Predicate reading: gte/lte over Lists.length / Sets.size / Maps.size.") $
      bounds,
    "length">:
      doc ("The code-point length of a string focus must fall within the bounds. Predicate reading:"
        ++ " gte/lte over Strings.length.") $
      bounds,
    "range">:
      doc ("The focus term must lie within the given range of the focus (ordered) type. Predicate"
        ++ " reading: hydra.core.lib.ordering.gte/lte. Requires the ordering class on the focus type.") $
      termRange,
    "pattern">:
      doc ("A string focus must match the regular expression, as a whole (anchored at both ends)."
        ++ " Predicate reading: hydra.core.lib.regex.matches over the printed AST.") $
      Regex.regex,
    "equals">:
      doc ("The focus term must equal the given term. Predicate reading: equality over the focus (equality)"
        ++ " type. (sh:hasValue.)") $
      Core.term,
    "oneOf">:
      doc ("The focus term must equal one of the given terms. Predicate reading: membership by equality."
        ++ " Requires the equality class on the focus type. (sh:in.)") $
      T.list Core.term,
    "allOf">:
      doc ("Every sub-constraint must hold at the focus. Predicate reading: conjunction. (sh:and.)") $
      T.list termConstraint,
    "anyOf">:
      doc ("At least one sub-constraint must hold at the focus. Predicate reading: disjunction. (sh:or.)") $
      T.list termConstraint,
    "not">:
      doc ("The sub-constraint must NOT hold at the focus. Predicate reading: negation. (sh:not.)") $
      termConstraint,
    "uniqueness">:
      doc ("RESERVED (not yet enforced in 1.0). The values selected by the given key path must be pairwise"
        ++ " distinct. Predicate reading: pairwise-distinctness by equality. Requires the equality class.") $
      Paths.subtypePath,
    "predicate">:
      doc ("RESERVED (not yet enforced in 1.0). An arbitrary unary predicate: a term of type t -> boolean,"
        ++ " applied to the focus and evaluated. The escape hatch; it is already a Stage-1 predicate.") $
      Core.term]

termRange :: TypeDefinition
termRange = define "TermRange" $
  doc ("A range over an ordered type, with optional lower and upper term bounds, each independently"
    ++ " inclusive or exclusive. An absent endpoint imposes no bound on that side.") $
  T.record [
    "min">:
      doc "The lower bound; absent means unbounded below." $
      T.optional termBound,
    "max">:
      doc "The upper bound; absent means unbounded above." $
      T.optional termBound]
