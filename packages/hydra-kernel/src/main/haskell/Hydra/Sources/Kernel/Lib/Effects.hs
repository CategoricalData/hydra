-- | Primitive declarations for the hydra.core.lib.effects namespace.

module Hydra.Sources.Kernel.Lib.Effects where

import Hydra.Kernel
import qualified Hydra.Core.Overlay.Haskell.Bootstrap     as Bootstrap
import           Hydra.Core.Overlay.Haskell.Dsl.Phantoms as Phantoms hiding (apply, compose, map)
import qualified Hydra.Core.Overlay.Haskell.Dsl.Types         as Types
import           Hydra.Core.Overlay.Haskell.Dsl.Types         (effect)
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++), foldl, map, pure)


ns :: ModuleName
ns = ModuleName "hydra.core.lib.effects"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = DefinitionPrimitive <$> definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleMetadata = descriptionMetadataWithComments (Just "Primitives in the hydra.core.lib.effects module.") [
              "A value of effect<t> describes a deferred, executable computation which, when executed by a host\
              \ interpreter, produces a value of type t; describing a computation is separate from executing it,\
              \ and Hydra uses the effect type to keep that boundary explicit.",
              "There is no eliminator (cases) for effects: an effect value is opaque, consumable only by further\
              \ composition within this module or by an executor outside the language.",
              "Every primitive in this module is pure: constructing or composing an effect description is a\
              \ referentially transparent operation on ordinary values, and nothing observable happens until an\
              \ executor runs the description. Effect descriptions have no identity semantics -- two equal\
              \ descriptions are interchangeable -- so the usual pure-term transformations are valid over\
              \ effect-building code. (Purity is independent of implementation strategy: a host may implement\
              \ these combinators natively rather than from a default implementation.)",
              "A host interpreter of effect<t> must honor, in addition to the purity of the combinators above:\
              \ purity of construction (building or composing a description performs no host interaction; only\
              \ an executor performing the described computation may interact with the host); order (when a\
              \ combinator specifies an order between the effects it composes, an executor performs them in\
              \ exactly that order); exactly-once (each composed effect is performed exactly once per execution,\
              \ never skipped, never repeated); and deferral (an effect specified to run after another must not\
              \ be performed, nor its producing function evaluated for effect, until the prior effect has run to\
              \ completion and its value is available). These obligations are observable -- the effects are real\
              \ host interactions -- so they are normative, not advisory.",
              "effect<t> is total: a leaf effect always completes and yields a value (for hydra.lib.files, an\
              \ either<FileError, t>); the obligations above govern order, multiplicity, and deferral, not\
              \ failure short-circuiting."]}
  where
    definitions = [apply, bind, compose, foldList, map, mapList, mapOptional, mapSet, pure]

define :: String -> String -> TermSignature -> [String] -> PrimitiveDefinition
define = impurePrimitiveInModule module_

-- Shared type variables
tx, ty, tz :: Type
tx = Types.var "x"
ty = Types.var "y"
tz = Types.var "z"

apply :: PrimitiveDefinition
apply = availableSince "0.17" $ withProvisions [
  requirement "appliesEffectfulFunctionToEffectfulArgument"
    "apply interprets the function-producing effect and the argument-producing effect, then\
    \ applies the resulting function to the resulting argument."]
  $ define "apply" "Applicative apply for effects."
  (sigWithParams [("ef", "the effect producing the function to apply"),
                  ("ex", "the effect producing the argument")]
    $ TypeScheme [Name "x", Name "y"]
    (effect (tx Types.~> ty) Types.~> effect tx Types.~> effect ty) mempty)
  ["apply(ef, ex) describes an effectful computation which first interprets ef to produce a\
  \ function, then interprets ex to produce an argument, and returns the result of applying the\
  \ function to the argument."]

bind :: PrimitiveDefinition
bind = availableSince "0.17" $ withProvisions [
  requirement "threadsResultIntoNextEffect"
    "bind passes the result of the first effect to the given function to obtain the next effect,\
    \ whose result is the overall result.",
  requirement "performsEffectsInOrderExactlyOnce"
    "bind performs the first effect to completion, exactly once, before performing the second\
    \ effect, also exactly once -- observable via real host side effects (not merely by return\
    \ value), ruling out a host that reorders, double-runs, skips, or eagerly runs either effect."]
  $ define "bind" "Sequence two effectful computations."
  (sigWithParams [("e", "the effect to interpret first"),
                  ("f", "the function producing the next effect from e's result")]
    $ TypeScheme [Name "x", Name "y"]
    (effect tx Types.~> (tx Types.~> effect ty) Types.~> effect ty) mempty)
  ["bind(e, f) describes an effectful computation which first interprets e, then passes its\
  \ result to f to obtain the next effect.",
   "Host interpreters provide the operational meaning; Hydra uses the type effect<t> to keep the\
   \ effect boundary explicit."]

compose :: PrimitiveDefinition
compose = availableSince "0.17" $ withProvisions [
  requirement "sequencesTwoKleisliArrows"
    "compose(f, g, x) runs f on x to obtain the first effect, then runs g on that effect's result\
    \ to obtain the second effect, equivalent to bind(f(x), g)."]
  $ define "compose" "Kleisli composition for effects."
  (sigWithParams [("f", "the first effect-returning function"),
                  ("g", "the second effect-returning function"),
                  ("x", "the input to the composed computation")]
    $ TypeScheme [Name "x", Name "y", Name "z"]
    ((tx Types.~> effect ty) Types.~> (ty Types.~> effect tz) Types.~> tx Types.~> effect tz) mempty)
  ["compose(f, g, x) describes an effectful computation equivalent to bind(f(x), g)."]

foldList :: PrimitiveDefinition
foldList = availableSince "0.17.6" $ withComments [
  "Renamed from hydra.lib.effects.foldl."]
  $ withProvisions [
  requirement "sequencesEffectReturningStepOverList"
    "foldList sequences an effect-returning step function over the list's elements, threading the\
    \ accumulator through, and the overall result is the final accumulator.",
  requirement "performsElementEffectsLeftToRightExactlyOnce"
    "foldList performs the element effects left-to-right in list order, each exactly once --\
    \ observable via real host side effects, ruling out a host that reorders, double-runs, or\
    \ skips an element's effect."]
  $ define "foldList" "Left-fold over a list with an effect-returning function."
  (sigWithParams [("f", "the effect-returning combining function"),
                  ("acc0", "the initial accumulator value"),
                  ("xs", "the list to fold over")]
    $ TypeScheme [Name "x", Name "y"]
    ((tx Types.~> ty Types.~> effect tx) Types.~> tx Types.~> Types.list ty Types.~> effect tx) mempty)
  ["foldList(f, acc0, xs) describes an effectful left fold over xs, sequencing applications of f from\
  \ left to right."]

map :: PrimitiveDefinition
map = availableSince "0.17" $ withProvisions [
  requirement "appliesPureFunctionToEffectResult"
    "map interprets the given effect and applies the pure function to its result; equivalent to\
    \ bind(e, \\x -> pure(f x))."]
  $ define "map" "Map a pure function over the result of an effect."
  (sigWithParams [("f", "the pure function to apply to the effect's result"),
                  ("e", "the effect to interpret")]
    $ TypeScheme [Name "x", Name "y"]
    ((tx Types.~> ty) Types.~> effect tx Types.~> effect ty) mempty)
  ["map(f, e) describes an effectful computation which interprets e and applies f to its result.",
   "Equivalent to bind(e, \\x -> pure(f x))."]

mapList :: PrimitiveDefinition
mapList = availableSince "0.17" $ withProvisions [
  requirement "appliesEffectReturningFunctionAndCollectsResults"
    "mapList applies the effect-returning function to each element and collects the per-element\
    \ results into a list of the same length, in order.",
  requirement "performsElementEffectsLeftToRightExactlyOnce"
    "mapList performs the element effects left-to-right in list order, each exactly once --\
    \ observable via real host side effects, ruling out a host that reorders, double-runs, or\
    \ skips an element's effect."]
  $ define "mapList" "Map an effect-returning function over a list."
  (sigWithParams [("f", "the effect-returning function to apply to each element"),
                  ("xs", "the list to map over")]
    $ TypeScheme [Name "x", Name "y"]
    ((tx Types.~> effect ty) Types.~> Types.list tx Types.~> effect (Types.list ty)) mempty)
  ["mapList(f, xs) describes an effectful computation which applies f to each element of xs from\
  \ left to right and collects the results."]

mapOptional :: PrimitiveDefinition
mapOptional = availableSince "0.17" $ withProvisions [
  requirement "appliesFunctionWhenPresentPassesThroughNoneWhenAbsent"
    "mapOptional over a present value applies the effect-returning function and wraps the result\
    \ in given; mapOptional over none yields none, with the function not applied.",
  requirement "performsExactlyZeroOrOneEffect"
    "mapOptional over none performs zero effects; mapOptional over a present value performs\
    \ exactly one effect -- observable via a real host side effect, distinguishing \"the function\
    \ was never invoked\" from \"the function was invoked and its effect ran\"."]
  $ define "mapOptional" "Map an effect-returning function over an optional."
  (sigWithParams [("f", "the effect-returning function to apply to the contained value"),
                  ("m", "the optional to map over")]
    $ TypeScheme [Name "x", Name "y"]
    ((tx Types.~> effect ty) Types.~> Types.optional tx Types.~> effect (Types.optional ty)) mempty)
  ["mapOptional(f, m) describes an effectful computation which returns none when m is none, or\
  \ applies f to the contained value and wraps the result in given."]

mapSet :: PrimitiveDefinition
mapSet = availableSince "0.17.6" $ withProvisions [
  requirement "appliesEffectReturningFunctionAndCollectsResultsAsSet"
    "mapSet applies the effect-returning function to each element and collects the per-element\
    \ results into a set, deduplicated by the result type's ordering.",
  requirement "performsElementEffectsInAscendingOrderExactlyOnce"
    "mapSet performs the element effects in ascending element order, each exactly once --\
    \ observable via real host side effects, ruling out a host that reorders, double-runs, or\
    \ skips an element's effect."]
  $ define "mapSet" "Map an effect-returning function over a set."
  (sigWithParams [("f", "the effect-returning function to apply to each element"),
                  ("s", "the set to map over")]
    $ Types.polyConstrained [("x", [Name "ordering"]), ("y", [Name "ordering"])]
    ((tx Types.~> effect ty) Types.~> Types.set tx Types.~> effect (Types.set ty)))
  ["mapSet(f, s) describes an effectful computation which applies f to each element of s, sequencing\
  \ the effects in ascending element order (the order is normative, since the effects are observable),\
  \ and collects the results as a set, deduplicated by the result type's ordering.",
   "Requires 'ordering' constraints on both element types (the set type contract)."]

pure :: PrimitiveDefinition
pure = availableSince "0.17" $ withProvisions [
  requirement "liftsValueWithoutHostInteraction"
    "pure(x) succeeds with exactly x, without requiring any host interaction."]
  $ define "pure" "Lift a pure value into an effect."
  (sigWithParams [("x", "the pure value to lift into an effect")]
    $ TypeScheme [Name "x"] (tx Types.~> effect tx) mempty)
  ["pure(x) describes an effectful computation which succeeds with x without requiring host\
  \ interaction."]
