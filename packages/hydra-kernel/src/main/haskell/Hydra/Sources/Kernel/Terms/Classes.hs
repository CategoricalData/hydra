
module Hydra.Sources.Kernel.Terms.Classes where

-- Standard imports for kernel terms modules
import Hydra.Kernel
import qualified Hydra.Core.Overlay.Haskell.Bootstrap         as Bootstrap
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Core         as Core
import qualified Hydra.Core.Dsl.Lib.Equality as Equality
import qualified Hydra.Core.Dsl.Lib.Lists    as Lists
import qualified Hydra.Core.Dsl.Lib.Logic    as Logic
import qualified Hydra.Core.Dsl.Lib.Maps     as Maps
import qualified Hydra.Core.Dsl.Lib.Optionals   as Optionals
import qualified Hydra.Core.Dsl.Lib.Pairs    as Pairs
import qualified Hydra.Core.Dsl.Lib.Sets     as Sets
import qualified Hydra.Core.Dsl.Lib.Strings  as Strings
import qualified Hydra.Sources.Kernel.Terms.Strip        as Strip
import qualified Hydra.Core.Dsl.Typing            as Typing
import           Hydra.Core.Overlay.Haskell.Dsl.Phantoms     as Phantoms
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S


-- | The hydra.core.classes registry: term-level bindings, each a TypeClass value
-- describing one of Hydra's built-in type classes.
--
-- The binding's local name (e.g. "equality") is the marker used in
-- TypeVariableConstraints.classes :: Set Name. The binding's body provides a
-- human-readable description for tooling and documentation.
--
-- Adding a new built-in type class is just adding a binding here and updating
-- callers that need to construct or recognize the marker name.
ns :: ModuleName
ns = ModuleName "hydra.core.classes"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> ([Strip.ns] L.++ kernelTypesModuleNames),
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Registry of Hydra's built-in type classes.")}
  where
    definitions = [
      toDefinition classIsSatisfiedByType,
      toDefinition equality,
      toDefinition fractional,
      toDefinition integral,
      toDefinition isComparableType,
      toDefinition numeric,
      toDefinition ordering]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

-- | Check whether a resolved, concrete type is an instance of a constraint class, identified
-- by the class's short name as it actually appears in TypeClassConstraintSimple values built
-- throughout the kernel (e.g. Name "fractional" in Kernel/Lib/Math.hs's polyConstrained calls,
-- Name "ordering" in this module's own inferTypeOfMap/collection rules) -- NOT the
-- "hydra.core.classes.*"-qualified registry module names from this module's own namespace, which
-- are never used as constraint markers. This is the entailment predicate used to discharge
-- class constraints at the end of inference (see Inference.hs); it is the one piece of "does
-- type T satisfy class C" logic in the kernel, kept alongside the class registry it checks
-- against. equality and ordering are satisfied exactly by the comparable types (see
-- isComparableType, which resolves nominal types through the given schema; #848); numeric/
-- integral/fractional are closed, finite instance sets over LiteralType's integer/float
-- variants, per docs/specification/classes.md. An unrecognized class name is treated as NOT
-- satisfied (fail-closed): the class registry is a closed, curated set (classes.md), so an
-- unrecognized name indicates a bug (a new class added without a matching arm here) rather
-- than a legitimate open extension point; failing closed surfaces that bug instead of quietly
-- admitting every type to an unknown class.
classIsSatisfiedByType :: TypedTermDefinition (M.Map Name TypeScheme -> Name -> Type -> Bool)
classIsSatisfiedByType = define "classIsSatisfiedByType" $
  doc "Check whether a type is an instance of a constraint class, by the class's short name, resolving nominal types through the given schema." $
  "schema" ~> "className" ~> "typ" ~>
  "isIntegerLiteral" <~ match _Type (Strip.deannotateType @@ var "typ") (Just false) [
    _Type_literal>>: "lt" ~> match _LiteralType (var "lt") (Just false) [
      _LiteralType_integer>>: constant true]] $
  "isFloatLiteral" <~ match _Type (Strip.deannotateType @@ var "typ") (Just false) [
    _Type_literal>>: "lt" ~> match _LiteralType (var "lt") (Just false) [
      _LiteralType_float>>: constant true]] $
  Logic.ifElse (Equality.equal (var "className") (Core.name (string "equality")))
    (isComparableType @@ var "schema" @@ var "typ") $
  Logic.ifElse (Equality.equal (var "className") (Core.name (string "ordering")))
    (isComparableType @@ var "schema" @@ var "typ") $
  Logic.ifElse (Equality.equal (var "className") (Core.name (string "numeric")))
    (Logic.or (var "isIntegerLiteral") (var "isFloatLiteral")) $
  Logic.ifElse (Equality.equal (var "className") (Core.name (string "integral")))
    (var "isIntegerLiteral") $
  Logic.ifElse (Equality.equal (var "className") (Core.name (string "fractional")))
    (var "isFloatLiteral")
    false

equality :: TypedTermDefinition TypeClass
equality = define "equality" $
  doc "The equality type class: instances support structural equality." $
  Typing.typeClass (string "Equality: instances support structural equality.")

fractional :: TypedTermDefinition TypeClass
fractional = define "fractional" $
  doc "The fractional type class: instances support total floating-point division." $
  Typing.typeClass (string "Fractional: instances support total floating-point division.")

integral :: TypedTermDefinition TypeClass
integral = define "integral" $
  doc "The integral type class: instances support integer division, modulus, remainder, and parity." $
  Typing.typeClass (string "Integral: instances support integer division, modulus, remainder, and parity.")

-- | Decide whether a type is comparable, i.e. whether its values admit Hydra's structural equality
-- and total order (docs/specification/ordering-and-equality.md, classes.md). This is the instance
-- test for both the equality and ordering classes (#848).
--
-- Not comparable: function and effect types, and any type with a non-comparable subexpression
-- (after name resolution). Comparable: literals, unit, void (vacuously), and every built-in
-- constructor, record, union, wrapper, and forall whose components are comparable.
--
-- Nominal references (qualified type variables) are resolved through the given schema. A
-- polymorphic definition is instantiated by binding each of its parameters to the comparability of
-- the corresponding argument, so a phantom parameter applied to a function type (e.g.
-- TypedTerm<a -> b>) does not make the type incomparable. Type-definition parameters carry no
-- constraints, so an unapplied parameter is assumed comparable: the obligation is checked at each
-- application.
-- Recursive references are handled coinductively: a definition already under examination (for the
-- same argument comparabilities) is assumed comparable, which is sound because a cycle alone
-- introduces no function or effect.
--
-- A genuine (unqualified) type variable that is not a definition parameter is assumed comparable:
-- in inference, a type with free variables never reaches the entailment check, because
-- substInClassConstraints re-keys the class constraint onto those variables, and the constraint
-- carries the obligation. A qualified name that does not resolve is likewise assumed comparable;
-- undefined type names are reported by other checks.
isComparableType :: TypedTermDefinition (M.Map Name TypeScheme -> Type -> Bool)
isComparableType = define "isComparableType" $
  doc "Check whether a type is comparable (an instance of equality and ordering), resolving nominal types through the given schema." $
  "schema" ~> "typ" ~>
  -- go visited env args t: the comparability of t applied to arguments whose comparabilities are args
  -- (outermost first), with definition parameters bound in env, threading the set of definitions
  -- already examined (or under examination).
  "go" <~ ("visited" ~> "env" ~> "args" ~> "t" ~>
    "goAll" <~ ("visited2" ~> "ts" ~> Lists.foldl
      ("acc" ~> "t2" ~> Logic.ifElse (Pairs.first $ var "acc")
        (var "go" @@ (Pairs.second $ var "acc") @@ var "env" @@ (list ([] :: [TypedTerm Bool])) @@ var "t2")
        (var "acc"))
      (pair true (var "visited2"))
      (var "ts")) $
    "allArgs" <~ Lists.foldl ("b1" ~> "b2" ~> Logic.and (var "b1") (var "b2")) true (var "args") $
    match _Type (Strip.deannotateType @@ var "t")
      (Just $ pair true (var "visited")) [
      _Type_application>>: "at" ~>
        "argResult" <~ var "go" @@ var "visited" @@ var "env" @@ (list ([] :: [TypedTerm Bool])) @@ (Core.applicationTypeArgument $ var "at") $
        var "go" @@ (Pairs.second $ var "argResult") @@ var "env"
          @@ (Lists.cons (Pairs.first $ var "argResult") (var "args"))
          @@ (Core.applicationTypeFunction $ var "at"),
      _Type_effect>>: constant $ pair false (var "visited"),
      _Type_either>>: "et" ~> var "goAll" @@ var "visited" @@ list [Core.eitherTypeLeft $ var "et", Core.eitherTypeRight $ var "et"],
      -- A forall applied to an argument binds its parameter to the argument's comparability; an
      -- unapplied forall is comparable iff its body is, its parameter being judged like any other
      -- type variable (by its constraints, which inference tracks; see above).
      _Type_forall>>: "ft" ~> Optionals.match (Lists.uncons $ var "args")
        (var "go" @@ var "visited"
          @@ (Maps.insert (Core.forallTypeParameter $ var "ft") true (var "env"))
          @@ (list ([] :: [TypedTerm Bool]))
          @@ (Core.forallTypeBody $ var "ft"))
        ("argsUc" ~> var "go" @@ var "visited"
          @@ (Maps.insert (Core.forallTypeParameter $ var "ft") (Pairs.first $ var "argsUc") (var "env"))
          @@ (Pairs.second $ var "argsUc")
          @@ (Core.forallTypeBody $ var "ft")),
      _Type_function>>: constant $ pair false (var "visited"),
      _Type_list>>: "lt" ~> var "goAll" @@ var "visited" @@ list [var "lt"],
      _Type_map>>: "mt" ~> var "goAll" @@ var "visited" @@ list [Core.mapTypeKeys $ var "mt", Core.mapTypeValues $ var "mt"],
      _Type_optional>>: "ot" ~> var "goAll" @@ var "visited" @@ list [var "ot"],
      _Type_pair>>: "pt" ~> var "goAll" @@ var "visited" @@ list [Core.pairTypeFirst $ var "pt", Core.pairTypeSecond $ var "pt"],
      _Type_record>>: "fields" ~> var "goAll" @@ var "visited" @@ Lists.map (reify Core.fieldTypeType) (var "fields"),
      _Type_set>>: "st" ~> var "goAll" @@ var "visited" @@ list [var "st"],
      _Type_union>>: "fields" ~> var "goAll" @@ var "visited" @@ Lists.map (reify Core.fieldTypeType) (var "fields"),
      _Type_variable>>: "v" ~> Optionals.match (Maps.lookup (var "v") (var "env"))
        (Optionals.match (Maps.lookup (var "v") (var "schema"))
          -- A genuine type variable, or an unresolved name: assumed comparable (see above)
          (pair (var "allArgs") (var "visited"))
          ("ts" ~>
            "params" <~ Core.typeSchemeVariables (var "ts") $
            -- The key distinguishes instantiations of the same polymorphic definition
            "key" <~ Core.name (Strings.concat $ Lists.cons (Core.unName $ var "v") (Lists.map
              ("b" ~> Logic.ifElse (var "b") (string "+") (string "-")) (var "args"))) $
            Logic.ifElse (Sets.member (var "key") (var "visited"))
              (pair true (var "visited"))
              (var "go"
                @@ (Sets.insert (var "key") (var "visited"))
                @@ (Maps.fromList $ Lists.zip (var "params")
                     (Lists.concat2 (var "args") (Lists.replicate (Lists.length $ var "params") true)))
                @@ (Lists.drop (Lists.length $ var "params") (var "args"))
                @@ (Core.typeSchemeBody $ var "ts"))))
        ("b" ~> pair (Logic.and (var "b") (var "allArgs")) (var "visited")),
      _Type_wrap>>: "wt" ~> var "goAll" @@ var "visited" @@ list [var "wt"]]) $
  Pairs.first $ var "go" @@ (Sets.empty :: TypedTerm (S.Set Name)) @@ (Maps.empty :: TypedTerm (M.Map Name Bool))
    @@ (list ([] :: [TypedTerm Bool])) @@ var "typ"

numeric :: TypedTermDefinition TypeClass
numeric = define "numeric" $
  doc "The numeric type class: instances support arithmetic (addition, subtraction, multiplication, negation)." $
  Typing.typeClass (string "Numeric: instances support arithmetic (addition, subtraction, multiplication, negation).")

ordering :: TypedTermDefinition TypeClass
ordering = define "ordering" $
  doc "The ordering type class: instances support total ordering (and equality)." $
  Typing.typeClass (string "Ordering: instances support total ordering (and equality).")
