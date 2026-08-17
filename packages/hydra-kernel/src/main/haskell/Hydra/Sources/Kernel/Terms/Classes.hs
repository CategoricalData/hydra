
module Hydra.Sources.Kernel.Terms.Classes where

-- Standard imports for kernel terms modules
import Hydra.Kernel
import qualified Hydra.Overlay.Haskell.Bootstrap         as Bootstrap
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Core         as Core
import qualified Hydra.Dsl.Lib.Equality as Equality
import qualified Hydra.Dsl.Lib.Logic    as Logic
import qualified Hydra.Sources.Kernel.Terms.Strip        as Strip
import qualified Hydra.Dsl.Typing            as Typing
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms     as Phantoms
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.List                   as L


-- | The hydra.classes registry: term-level bindings, each a TypeClass value
-- describing one of Hydra's built-in type classes.
--
-- The binding's local name (e.g. "equality") is the marker used in
-- TypeVariableConstraints.classes :: Set Name. The binding's body provides a
-- human-readable description for tooling and documentation.
--
-- Adding a new built-in type class is just adding a binding here and updating
-- callers that need to construct or recognize the marker name.
ns :: ModuleName
ns = ModuleName "hydra.classes"

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
      toDefinition numeric,
      toDefinition ordering]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

-- | Check whether a resolved, concrete type is an instance of a constraint class, identified
-- by the class's short name as it actually appears in TypeClassConstraintSimple values built
-- throughout the kernel (e.g. Name "fractional" in Kernel/Lib/Math.hs's polyConstrained calls,
-- Name "ordering" in this module's own inferTypeOfMap/collection rules) -- NOT the
-- "hydra.classes.*"-qualified registry module names from this module's own namespace, which
-- are never used as constraint markers. This is the entailment predicate used to discharge
-- class constraints at the end of inference (see Inference.hs); it is the one piece of "does
-- type T satisfy class C" logic in the kernel, kept alongside the class registry it checks
-- against. equality and ordering are universal (every type is an instance); numeric/
-- integral/fractional are closed, finite instance sets over LiteralType's integer/float
-- variants, per docs/specification/classes.md. An unrecognized class name is treated as NOT
-- satisfied (fail-closed): the class registry is a closed, curated set (classes.md), so an
-- unrecognized name indicates a bug (a new class added without a matching arm here) rather
-- than a legitimate open extension point; failing closed surfaces that bug instead of quietly
-- admitting every type to an unknown class.
classIsSatisfiedByType :: TypedTermDefinition (Name -> Type -> Bool)
classIsSatisfiedByType = define "classIsSatisfiedByType" $
  doc "Check whether a type is an instance of a constraint class, by the class's short name." $
  "className" ~> "typ" ~>
  "isIntegerLiteral" <~ match _Type (Strip.deannotateType @@ var "typ") (Just false) [
    _Type_literal>>: "lt" ~> match _LiteralType (var "lt") (Just false) [
      _LiteralType_integer>>: constant true]] $
  "isFloatLiteral" <~ match _Type (Strip.deannotateType @@ var "typ") (Just false) [
    _Type_literal>>: "lt" ~> match _LiteralType (var "lt") (Just false) [
      _LiteralType_float>>: constant true]] $
  Logic.ifElse (Equality.equal (var "className") (Core.name (string "equality")))
    true $
  Logic.ifElse (Equality.equal (var "className") (Core.name (string "ordering")))
    true $
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

numeric :: TypedTermDefinition TypeClass
numeric = define "numeric" $
  doc "The numeric type class: instances support arithmetic (addition, subtraction, multiplication, negation)." $
  Typing.typeClass (string "Numeric: instances support arithmetic (addition, subtraction, multiplication, negation).")

ordering :: TypedTermDefinition TypeClass
ordering = define "ordering" $
  doc "The ordering type class: instances support total ordering (and equality)." $
  Typing.typeClass (string "Ordering: instances support total ordering (and equality).")
