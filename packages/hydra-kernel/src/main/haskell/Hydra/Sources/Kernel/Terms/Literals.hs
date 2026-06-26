module Hydra.Sources.Kernel.Terms.Literals where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (
  bigintToIntegerValue, integerValueToBigint)
import qualified Hydra.Dsl.Paths    as Paths
import qualified Hydra.Overlay.Haskell.Dsl.Annotations       as Annotations
import qualified Hydra.Dsl.Ast          as Ast
import qualified Hydra.Overlay.Haskell.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Coders       as Coders
import qualified Hydra.Dsl.Util      as Util
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Core         as Core
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Graph        as Graph
import qualified Hydra.Dsl.Json.Model         as Json
import qualified Hydra.Dsl.Lib.Chars    as Chars
import qualified Hydra.Dsl.Lib.Eithers  as Eithers
import qualified Hydra.Dsl.Lib.Equality as Equality
import qualified Hydra.Dsl.Lib.Lists    as Lists
import qualified Hydra.Dsl.Lib.Literals as Literals
import qualified Hydra.Dsl.Lib.Logic    as Logic
import qualified Hydra.Dsl.Lib.Maps     as Maps
import qualified Hydra.Dsl.Lib.Math     as Math
import qualified Hydra.Dsl.Lib.Optionals   as Optionals
import qualified Hydra.Dsl.Lib.Pairs    as Pairs
import qualified Hydra.Dsl.Lib.Sets     as Sets
import qualified Hydra.Dsl.Lib.Strings  as Strings
import qualified Hydra.Overlay.Haskell.Dsl.Literals          as Literals
import qualified Hydra.Overlay.Haskell.Dsl.LiteralTypes      as LiteralTypes
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Base         as MetaBase
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Terms        as MetaTerms
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Types        as MetaTypes
import qualified Hydra.Dsl.Packaging       as Packaging
import qualified Hydra.Dsl.Parsing      as Parsing
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms     as Phantoms
import qualified Hydra.Overlay.Haskell.Dsl.Prims             as Prims
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Tabular           as Tabular
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Testing      as Testing
import qualified Hydra.Overlay.Haskell.Dsl.Terms             as Terms
import qualified Hydra.Overlay.Haskell.Dsl.Tests             as Tests
import qualified Hydra.Dsl.Topology     as Topology
import qualified Hydra.Overlay.Haskell.Dsl.Types             as Types
import qualified Hydra.Dsl.Typing       as Typing
import qualified Hydra.Dsl.Util         as Util
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Variants     as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y


ns :: ModuleName
ns = ModuleName "hydra.literals"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> (kernelTypesModuleNames),
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Conversion functions for literal values.")}
  where
   definitions = [
     toDefinition bigintToIntegerValue,
     toDefinition integerValueToBigint]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

bigintToIntegerValue :: TypedTermDefinition (IntegerType -> Integer -> IntegerValue)
bigintToIntegerValue = define "bigintToIntegerValue" $
  doc "Convert a bigint to an integer value of a given type (note: lossy)" $
  "it" ~> "bi" ~> cases _IntegerType (var "it")
    Nothing [
    _IntegerType_bigint>>: constant $ Core.integerValueBigint $ var "bi",
    _IntegerType_int8>>: constant $ Core.integerValueInt8 $ Literals.bigintToInt8 $ var "bi",
    _IntegerType_int16>>: constant $ Core.integerValueInt16 $ Literals.bigintToInt16 $ var "bi",
    _IntegerType_int32>>: constant $ Core.integerValueInt32 $ Literals.bigintToInt32 $ var "bi",
    _IntegerType_int64>>: constant $ Core.integerValueInt64 $ Literals.bigintToInt64 $ var "bi",
    _IntegerType_uint8>>: constant $ Core.integerValueUint8 $ Literals.bigintToUint8 $ var "bi",
    _IntegerType_uint16>>: constant $ Core.integerValueUint16 $ Literals.bigintToUint16 $ var "bi",
    _IntegerType_uint32>>: constant $ Core.integerValueUint32 $ Literals.bigintToUint32 $ var "bi",
    _IntegerType_uint64>>: constant $ Core.integerValueUint64 $ Literals.bigintToUint64 $ var "bi"]

integerValueToBigint :: TypedTermDefinition (IntegerValue -> Integer)
integerValueToBigint = define "integerValueToBigint" $
  doc "Convert an integer value of any precision to a bigint" $
  match _IntegerValue
    Nothing [
    _IntegerValue_bigint>>: "bi" ~> var "bi",
    _IntegerValue_int8>>: "i8" ~> Literals.int8ToBigint $ var "i8",
    _IntegerValue_int16>>: "i16" ~> Literals.int16ToBigint $ var "i16",
    _IntegerValue_int32>>: "i32" ~> Literals.int32ToBigint $ var "i32",
    _IntegerValue_int64>>: "i64" ~> Literals.int64ToBigint $ var "i64",
    _IntegerValue_uint8>>: "ui8" ~> Literals.uint8ToBigint $ var "ui8",
    _IntegerValue_uint16>>: "ui16" ~> Literals.uint16ToBigint $ var "ui16",
    _IntegerValue_uint32>>: "ui32" ~> Literals.uint32ToBigint $ var "ui32",
    _IntegerValue_uint64>>: "ui64" ~> Literals.uint64ToBigint $ var "ui64"]
