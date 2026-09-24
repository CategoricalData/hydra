module Hydra.Sources.Kernel.Terms.Literals where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (
  bigintToIntegerValue, integerValueToBigint)
import qualified Hydra.Core.Dsl.Paths    as Paths
import qualified Hydra.Core.Overlay.Haskell.Dsl.Annotations       as Annotations
import qualified Hydra.Core.Dsl.Ast          as Ast
import qualified Hydra.Core.Overlay.Haskell.Bootstrap         as Bootstrap
import qualified Hydra.Core.Dsl.Coders       as Coders
import qualified Hydra.Core.Dsl.Util      as Util
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Core         as Core
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Graph        as Graph
import qualified Hydra.Core.Dsl.Json.Model         as Json
import qualified Hydra.Core.Dsl.Lib.Chars    as Chars
import qualified Hydra.Core.Dsl.Lib.Eithers  as Eithers
import qualified Hydra.Core.Dsl.Lib.Equality as Equality
import qualified Hydra.Core.Dsl.Lib.Lists    as Lists
import qualified Hydra.Core.Dsl.Lib.Literals as Literals
import qualified Hydra.Core.Dsl.Lib.Logic    as Logic
import qualified Hydra.Core.Dsl.Lib.Maps     as Maps
import qualified Hydra.Core.Dsl.Lib.Math     as Math
import qualified Hydra.Core.Dsl.Lib.Optionals   as Optionals
import qualified Hydra.Core.Dsl.Lib.Pairs    as Pairs
import qualified Hydra.Core.Dsl.Lib.Sets     as Sets
import qualified Hydra.Core.Dsl.Lib.Strings  as Strings
import qualified Hydra.Core.Overlay.Haskell.Dsl.Literals          as Literals
import qualified Hydra.Core.Overlay.Haskell.Dsl.LiteralTypes      as LiteralTypes
import qualified Hydra.Core.Overlay.Haskell.Dsl.Base         as MetaBase
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Terms        as MetaTerms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Types        as MetaTypes
import qualified Hydra.Core.Dsl.Packaging       as Packaging
import qualified Hydra.Core.Dsl.Parsing      as Parsing
import           Hydra.Core.Overlay.Haskell.Dsl.Phantoms     as Phantoms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Prims             as Prims
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Tabular           as Tabular
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Testing      as Testing
import qualified Hydra.Core.Overlay.Haskell.Dsl.Terms             as Terms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Tests             as Tests
import qualified Hydra.Core.Dsl.Topology     as Topology
import qualified Hydra.Core.Overlay.Haskell.Dsl.Types             as Types
import qualified Hydra.Core.Dsl.Typing       as Typing
import qualified Hydra.Core.Dsl.Util         as Util
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Variants     as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y


ns :: ModuleName
ns = ModuleName "hydra.core.literals"

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
  "it" ~> "bi" ~> match _IntegerType (var "it")
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
  cases _IntegerValue
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
