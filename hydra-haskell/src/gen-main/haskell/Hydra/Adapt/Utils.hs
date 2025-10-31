-- | Additional adapter utilities, above and beyond the generated ones.

module Hydra.Adapt.Utils where

import qualified Hydra.Coders as Coders
import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Optionals as Optionals
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Mantle as Mantle
import qualified Hydra.Module as Module
import qualified Hydra.Names as Names
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Show.Core as Core_
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

bidirectional :: ((Coders.CoderDirection -> t0 -> Compute.Flow t1 t0) -> Compute.Coder t1 t1 t0 t0)
bidirectional f = Compute.Coder {
  Compute.coderEncode = (f Coders.CoderDirectionEncode),
  Compute.coderDecode = (f Coders.CoderDirectionDecode)}

chooseAdapter :: ((t0 -> Compute.Flow t1 [Compute.Adapter t2 t3 t0 t0 t4 t4]) -> (t0 -> Bool) -> (t0 -> String) -> (t0 -> String) -> t0 -> Compute.Flow t1 (Compute.Adapter t2 t3 t0 t0 t4 t4))
chooseAdapter alts supported show describe typ = (Logic.ifElse (supported typ) (Flows.pure (Compute.Adapter {
  Compute.adapterIsLossy = False,
  Compute.adapterSource = typ,
  Compute.adapterTarget = typ,
  Compute.adapterCoder = idCoder})) (Flows.bind (alts typ) (\raw ->  
  let candidates = (Lists.filter (\adapter -> supported (Compute.adapterTarget adapter)) raw)
  in (Logic.ifElse (Lists.null candidates) (Flows.fail (Strings.cat [
    "no adapters found for ",
    describe typ,
    Logic.ifElse (Lists.null raw) "" (Strings.cat [
      " (discarded ",
      Literals.showInt32 (Lists.length raw),
      " unsupported candidate types: ",
      Core_.list show (Lists.map Compute.adapterTarget raw),
      ")"]),
    ". Original type: ",
    (show typ)])) (Flows.pure (Lists.head candidates))))))

composeCoders :: (Compute.Coder t0 t1 t2 t3 -> Compute.Coder t0 t1 t3 t4 -> Compute.Coder t0 t1 t2 t4)
composeCoders c1 c2 = Compute.Coder {
  Compute.coderEncode = (\a -> Flows.bind (Compute.coderEncode c1 a) (\b1 -> Compute.coderEncode c2 b1)),
  Compute.coderDecode = (\c -> Flows.bind (Compute.coderDecode c2 c) (\b2 -> Compute.coderDecode c1 b2))}

encodeDecode :: (Coders.CoderDirection -> Compute.Coder t0 t0 t1 t1 -> t1 -> Compute.Flow t0 t1)
encodeDecode dir coder = ((\x -> case x of
  Coders.CoderDirectionEncode -> (Compute.coderEncode coder)
  Coders.CoderDirectionDecode -> (Compute.coderDecode coder)) dir)

-- | Check if float type is supported by language constraints
floatTypeIsSupported :: (Coders.LanguageConstraints -> Core.FloatType -> Bool)
floatTypeIsSupported constraints ft = (Sets.member ft (Coders.languageConstraintsFloatTypes constraints))

idAdapter :: (t0 -> Compute.Adapter t1 t2 t0 t0 t3 t3)
idAdapter t = Compute.Adapter {
  Compute.adapterIsLossy = False,
  Compute.adapterSource = t,
  Compute.adapterTarget = t,
  Compute.adapterCoder = idCoder}

idCoder :: (Compute.Coder t0 t1 t2 t2)
idCoder = Compute.Coder {
  Compute.coderEncode = Flows.pure,
  Compute.coderDecode = Flows.pure}

-- | Check if integer type is supported by language constraints
integerTypeIsSupported :: (Coders.LanguageConstraints -> Core.IntegerType -> Bool)
integerTypeIsSupported constraints it = (Sets.member it (Coders.languageConstraintsIntegerTypes constraints))

-- | Check if literal type is supported by language constraints
literalTypeIsSupported :: (Coders.LanguageConstraints -> Core.LiteralType -> Bool)
literalTypeIsSupported constraints lt =  
  let isSupported = (\lt -> (\x -> case x of
          Core.LiteralTypeFloat v1 -> (floatTypeIsSupported constraints v1)
          Core.LiteralTypeInteger v1 -> (integerTypeIsSupported constraints v1)
          _ -> True) lt)
  in (Logic.and (Sets.member (Variants.literalTypeVariant lt) (Coders.languageConstraintsLiteralVariants constraints)) (isSupported lt))

-- | Convert a name to file path, given case conventions for namespaces and local names, and assuming '/' as the file path separator
nameToFilePath :: (Mantle.CaseConvention -> Mantle.CaseConvention -> Module.FileExtension -> Core.Name -> String)
nameToFilePath nsConv localConv ext name =  
  let qualName = (Names.qualifyName name)
  in  
    let ns = (Module.qualifiedNameNamespace qualName)
    in  
      let local = (Module.qualifiedNameLocal qualName)
      in  
        let nsToFilePath = (\ns -> Strings.intercalate "/" (Lists.map (\part -> Formatting.convertCase Mantle.CaseConventionCamel nsConv part) (Strings.splitOn "." (Module.unNamespace ns))))
        in  
          let prefix = (Optionals.maybe "" (\n -> Strings.cat2 (nsToFilePath n) "/") ns)
          in  
            let suffix = (Formatting.convertCase Mantle.CaseConventionPascal localConv local)
            in (Strings.cat [
              prefix,
              suffix,
              ".",
              (Module.unFileExtension ext)])

-- | Check if type is supported by language constraints
typeIsSupported :: (Coders.LanguageConstraints -> Core.Type -> Bool)
typeIsSupported constraints t =  
  let base = (Rewriting.deannotateType t)
  in  
    let isVariable = (\v -> (\x -> case x of
            Mantle.TypeVariantVariable -> True
            _ -> False) v)
    in  
      let isSupportedVariant = (\v -> Logic.or (isVariable v) (Sets.member v (Coders.languageConstraintsTypeVariants constraints)))
      in  
        let isSupported = (\base -> (\x -> case x of
                Core.TypeAnnotated v1 -> (typeIsSupported constraints (Core.annotatedTypeBody v1))
                Core.TypeApplication v1 -> (Logic.and (typeIsSupported constraints (Core.applicationTypeFunction v1)) (typeIsSupported constraints (Core.applicationTypeArgument v1)))
                Core.TypeForall v1 -> (typeIsSupported constraints (Core.forallTypeBody v1))
                Core.TypeFunction v1 -> (Logic.and (typeIsSupported constraints (Core.functionTypeDomain v1)) (typeIsSupported constraints (Core.functionTypeCodomain v1)))
                Core.TypeList v1 -> (typeIsSupported constraints v1)
                Core.TypeLiteral v1 -> (literalTypeIsSupported constraints v1)
                Core.TypeMap v1 -> (Logic.and (typeIsSupported constraints (Core.mapTypeKeys v1)) (typeIsSupported constraints (Core.mapTypeValues v1)))
                Core.TypeOptional v1 -> (typeIsSupported constraints v1)
                Core.TypeProduct v1 -> (Lists.foldl Logic.and True (Lists.map (typeIsSupported constraints) v1))
                Core.TypeRecord v1 -> (Lists.foldl Logic.and True (Lists.map (\field -> typeIsSupported constraints (Core.fieldTypeType field)) (Core.rowTypeFields v1)))
                Core.TypeSet v1 -> (typeIsSupported constraints v1)
                Core.TypeSum v1 -> (Lists.foldl Logic.and True (Lists.map (typeIsSupported constraints) v1))
                Core.TypeUnion v1 -> (Lists.foldl Logic.and True (Lists.map (\field -> typeIsSupported constraints (Core.fieldTypeType field)) (Core.rowTypeFields v1)))
                Core.TypeUnit -> True
                Core.TypeWrap v1 -> (typeIsSupported constraints (Core.wrappedTypeBody v1))
                Core.TypeVariable _ -> True) base)
        in (Logic.and (Coders.languageConstraintsTypes constraints base) (Logic.and (isSupportedVariant (Variants.typeVariant base)) (isSupported base)))

unidirectionalCoder :: ((t0 -> Compute.Flow t1 t2) -> Compute.Coder t1 t3 t0 t2)
unidirectionalCoder m = Compute.Coder {
  Compute.coderEncode = m,
  Compute.coderDecode = (\_ -> Flows.fail "inbound mapping is unsupported")}
