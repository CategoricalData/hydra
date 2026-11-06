{-
stack ghci hydra:lib hydra:hydra-test

Test.Hspec.hspec Hydra.ExtSchemaModelsSpec.spec
-}

module Hydra.ExtSchemaModelsSpec where

import Hydra.Kernel
import qualified Hydra.Sources.Csharp.Syntax as CsharpSyntax
import qualified Hydra.Sources.Delta.Parquet as DeltaParquet
import qualified Hydra.Sources.Kusto.Kql as KustoKql
import qualified Hydra.Sources.Other.Atlas as Atlas
import qualified Hydra.Sources.Other.AzureDtld as AzureDtld
import qualified Hydra.Sources.Other.Datalog as Datalog
import qualified Hydra.Sources.Other.GeoJson as GeoJson
import qualified Hydra.Sources.Other.IanaRelations as IanaRelations
import qualified Hydra.Sources.Other.Osv as Osv
import qualified Hydra.Sources.Other.StacItems as StacItems
import qualified Hydra.Sources.Parquet.Format as ParquetFormat
import qualified Hydra.Sources.Sql.Ansi as SqlAnsi
import qualified Hydra.Sources.Workflow as Workflow
import qualified Hydra.Sources.Xml.Schema as XmlSchema
import qualified Hydra.Sources.Kernel.Types.All as KernelTypesAll

import qualified Data.List as L
import qualified Data.Set as S
import qualified Test.Hspec as H


-- | Schema-validity checks (#816) for the hydra-ext modules that are pure schema/syntax models with no
-- coder: SQL (Ansi), Parquet, Delta Parquet, Kusto (KQL), C#, and the "Other" domain modules.
-- There is no generation entry point to golden-test for these, so the checks are structural:
-- every definition is a type definition, definition names are unique and live in the module's own
-- namespace, and every type reference resolves to a definition of the module, of another model in this
-- list (e.g. STAC items reference GeoJSON), of the XML schema module, or of the kernel.
spec :: H.Spec
spec = H.describe "hydra-ext schema models (#816)" $
  mapM_ checkModule schemaModules

schemaModules :: [Module]
schemaModules = [
  Atlas.module_, AzureDtld.module_, CsharpSyntax.module_, Datalog.module_, DeltaParquet.module_,
  GeoJson.module_, IanaRelations.module_, KustoKql.module_, Osv.module_, ParquetFormat.module_,
  SqlAnsi.module_, StacItems.module_, Workflow.module_]

checkModule :: Module -> H.Spec
checkModule m = H.describe (unModuleName (moduleName m)) $ do

  H.it "contains at least one definition" $
    null (moduleDefinitions m) `H.shouldBe` False

  H.it "contains only type definitions" $
    [n | d <- moduleDefinitions m, not (isType d), let n = defName d] `H.shouldBe` []

  H.it "has unique definition names" $
    duplicates names `H.shouldBe` []

  H.it "keeps every definition in the module's own namespace" $
    [unName n | n <- names, not ((unModuleName (moduleName m) ++ ".") `L.isPrefixOf` unName n)]
      `H.shouldBe` []

  H.it "resolves every type reference to a module or kernel type" $
    [unName r | r <- S.toList refs, not (S.member r known)] `H.shouldBe` []
  where
    typeDefs = [td | DefinitionType td <- moduleDefinitions m]
    names = map typeDefinitionName typeDefs
    known = allModelNames `S.union` kernelTypeNames
    refs = S.unions [freeVariablesInType (typeSchemeBody (typeDefinitionBody td)) | td <- typeDefs]
    isType (DefinitionType _) = True
    isType _ = False
    defName (DefinitionPrimitive p) = unName (primitiveDefinitionName p)
    defName (DefinitionTerm t) = unName (termDefinitionName t)
    defName (DefinitionType t) = unName (typeDefinitionName t)

allModelNames :: S.Set Name
allModelNames = S.fromList
  [typeDefinitionName td | sm <- XmlSchema.module_ : schemaModules, DefinitionType td <- moduleDefinitions sm]

kernelTypeNames :: S.Set Name
kernelTypeNames = S.fromList
  [typeDefinitionName td | km <- KernelTypesAll.kernelTypesModules, DefinitionType td <- moduleDefinitions km]

duplicates :: Ord a => [a] -> [a]
duplicates xs = S.toList (snd (foldl step (S.empty, S.empty) xs))
  where step (seen, dups) x = if S.member x seen then (seen, S.insert x dups) else (S.insert x seen, dups)
