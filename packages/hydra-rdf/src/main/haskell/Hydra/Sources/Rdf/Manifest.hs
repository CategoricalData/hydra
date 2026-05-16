-- | Package manifest for hydra-rdf.
--
-- Owns the RDF/OWL/SHACL/ShEx/XML Schema DSL sources. See
-- feature_290_packaging-plan.md, "Sync system redesign / Package manifests".

module Hydra.Sources.Rdf.Manifest (
  mainModules,
  testModules,
  dslInputModules,
) where

import Hydra.Kernel

import qualified Hydra.Sources.Owl.Syntax as OwlSyntax
import qualified Hydra.Sources.Rdf.Serde as RdfSerde
import qualified Hydra.Sources.Rdf.Syntax as RdfSyntax
import qualified Hydra.Sources.Rdf.Utils as RdfUtils
import qualified Hydra.Sources.Shacl.Coder as ShaclCoder
import qualified Hydra.Sources.Shacl.Language as ShaclLanguage
import qualified Hydra.Sources.Shacl.Model as ShaclModel
import qualified Hydra.Sources.Shex.Syntax as ShexSyntax
import qualified Hydra.Sources.Xml.Schema as XmlSchema

mainModules :: [Module]
mainModules = [
  OwlSyntax.module_,
  RdfSerde.module_,
  RdfSyntax.module_,
  RdfUtils.module_,
  ShaclCoder.module_,
  ShaclLanguage.module_,
  ShaclModel.module_,
  ShexSyntax.module_,
  XmlSchema.module_]

-- | Modules in this package whose type definitions should produce derived
-- DSL wrapper modules. Empty today — no hydra-rdf modules currently
-- generate DSL wrappers. Candidates if needed in the future include
-- RdfSyntax, OwlSyntax, ShexSyntax, ShaclModel, XmlSchema. Add
-- explicitly when the wrappers are wanted.
dslInputModules :: [Module]
dslInputModules = []

testModules :: [Module]
testModules = []
