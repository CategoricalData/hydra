-- Note: this is an automatically generated file. Do not edit.

module Generation.Spec (spec) where

import qualified Test.Hspec as H
import qualified Generation.Hydra.Test.FormattingSpec as Generation_Hydra_Test_FormattingSpec
import qualified Generation.Hydra.Test.Lib.ListsSpec as Generation_Hydra_Test_Lib_ListsSpec
import qualified Generation.Hydra.Test.Lib.LogicSpec as Generation_Hydra_Test_Lib_LogicSpec
import qualified Generation.Hydra.Test.Lib.MathSpec as Generation_Hydra_Test_Lib_MathSpec
import qualified Generation.Hydra.Test.Lib.StringsSpec as Generation_Hydra_Test_Lib_StringsSpec

spec :: H.Spec
spec = do
    Generation_Hydra_Test_FormattingSpec.spec
    Generation_Hydra_Test_Lib_ListsSpec.spec
    Generation_Hydra_Test_Lib_LogicSpec.spec
    Generation_Hydra_Test_Lib_MathSpec.spec
    Generation_Hydra_Test_Lib_StringsSpec.spec
