-- Note: this file was created with the help of a large language model. It requires further human review.
-- TEMPORARILY STUBBED: The gen-main Cpp/Names and Cpp/Utils modules need regeneration before this
-- module can compile. The original implementation is preserved in git history.

module Hydra.Ext.Staging.Cpp.Coder (moduleToCpp) where

import Hydra.Kernel

import qualified Data.Map as M

type Result a = Either (InContext OtherError) a

-- | Convert a module to C++ code files
-- Note: temporarily stubbed until gen-main Cpp/Names and Cpp/Utils are regenerated
moduleToCpp :: Module -> [Definition] -> Context -> Graph -> Result (M.Map FilePath String)
moduleToCpp _ _ cx _ = Left (InContext (OtherError "C++ coder temporarily disabled pending gen-main regeneration") cx)
