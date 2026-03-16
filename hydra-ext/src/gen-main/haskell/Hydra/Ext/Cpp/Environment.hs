-- Note: this is an automatically generated file. Do not edit.

-- | Type definitions for C++ code generation environment

module Hydra.Ext.Cpp.Environment where

import qualified Hydra.Core as Core
import qualified Hydra.Module as Module
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Environment for C++ code generation
data CppEnvironment = 
  CppEnvironment {
    -- | Namespace mapping for code generation
    cppEnvironmentNamespaces :: (Module.Namespaces String),
    -- | Type variables in scope, with their C++ names
    cppEnvironmentBoundTypeVariables :: ([Core.Name], (M.Map Core.Name String))}
  deriving (Eq, Ord, Read, Show)

_CppEnvironment = Core.Name "hydra.ext.cpp.environment.CppEnvironment"

_CppEnvironment_namespaces = Core.Name "namespaces"

_CppEnvironment_boundTypeVariables = Core.Name "boundTypeVariables"
