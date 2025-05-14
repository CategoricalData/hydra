module Hydra.Dsl.Compute where

import Hydra.Kernel
import Hydra.Dsl.Phantoms
import Hydra.Dsl.Core

import qualified Data.Map as M
import qualified Data.Maybe as Y


traversalOrderPre = unitVariant _TraversalOrder _TraversalOrder_pre
traversalOrderPost = unitVariant _TraversalOrder _TraversalOrder_post
