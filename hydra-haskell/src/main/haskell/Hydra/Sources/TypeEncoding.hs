{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.TypeEncoding where

import Hydra.Kernel
import Hydra.Sources.Core
import Hydra.Dsl.Base
import Hydra.TermEncoding
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types


typeEncodingModule :: Module Kv
typeEncodingModule = Module (Namespace "hydra/typeEncoding") elements [hydraCoreModule] $
    Just "Implementation of LambdaGraph's epsilon encoding, which maps types to terms and back"
  where
   elements = []
