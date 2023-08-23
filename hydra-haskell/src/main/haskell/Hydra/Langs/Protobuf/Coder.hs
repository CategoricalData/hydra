module Hydra.Langs.Protobuf.Coder (moduleToProtobuf) where

import Hydra.Kernel
import Hydra.Langs.Protobuf.Language
import qualified Hydra.Langs.Protobuf.Type as PT

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


moduleToProtobuf :: (Ord a, Read a, Show a) => Module a -> Flow (Graph a) (M.Map FilePath String)
moduleToProtobuf mod = fail "not implemented"

encodeType :: Type a -> Flow (Graph a) PT.Type
encodeType typ = fail "not implemented"
