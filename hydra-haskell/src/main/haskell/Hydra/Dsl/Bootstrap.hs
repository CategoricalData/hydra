-- | A bootstrapping DSL, used for Hydra's inner core models

module Hydra.Dsl.Bootstrap (
  module Hydra.Dsl.AsType,
  bootstrapGraph,
  datatype,
  qualify,
  typeref,
  defineType,
  use,
  useType,
) where

import Hydra.Compute
import Hydra.Constants
import Hydra.Core
import Hydra.Dsl.AsType
import qualified Hydra.Encode.Core as EncodeCore
import Hydra.Graph
import Hydra.Lexical
import Hydra.Annotations
import Hydra.Module
import Hydra.Rewriting
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types
import Hydra.Tools.Debug

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S


-- | An empty graph (no elements, no schema, but with primitive functions) which is used for bootstrapping Hydra Core
bootstrapGraph :: Graph
bootstrapGraph = emptyGraph {graphPrimitives = M.fromList $ fmap (\p -> (primitiveName p, p)) (L.concat (libraryPrimitives <$> standardLibraries))}

datatype :: Namespace -> String -> Type -> Binding
datatype gname lname typ = typeElement elName $ rewriteType replacePlaceholders typ
  where
    elName = qualify gname (Name lname)

    -- Note: placeholders are only expected at the top level, or beneath annotations and/or type lambdas
    replacePlaceholders rec t = case rect of
        TypeRecord (RowType tname fields) -> if tname == placeholderName
          then TypeRecord (RowType elName fields)
          else rect
        TypeUnion (RowType tname fields) -> if tname == placeholderName
          then TypeUnion (RowType elName fields)
          else rect
        TypeWrap (WrappedType tname t) -> if tname == placeholderName
          then TypeWrap (WrappedType elName t)
          else rect
        _ -> rect
      where
        rect = rec t

qualify :: Namespace -> Name -> Name
qualify (Namespace gname) (Name lname) = Name $ gname ++ "." ++ lname

typeref :: Namespace -> String -> Type
typeref ns = TypeVariable . qualify ns . Name

-- | New DSL helpers (Option 1 from dsl-redesign-options.md)

-- | Define a type in a namespace
defineType :: Namespace -> String -> Type -> Binding
defineType = datatype

-- | Reference a type by its binding
use :: Binding -> Type
use b = TypeVariable (bindingName b)

-- | Reference a type in a namespace (old style, for migration)
useType :: Namespace -> String -> Type
useType = typeref
