-- | Haskell-specific convenience layer over the generated Hydra.Dsl.Parsing module.

module Hydra.Dsl.Meta.Parsing (
  module Hydra.Dsl.Parsing,
  module Hydra.Dsl.Meta.Parsing,
) where

import Hydra.Kernel
import Hydra.Dsl.Meta.Phantoms
import Hydra.Dsl.Parsing

runParser :: TTerm (Parser a) -> TTerm String -> TTerm (ParseResult a)
runParser p input = unwrap _Parser @@ p @@ input
