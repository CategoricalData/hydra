-- Note: this is an automatically generated file. Do not edit.

-- | String representations of hydra.typing types

module Hydra.Show.Typing where

import qualified Hydra.Core as Core
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Show.Core as Core_
import qualified Hydra.Typing as Typing
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Show a type constraint as a string
typeConstraint :: (Typing.TypeConstraint -> String)
typeConstraint tc =  
  let ltyp = (Typing.typeConstraintLeft tc) 
      rtyp = (Typing.typeConstraintRight tc)
  in (Strings.cat [
    Core_.type_ ltyp,
    "\8801",
    (Core_.type_ rtyp)])

-- | Show a type substitution as a string
typeSubst :: (Typing.TypeSubst -> String)
typeSubst ts =  
  let subst = (Typing.unTypeSubst ts) 
      pairs = (Maps.toList subst)
      showPair = (\pair ->  
              let name = (Core.unName (Pairs.first pair)) 
                  typ = (Pairs.second pair)
              in (Strings.cat [
                name,
                "\8614",
                (Core_.type_ typ)]))
      pairStrs = (Lists.map showPair pairs)
  in (Strings.cat [
    "{",
    Strings.intercalate "," pairStrs,
    "}"])
