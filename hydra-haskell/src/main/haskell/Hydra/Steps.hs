module Hydra.Steps (
  Coder(..),
  bidirectional,
  composeSteps,
  idCoder,
  stepEither,
  module Hydra.Evaluation
) where

import Hydra.Evaluation
import Control.Monad


instance Functor Result where
  fmap f r = case r of
    ResultFailure msg -> ResultFailure msg
    ResultSuccess x -> ResultSuccess $ f x 
instance Applicative Result where
  pure = ResultSuccess
  rf <*> rx = case (rf, rx) of
    (_, ResultFailure msg) -> ResultFailure msg
    (ResultFailure msg, _) -> ResultFailure msg
    (ResultSuccess f', ResultSuccess x') -> ResultSuccess $ f' x'
instance Monad Result where
  r >>= f = case r of
    ResultFailure msg -> ResultFailure msg 
    ResultSuccess x -> f x
instance MonadFail Result where
  fail = ResultFailure

composeSteps :: Coder a b -> Coder b c -> Coder a c
composeSteps s1 s2 = Coder {
  coderEncode = coderEncode s1 >=> coderEncode s2,
  coderDecode = coderDecode s2 >=> coderDecode s1}

bidirectional :: (CoderDirection -> b -> Result b) -> Coder b b
bidirectional m = Coder (m CoderDirectionEncode) (m CoderDirectionDecode)

idCoder :: Coder a a
idCoder = Coder pure pure

stepEither :: CoderDirection -> Coder a a -> a -> Result a
stepEither dir = case dir of
  CoderDirectionEncode -> coderEncode
  CoderDirectionDecode -> coderDecode
