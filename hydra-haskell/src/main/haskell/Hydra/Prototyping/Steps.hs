module Hydra.Prototyping.Steps (
  Step(..),
  bidirectional,
  composeSteps,
  idStep,
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

composeSteps :: Step a b -> Step b c -> Step a c
composeSteps s1 s2 = Step {
  stepOut = stepOut s1 >=> stepOut s2,
  stepIn = stepIn s2 >=> stepIn s1}

bidirectional :: (StepDirection -> b -> Result b) -> Step b b
bidirectional m = Step (m StepDirectionOut) (m StepDirectionIn)

idStep :: Step a a
idStep = Step pure pure

stepEither :: StepDirection -> Step a a -> a -> Result a
stepEither dir = case dir of
  StepDirectionOut -> stepOut
  StepDirectionIn -> stepIn
