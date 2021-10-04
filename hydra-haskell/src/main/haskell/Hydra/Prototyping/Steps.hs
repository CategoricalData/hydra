module Hydra.Prototyping.Steps (
  Step(..),
  idStep,
  stepEither,
  module Hydra.Traversal
) where

import Hydra.Traversal


data Step a b = Step {
  stepOut :: a -> Either String b,
  stepIn :: b -> Either String a }

idStep :: Step a a
idStep = Step (pure . id) (pure . id)

stepEither :: StepDirection -> Step a a -> a -> Either String a
stepEither dir = case dir of
  StepDirectionOut -> stepOut
  StepDirectionIn -> stepIn
