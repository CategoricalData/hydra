module Hydra.Prototyping.Steps (
  Step(..),
) where

data Step a b = Step {
  stepOut :: a -> Either String b,
  stepIn :: b -> Either String a }

--data TypedStep at bt a b = TypedStep {
--  typedStepOut :: at,
--  typedStepIn :: bt,
--  typedStepStep :: Step a b }
