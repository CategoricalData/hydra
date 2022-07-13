module Hydra.Impl.Haskell.Extras (
  debug,
  eitherToQualified,
  elementAsTypedTerm,
  fieldTypes,
  qualifiedToResult,
  requireType,
  resultToQualified,
  setContextElements,
  unexpected,
  unidirectionalStep,
  module Hydra.Common,
  module Hydra.Errors,
  ) where

import Hydra.Common
import Hydra.Core
import Hydra.Errors
import Hydra.Graph
import Hydra.Primitives
import Hydra.Steps
import Hydra.CoreDecoding

import qualified Data.List as L
import qualified Data.Map as M


instance Functor Qualified where
  fmap f (Qualified x msgs) = Qualified (fmap f x) msgs
instance Applicative Qualified where
  pure x = Qualified (Just x) []
  Qualified f mf <*> Qualified x mx = Qualified (f <*> x) $ mf <> mx
instance Monad Qualified where
  Qualified x m >>= f = case x of
    Nothing -> Qualified Nothing m
    Just x' -> Qualified fx $ m2 <> m
      where Qualified fx m2 = f x'
instance MonadFail Qualified where
  fail msg = Qualified Nothing [msg]

debug :: String -> Result m -> Result m
debug msg r = case r of
  ResultSuccess _ -> r
  ResultFailure msg1 -> ResultFailure $ "failure[" ++ msg ++ "]: " ++ msg1

eitherToQualified :: Result m -> Qualified m
eitherToQualified e = case e of
  ResultFailure msg -> Qualified Nothing [msg]
  ResultSuccess x -> Qualified (Just x) []

elementAsTypedTerm :: (Show m) => Context m -> Element m -> Result (TypedTerm m)
elementAsTypedTerm schemaCtx el = TypedTerm <$> decodeType schemaCtx (elementSchema el) <*> pure (elementData el)

fieldTypes :: (Show m) => Context m -> Type m -> Result (M.Map FieldName (Type m))
fieldTypes scx t = case typeExpr t of
    TypeRecord fields -> pure $ toMap fields
    TypeUnion fields -> pure $ toMap fields
    TypeElement et -> fieldTypes scx et
    TypeNominal name -> do
      el <- requireElement (Just "field types") scx name
      decodeType scx (elementData el) >>= fieldTypes scx
    TypeLambda (LambdaType _ body) -> fieldTypes scx body
    _ -> fail $ "expected record or union type, but found " ++ show t
  where
    toMap fields = M.fromList (toPair <$> fields)
    toPair (FieldType fname ftype) = (fname, ftype)

qualifiedToResult :: Qualified m -> Result m
qualifiedToResult (Qualified x m) = case x of
  Nothing -> fail $ L.head m
  Just x' -> pure x'

requireType :: (Show m) => Context m -> Name -> Result (Type m)
requireType scx name = do
  el <- requireElement (Just "require type") scx name
  decodeType scx $ elementData el

resultToQualified :: Result m -> Qualified m
resultToQualified r = case r of
  ResultSuccess x -> pure x
  ResultFailure msg -> fail msg

setContextElements :: [Graph m] -> Context m -> Context m
setContextElements graphs cx = cx { contextElements = M.fromList $
  ((\e -> (elementName e, e)) <$> (L.concat (graphElements <$> graphs)))}

unexpected :: (MonadFail m, Show a1) => String -> a1 -> m a2
unexpected cat obj = fail $ "expected " ++ cat ++ " but found: " ++ show obj

unidirectionalStep :: (a -> Result b) -> Step a b
unidirectionalStep m = Step {
  stepOut = m,
  stepIn = \_ -> fail "inbound mapping is unsupported"}
