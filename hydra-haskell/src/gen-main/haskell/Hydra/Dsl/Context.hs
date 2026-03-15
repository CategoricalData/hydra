-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.context

module Hydra.Dsl.Context where

import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

context :: ([String] -> [String] -> M.Map Core.Name Core.Term -> Context.Context)
context trace messages other = Context.Context {
  Context.contextTrace = trace,
  Context.contextMessages = messages,
  Context.contextOther = other}

contextTrace :: (Context.Context -> [String])
contextTrace = Context.contextTrace

contextMessages :: (Context.Context -> [String])
contextMessages = Context.contextMessages

contextOther :: (Context.Context -> M.Map Core.Name Core.Term)
contextOther = Context.contextOther

contextWithTrace :: (Context.Context -> [String] -> Context.Context)
contextWithTrace original newVal = Context.Context {
  Context.contextTrace = newVal,
  Context.contextMessages = (Context.contextMessages original),
  Context.contextOther = (Context.contextOther original)}

contextWithMessages :: (Context.Context -> [String] -> Context.Context)
contextWithMessages original newVal = Context.Context {
  Context.contextTrace = (Context.contextTrace original),
  Context.contextMessages = newVal,
  Context.contextOther = (Context.contextOther original)}

contextWithOther :: (Context.Context -> M.Map Core.Name Core.Term -> Context.Context)
contextWithOther original newVal = Context.Context {
  Context.contextTrace = (Context.contextTrace original),
  Context.contextMessages = (Context.contextMessages original),
  Context.contextOther = newVal}

inContext :: (t0 -> Context.Context -> Context.InContext t0)
inContext object context = Context.InContext {
  Context.inContextObject = object,
  Context.inContextContext = context}

inContextObject :: (Context.InContext t0 -> t0)
inContextObject = Context.inContextObject

inContextContext :: (Context.InContext t0 -> Context.Context)
inContextContext = Context.inContextContext

inContextWithObject :: (Context.InContext t0 -> t1 -> Context.InContext t1)
inContextWithObject original newVal = Context.InContext {
  Context.inContextObject = newVal,
  Context.inContextContext = (Context.inContextContext original)}

inContextWithContext :: (Context.InContext t0 -> Context.Context -> Context.InContext t0)
inContextWithContext original newVal = Context.InContext {
  Context.inContextObject = (Context.inContextObject original),
  Context.inContextContext = newVal}
