-- | Meta-DSL for constructing context-related terms (Context, etc.)

module Hydra.Dsl.Meta.Context where

import Hydra.Kernel
import Hydra.Dsl.Meta.Phantoms

import qualified Hydra.Dsl.Meta.Lib.Eithers as Eithers
import qualified Hydra.Dsl.Meta.Lib.Lists as Lists
import qualified Hydra.Dsl.Meta.Lib.Maps as Maps

import qualified Data.Map as M


context :: TTerm [String] -> TTerm [String] -> TTerm (M.Map Name Term) -> TTerm Context
context traceStack messages other = record _Context [
    _Context_trace>>: traceStack,
    _Context_messages>>: messages,
    _Context_other>>: other]

contextTrace :: TTerm Context -> TTerm [String]
contextTrace cx = project _Context _Context_trace @@ cx

contextMessages :: TTerm Context -> TTerm [String]
contextMessages cx = project _Context _Context_messages @@ cx

contextOther :: TTerm Context -> TTerm (M.Map Name Term)
contextOther cx = project _Context _Context_other @@ cx

contextWithTrace :: TTerm Context -> TTerm [String] -> TTerm Context
contextWithTrace cx newTrace = context
    newTrace
    (Hydra.Dsl.Meta.Context.contextMessages cx)
    (Hydra.Dsl.Meta.Context.contextOther cx)

inContext :: TTerm e -> TTerm Context -> TTerm (InContext e)
inContext obj cx = record _InContext [
    _InContext_object>>: obj,
    _InContext_context>>: cx]

inContextObject :: TTerm (InContext e) -> TTerm e
inContextObject ic = project _InContext _InContext_object @@ ic

inContextContext :: TTerm (InContext e) -> TTerm Context
inContextContext ic = project _InContext _InContext_context @@ ic

-- | Wrap an Either's Left side with a Context, converting Either e a to Either (InContext e) a
withContext :: TTerm Context -> TTerm (Either e a) -> TTerm (Either (InContext e) a)
withContext cx e = Eithers.bimap ("_wc_e" ~> Hydra.Dsl.Meta.Context.inContext (var "_wc_e") cx) ("_wc_a" ~> var "_wc_a") e

-- | Push a trace frame onto a Context, returning a new Context with the updated trace stack
pushTrace :: TTerm String -> TTerm Context -> TTerm Context
pushTrace label cx = context
    (Lists.cons label (Hydra.Dsl.Meta.Context.contextTrace cx))
    (Hydra.Dsl.Meta.Context.contextMessages cx)
    (Hydra.Dsl.Meta.Context.contextOther cx)

-- | Create a new Context with an updated 'other' map
contextWithOther :: TTerm Context -> TTerm (M.Map Name Term) -> TTerm Context
contextWithOther cx newOther = context
    (Hydra.Dsl.Meta.Context.contextTrace cx)
    (Hydra.Dsl.Meta.Context.contextMessages cx)
    newOther

-- | Create a Left (InContext error cx) — the Either-based replacement for Flows.fail
failInContext :: TTerm e -> TTerm Context -> TTerm (Either (InContext e) a)
failInContext err cx = left (Hydra.Dsl.Meta.Context.inContext err cx)
