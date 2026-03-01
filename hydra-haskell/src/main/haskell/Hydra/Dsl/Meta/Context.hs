-- | Meta-DSL for constructing context-related terms (Context, etc.)

module Hydra.Dsl.Meta.Context where

import Hydra.Kernel
import Hydra.Dsl.Meta.Phantoms

import qualified Hydra.Dsl.Meta.Lib.Maps as Maps

import qualified Data.Map as M


context :: TTerm [String] -> TTerm [String] -> TTerm (M.Map Name Term) -> TTerm Context
context traceStack messages other = record _Context [
    _Context_trace>>: traceStack,
    _Context_messages>>: messages,
    _Context_other>>: other]

emptyContext :: TTerm Context
emptyContext = context (list ([] :: [TTerm String])) (list ([] :: [TTerm String])) Maps.empty

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
