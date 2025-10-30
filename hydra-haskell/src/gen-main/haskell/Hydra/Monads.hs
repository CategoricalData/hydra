-- | Functions for working with Hydra's 'flow' and other monads.

module Hydra.Monads where

import qualified Hydra.Compute as Compute
import qualified Hydra.Constants as Constants
import qualified Hydra.Core as Core
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Optionals as Optionals
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Mantle as Mantle
import qualified Hydra.Show.Core as Core_
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

bind :: (Compute.Flow t0 t1 -> (t1 -> Compute.Flow t0 t2) -> Compute.Flow t0 t2)
bind l r =  
  let q = (\s0 -> \t0 ->  
          let fs1 = (Compute.unFlow l s0 t0)
          in (Optionals.maybe (Compute.FlowState {
            Compute.flowStateValue = Nothing,
            Compute.flowStateState = (Compute.flowStateState fs1),
            Compute.flowStateTrace = (Compute.flowStateTrace fs1)}) (\v -> Compute.unFlow (r v) (Compute.flowStateState fs1) (Compute.flowStateTrace fs1)) (Compute.flowStateValue fs1)))
  in (Compute.Flow q)

emptyTrace :: Compute.Trace
emptyTrace = Compute.Trace {
  Compute.traceStack = [],
  Compute.traceMessages = [],
  Compute.traceOther = Maps.empty}

exec :: (Compute.Flow t0 t1 -> t0 -> t0)
exec f s0 = (Compute.flowStateState (Compute.unFlow f s0 emptyTrace))

fail :: (String -> Compute.Flow t0 t1)
fail msg = (Compute.Flow (\s -> \t -> Compute.FlowState {
  Compute.flowStateValue = Nothing,
  Compute.flowStateState = s,
  Compute.flowStateTrace = (pushError msg t)}))

flowSucceeds :: (t0 -> Compute.Flow t0 t1 -> Bool)
flowSucceeds s f = (Optionals.isJust (Compute.flowStateValue (Compute.unFlow f s emptyTrace)))

fromFlow :: (t0 -> t1 -> Compute.Flow t1 t0 -> t0)
fromFlow def cx f = (Optionals.maybe def (\xmo -> xmo) (Compute.flowStateValue (Compute.unFlow f cx emptyTrace)))

getState :: (Compute.Flow t0 t0)
getState = (Compute.Flow (\s0 -> \t0 ->  
  let fs1 = (Compute.unFlow (pure ()) s0 t0)
  in  
    let v = (Compute.flowStateValue fs1)
    in  
      let s = (Compute.flowStateState fs1)
      in  
        let t = (Compute.flowStateTrace fs1)
        in (Optionals.maybe (Compute.FlowState {
          Compute.flowStateValue = Nothing,
          Compute.flowStateState = s,
          Compute.flowStateTrace = t}) (\_ -> Compute.FlowState {
          Compute.flowStateValue = (Just s),
          Compute.flowStateState = s,
          Compute.flowStateTrace = t}) v)))

map :: ((t0 -> t1) -> Compute.Flow t2 t0 -> Compute.Flow t2 t1)
map f f1 = (Compute.Flow (\s0 -> \t0 ->  
  let f2 = (Compute.unFlow f1 s0 t0)
  in Compute.FlowState {
    Compute.flowStateValue = (Optionals.map f (Compute.flowStateValue f2)),
    Compute.flowStateState = (Compute.flowStateState f2),
    Compute.flowStateTrace = (Compute.flowStateTrace f2)}))

modify :: ((t0 -> t0) -> Compute.Flow t0 ())
modify f = (bind getState (\s -> putState (f s)))

mutateTrace :: ((Compute.Trace -> Mantle.Either String Compute.Trace) -> (Compute.Trace -> Compute.Trace -> Compute.Trace) -> Compute.Flow t0 t1 -> Compute.Flow t0 t1)
mutateTrace mutate restore f =  
  let choose = (\forLeft -> \forRight -> \e -> (\x -> case x of
          Mantle.EitherLeft v1 -> (forLeft v1)
          Mantle.EitherRight v1 -> (forRight v1)) e)
  in  
    let flowFun = (\s0 -> \t0 ->  
            let forLeft = (\msg -> Compute.FlowState {
                    Compute.flowStateValue = Nothing,
                    Compute.flowStateState = s0,
                    Compute.flowStateTrace = (pushError msg t0)})
            in  
              let forRight = (\t1 ->  
                      let f2 = (Compute.unFlow f s0 t1)
                      in Compute.FlowState {
                        Compute.flowStateValue = (Compute.flowStateValue f2),
                        Compute.flowStateState = (Compute.flowStateState f2),
                        Compute.flowStateTrace = (restore t0 (Compute.flowStateTrace f2))})
              in (choose forLeft forRight (mutate t0)))
    in (Compute.Flow flowFun)

optionalToList :: (Maybe t0 -> [t0])
optionalToList mx = (Optionals.maybe [] Lists.pure mx)

pure :: (t0 -> Compute.Flow t1 t0)
pure xp = (Compute.Flow (\s -> \t -> Compute.FlowState {
  Compute.flowStateValue = (Just xp),
  Compute.flowStateState = s,
  Compute.flowStateTrace = t}))

-- | Push an error message
pushError :: (String -> Compute.Trace -> Compute.Trace)
pushError msg t =  
  let condenseRepeats = (\ys ->  
          let condenseGroup = (\xs ->  
                  let x = (Lists.head xs)
                  in  
                    let n = (Lists.length xs)
                    in (Logic.ifElse (Equality.equal n 1) x (Strings.cat [
                      x,
                      " (x",
                      Literals.showInt32 n,
                      ")"])))
          in (Lists.map condenseGroup (Lists.group ys)))
  in  
    let errorMsg = (Strings.cat [
            "Error: ",
            msg,
            " (",
            Strings.intercalate " > " (condenseRepeats (Lists.reverse (Compute.traceStack t))),
            ")"])
    in Compute.Trace {
      Compute.traceStack = (Compute.traceStack t),
      Compute.traceMessages = (Lists.cons errorMsg (Compute.traceMessages t)),
      Compute.traceOther = (Compute.traceOther t)}

putState :: (t0 -> Compute.Flow t0 ())
putState cx = (Compute.Flow (\s0 -> \t0 ->  
  let f1 = (Compute.unFlow (pure ()) s0 t0)
  in Compute.FlowState {
    Compute.flowStateValue = (Compute.flowStateValue f1),
    Compute.flowStateState = cx,
    Compute.flowStateTrace = (Compute.flowStateTrace f1)}))

-- | Summarize a trace as a string
traceSummary :: (Compute.Trace -> String)
traceSummary t =  
  let messageLines = (Lists.nub (Compute.traceMessages t))
  in  
    let toLine = (\pair -> Strings.cat [
            Strings.cat [
              Strings.cat [
                "\t",
                (Core.unName (fst pair))],
              ": "],
            (Core_.term (snd pair))])
    in  
      let keyvalLines = (Logic.ifElse (Maps.null (Compute.traceOther t)) [] (Lists.cons "key/value pairs: " (Lists.map toLine (Maps.toList (Compute.traceOther t)))))
      in (Strings.intercalate "\n" (Lists.concat2 messageLines keyvalLines))

unexpected :: (String -> String -> Compute.Flow t0 t1)
unexpected expected actual = (fail (Strings.cat [
  Strings.cat [
    Strings.cat [
      "expected ",
      expected],
    " but found: "],
  actual]))

warn :: (String -> Compute.Flow t0 t1 -> Compute.Flow t0 t1)
warn msg b = (Compute.Flow (\s0 -> \t0 ->  
  let f1 = (Compute.unFlow b s0 t0)
  in  
    let addMessage = (\t -> Compute.Trace {
            Compute.traceStack = (Compute.traceStack t),
            Compute.traceMessages = (Lists.cons (Strings.cat [
              "Warning: ",
              msg]) (Compute.traceMessages t)),
            Compute.traceOther = (Compute.traceOther t)})
    in Compute.FlowState {
      Compute.flowStateValue = (Compute.flowStateValue f1),
      Compute.flowStateState = (Compute.flowStateState f1),
      Compute.flowStateTrace = (addMessage (Compute.flowStateTrace f1))}))

withFlag :: (Core.Name -> Compute.Flow t0 t1 -> Compute.Flow t0 t1)
withFlag flag f =  
  let mutate = (\t -> Logic.ifElse False (Mantle.EitherLeft "never happens") (Mantle.EitherRight (Compute.Trace {
          Compute.traceStack = (Compute.traceStack t),
          Compute.traceMessages = (Compute.traceMessages t),
          Compute.traceOther = (Maps.insert flag (Core.TermLiteral (Core.LiteralBoolean True)) (Compute.traceOther t))})))
  in  
    let restore = (\ignored -> \t1 -> Compute.Trace {
            Compute.traceStack = (Compute.traceStack t1),
            Compute.traceMessages = (Compute.traceMessages t1),
            Compute.traceOther = (Maps.remove flag (Compute.traceOther t1))})
    in (mutateTrace mutate restore f)

withState :: (t0 -> Compute.Flow t0 t1 -> Compute.Flow t2 t1)
withState cx0 f = (Compute.Flow (\cx1 -> \t1 ->  
  let f1 = (Compute.unFlow f cx0 t1)
  in Compute.FlowState {
    Compute.flowStateValue = (Compute.flowStateValue f1),
    Compute.flowStateState = cx1,
    Compute.flowStateTrace = (Compute.flowStateTrace f1)}))

withTrace :: (String -> Compute.Flow t0 t1 -> Compute.Flow t0 t1)
withTrace msg f =  
  let mutate = (\t -> Logic.ifElse (Equality.gte (Lists.length (Compute.traceStack t)) Constants.maxTraceDepth) (Mantle.EitherLeft "maximum trace depth exceeded. This may indicate an infinite loop") (Mantle.EitherRight (Compute.Trace {
          Compute.traceStack = (Lists.cons msg (Compute.traceStack t)),
          Compute.traceMessages = (Compute.traceMessages t),
          Compute.traceOther = (Compute.traceOther t)})))
  in  
    let restore = (\t0 -> \t1 -> Compute.Trace {
            Compute.traceStack = (Compute.traceStack t0),
            Compute.traceMessages = (Compute.traceMessages t1),
            Compute.traceOther = (Compute.traceOther t1)})
    in (mutateTrace mutate restore f)
