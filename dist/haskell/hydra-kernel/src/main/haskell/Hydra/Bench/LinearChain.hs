-- Note: this is an automatically generated file. Do not edit.
-- | Linear-chain inference benchmark. walkerK cases on _Term variants and recurses to walker(K-1) — depth-N type-resolution stress test.

module Hydra.Bench.LinearChain where
import qualified Hydra.Core as Core
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Strip as Strip
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Term walker level 0; recurses to walker0.
walker0 :: t0 -> Maybe t0
walker0 t = Just t
-- | Term walker level 1; recurses to walker0.
walker1 :: Core.Term -> Maybe Core.Term
walker1 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker0 arg) (\_ -> walker0 fun) (walker0 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker0 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker0 inner) (walker0 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 10; recurses to walker9.
walker10 :: Core.Term -> Maybe Core.Term
walker10 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker9 arg) (\_ -> walker9 fun) (walker9 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker9 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker9 inner) (walker9 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 100; recurses to walker99.
walker100 :: Core.Term -> Maybe Core.Term
walker100 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker99 arg) (\_ -> walker99 fun) (walker99 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker99 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker99 inner) (walker99 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 101; recurses to walker100.
walker101 :: Core.Term -> Maybe Core.Term
walker101 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker100 arg) (\_ -> walker100 fun) (walker100 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker100 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker100 inner) (walker100 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 102; recurses to walker101.
walker102 :: Core.Term -> Maybe Core.Term
walker102 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker101 arg) (\_ -> walker101 fun) (walker101 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker101 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker101 inner) (walker101 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 103; recurses to walker102.
walker103 :: Core.Term -> Maybe Core.Term
walker103 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker102 arg) (\_ -> walker102 fun) (walker102 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker102 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker102 inner) (walker102 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 104; recurses to walker103.
walker104 :: Core.Term -> Maybe Core.Term
walker104 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker103 arg) (\_ -> walker103 fun) (walker103 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker103 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker103 inner) (walker103 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 105; recurses to walker104.
walker105 :: Core.Term -> Maybe Core.Term
walker105 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker104 arg) (\_ -> walker104 fun) (walker104 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker104 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker104 inner) (walker104 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 106; recurses to walker105.
walker106 :: Core.Term -> Maybe Core.Term
walker106 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker105 arg) (\_ -> walker105 fun) (walker105 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker105 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker105 inner) (walker105 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 107; recurses to walker106.
walker107 :: Core.Term -> Maybe Core.Term
walker107 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker106 arg) (\_ -> walker106 fun) (walker106 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker106 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker106 inner) (walker106 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 108; recurses to walker107.
walker108 :: Core.Term -> Maybe Core.Term
walker108 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker107 arg) (\_ -> walker107 fun) (walker107 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker107 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker107 inner) (walker107 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 109; recurses to walker108.
walker109 :: Core.Term -> Maybe Core.Term
walker109 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker108 arg) (\_ -> walker108 fun) (walker108 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker108 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker108 inner) (walker108 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 11; recurses to walker10.
walker11 :: Core.Term -> Maybe Core.Term
walker11 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker10 arg) (\_ -> walker10 fun) (walker10 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker10 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker10 inner) (walker10 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 110; recurses to walker109.
walker110 :: Core.Term -> Maybe Core.Term
walker110 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker109 arg) (\_ -> walker109 fun) (walker109 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker109 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker109 inner) (walker109 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 111; recurses to walker110.
walker111 :: Core.Term -> Maybe Core.Term
walker111 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker110 arg) (\_ -> walker110 fun) (walker110 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker110 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker110 inner) (walker110 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 112; recurses to walker111.
walker112 :: Core.Term -> Maybe Core.Term
walker112 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker111 arg) (\_ -> walker111 fun) (walker111 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker111 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker111 inner) (walker111 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 113; recurses to walker112.
walker113 :: Core.Term -> Maybe Core.Term
walker113 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker112 arg) (\_ -> walker112 fun) (walker112 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker112 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker112 inner) (walker112 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 114; recurses to walker113.
walker114 :: Core.Term -> Maybe Core.Term
walker114 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker113 arg) (\_ -> walker113 fun) (walker113 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker113 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker113 inner) (walker113 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 115; recurses to walker114.
walker115 :: Core.Term -> Maybe Core.Term
walker115 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker114 arg) (\_ -> walker114 fun) (walker114 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker114 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker114 inner) (walker114 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 116; recurses to walker115.
walker116 :: Core.Term -> Maybe Core.Term
walker116 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker115 arg) (\_ -> walker115 fun) (walker115 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker115 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker115 inner) (walker115 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 117; recurses to walker116.
walker117 :: Core.Term -> Maybe Core.Term
walker117 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker116 arg) (\_ -> walker116 fun) (walker116 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker116 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker116 inner) (walker116 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 118; recurses to walker117.
walker118 :: Core.Term -> Maybe Core.Term
walker118 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker117 arg) (\_ -> walker117 fun) (walker117 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker117 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker117 inner) (walker117 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 119; recurses to walker118.
walker119 :: Core.Term -> Maybe Core.Term
walker119 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker118 arg) (\_ -> walker118 fun) (walker118 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker118 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker118 inner) (walker118 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 12; recurses to walker11.
walker12 :: Core.Term -> Maybe Core.Term
walker12 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker11 arg) (\_ -> walker11 fun) (walker11 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker11 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker11 inner) (walker11 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 120; recurses to walker119.
walker120 :: Core.Term -> Maybe Core.Term
walker120 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker119 arg) (\_ -> walker119 fun) (walker119 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker119 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker119 inner) (walker119 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 121; recurses to walker120.
walker121 :: Core.Term -> Maybe Core.Term
walker121 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker120 arg) (\_ -> walker120 fun) (walker120 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker120 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker120 inner) (walker120 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 122; recurses to walker121.
walker122 :: Core.Term -> Maybe Core.Term
walker122 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker121 arg) (\_ -> walker121 fun) (walker121 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker121 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker121 inner) (walker121 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 123; recurses to walker122.
walker123 :: Core.Term -> Maybe Core.Term
walker123 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker122 arg) (\_ -> walker122 fun) (walker122 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker122 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker122 inner) (walker122 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 124; recurses to walker123.
walker124 :: Core.Term -> Maybe Core.Term
walker124 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker123 arg) (\_ -> walker123 fun) (walker123 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker123 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker123 inner) (walker123 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 125; recurses to walker124.
walker125 :: Core.Term -> Maybe Core.Term
walker125 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker124 arg) (\_ -> walker124 fun) (walker124 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker124 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker124 inner) (walker124 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 126; recurses to walker125.
walker126 :: Core.Term -> Maybe Core.Term
walker126 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker125 arg) (\_ -> walker125 fun) (walker125 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker125 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker125 inner) (walker125 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 127; recurses to walker126.
walker127 :: Core.Term -> Maybe Core.Term
walker127 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker126 arg) (\_ -> walker126 fun) (walker126 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker126 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker126 inner) (walker126 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 128; recurses to walker127.
walker128 :: Core.Term -> Maybe Core.Term
walker128 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker127 arg) (\_ -> walker127 fun) (walker127 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker127 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker127 inner) (walker127 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 129; recurses to walker128.
walker129 :: Core.Term -> Maybe Core.Term
walker129 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker128 arg) (\_ -> walker128 fun) (walker128 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker128 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker128 inner) (walker128 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 13; recurses to walker12.
walker13 :: Core.Term -> Maybe Core.Term
walker13 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker12 arg) (\_ -> walker12 fun) (walker12 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker12 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker12 inner) (walker12 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 130; recurses to walker129.
walker130 :: Core.Term -> Maybe Core.Term
walker130 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker129 arg) (\_ -> walker129 fun) (walker129 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker129 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker129 inner) (walker129 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 131; recurses to walker130.
walker131 :: Core.Term -> Maybe Core.Term
walker131 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker130 arg) (\_ -> walker130 fun) (walker130 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker130 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker130 inner) (walker130 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 132; recurses to walker131.
walker132 :: Core.Term -> Maybe Core.Term
walker132 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker131 arg) (\_ -> walker131 fun) (walker131 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker131 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker131 inner) (walker131 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 133; recurses to walker132.
walker133 :: Core.Term -> Maybe Core.Term
walker133 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker132 arg) (\_ -> walker132 fun) (walker132 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker132 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker132 inner) (walker132 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 134; recurses to walker133.
walker134 :: Core.Term -> Maybe Core.Term
walker134 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker133 arg) (\_ -> walker133 fun) (walker133 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker133 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker133 inner) (walker133 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 135; recurses to walker134.
walker135 :: Core.Term -> Maybe Core.Term
walker135 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker134 arg) (\_ -> walker134 fun) (walker134 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker134 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker134 inner) (walker134 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 136; recurses to walker135.
walker136 :: Core.Term -> Maybe Core.Term
walker136 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker135 arg) (\_ -> walker135 fun) (walker135 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker135 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker135 inner) (walker135 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 137; recurses to walker136.
walker137 :: Core.Term -> Maybe Core.Term
walker137 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker136 arg) (\_ -> walker136 fun) (walker136 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker136 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker136 inner) (walker136 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 138; recurses to walker137.
walker138 :: Core.Term -> Maybe Core.Term
walker138 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker137 arg) (\_ -> walker137 fun) (walker137 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker137 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker137 inner) (walker137 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 139; recurses to walker138.
walker139 :: Core.Term -> Maybe Core.Term
walker139 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker138 arg) (\_ -> walker138 fun) (walker138 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker138 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker138 inner) (walker138 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 14; recurses to walker13.
walker14 :: Core.Term -> Maybe Core.Term
walker14 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker13 arg) (\_ -> walker13 fun) (walker13 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker13 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker13 inner) (walker13 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 140; recurses to walker139.
walker140 :: Core.Term -> Maybe Core.Term
walker140 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker139 arg) (\_ -> walker139 fun) (walker139 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker139 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker139 inner) (walker139 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 141; recurses to walker140.
walker141 :: Core.Term -> Maybe Core.Term
walker141 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker140 arg) (\_ -> walker140 fun) (walker140 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker140 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker140 inner) (walker140 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 142; recurses to walker141.
walker142 :: Core.Term -> Maybe Core.Term
walker142 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker141 arg) (\_ -> walker141 fun) (walker141 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker141 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker141 inner) (walker141 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 143; recurses to walker142.
walker143 :: Core.Term -> Maybe Core.Term
walker143 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker142 arg) (\_ -> walker142 fun) (walker142 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker142 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker142 inner) (walker142 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 144; recurses to walker143.
walker144 :: Core.Term -> Maybe Core.Term
walker144 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker143 arg) (\_ -> walker143 fun) (walker143 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker143 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker143 inner) (walker143 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 145; recurses to walker144.
walker145 :: Core.Term -> Maybe Core.Term
walker145 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker144 arg) (\_ -> walker144 fun) (walker144 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker144 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker144 inner) (walker144 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 146; recurses to walker145.
walker146 :: Core.Term -> Maybe Core.Term
walker146 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker145 arg) (\_ -> walker145 fun) (walker145 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker145 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker145 inner) (walker145 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 147; recurses to walker146.
walker147 :: Core.Term -> Maybe Core.Term
walker147 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker146 arg) (\_ -> walker146 fun) (walker146 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker146 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker146 inner) (walker146 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 148; recurses to walker147.
walker148 :: Core.Term -> Maybe Core.Term
walker148 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker147 arg) (\_ -> walker147 fun) (walker147 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker147 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker147 inner) (walker147 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 149; recurses to walker148.
walker149 :: Core.Term -> Maybe Core.Term
walker149 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker148 arg) (\_ -> walker148 fun) (walker148 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker148 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker148 inner) (walker148 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 15; recurses to walker14.
walker15 :: Core.Term -> Maybe Core.Term
walker15 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker14 arg) (\_ -> walker14 fun) (walker14 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker14 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker14 inner) (walker14 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 150; recurses to walker149.
walker150 :: Core.Term -> Maybe Core.Term
walker150 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker149 arg) (\_ -> walker149 fun) (walker149 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker149 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker149 inner) (walker149 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 151; recurses to walker150.
walker151 :: Core.Term -> Maybe Core.Term
walker151 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker150 arg) (\_ -> walker150 fun) (walker150 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker150 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker150 inner) (walker150 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 152; recurses to walker151.
walker152 :: Core.Term -> Maybe Core.Term
walker152 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker151 arg) (\_ -> walker151 fun) (walker151 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker151 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker151 inner) (walker151 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 153; recurses to walker152.
walker153 :: Core.Term -> Maybe Core.Term
walker153 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker152 arg) (\_ -> walker152 fun) (walker152 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker152 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker152 inner) (walker152 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 154; recurses to walker153.
walker154 :: Core.Term -> Maybe Core.Term
walker154 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker153 arg) (\_ -> walker153 fun) (walker153 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker153 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker153 inner) (walker153 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 155; recurses to walker154.
walker155 :: Core.Term -> Maybe Core.Term
walker155 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker154 arg) (\_ -> walker154 fun) (walker154 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker154 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker154 inner) (walker154 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 156; recurses to walker155.
walker156 :: Core.Term -> Maybe Core.Term
walker156 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker155 arg) (\_ -> walker155 fun) (walker155 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker155 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker155 inner) (walker155 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 157; recurses to walker156.
walker157 :: Core.Term -> Maybe Core.Term
walker157 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker156 arg) (\_ -> walker156 fun) (walker156 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker156 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker156 inner) (walker156 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 158; recurses to walker157.
walker158 :: Core.Term -> Maybe Core.Term
walker158 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker157 arg) (\_ -> walker157 fun) (walker157 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker157 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker157 inner) (walker157 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 159; recurses to walker158.
walker159 :: Core.Term -> Maybe Core.Term
walker159 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker158 arg) (\_ -> walker158 fun) (walker158 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker158 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker158 inner) (walker158 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 16; recurses to walker15.
walker16 :: Core.Term -> Maybe Core.Term
walker16 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker15 arg) (\_ -> walker15 fun) (walker15 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker15 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker15 inner) (walker15 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 160; recurses to walker159.
walker160 :: Core.Term -> Maybe Core.Term
walker160 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker159 arg) (\_ -> walker159 fun) (walker159 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker159 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker159 inner) (walker159 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 161; recurses to walker160.
walker161 :: Core.Term -> Maybe Core.Term
walker161 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker160 arg) (\_ -> walker160 fun) (walker160 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker160 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker160 inner) (walker160 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 162; recurses to walker161.
walker162 :: Core.Term -> Maybe Core.Term
walker162 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker161 arg) (\_ -> walker161 fun) (walker161 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker161 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker161 inner) (walker161 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 163; recurses to walker162.
walker163 :: Core.Term -> Maybe Core.Term
walker163 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker162 arg) (\_ -> walker162 fun) (walker162 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker162 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker162 inner) (walker162 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 164; recurses to walker163.
walker164 :: Core.Term -> Maybe Core.Term
walker164 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker163 arg) (\_ -> walker163 fun) (walker163 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker163 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker163 inner) (walker163 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 165; recurses to walker164.
walker165 :: Core.Term -> Maybe Core.Term
walker165 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker164 arg) (\_ -> walker164 fun) (walker164 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker164 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker164 inner) (walker164 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 166; recurses to walker165.
walker166 :: Core.Term -> Maybe Core.Term
walker166 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker165 arg) (\_ -> walker165 fun) (walker165 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker165 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker165 inner) (walker165 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 167; recurses to walker166.
walker167 :: Core.Term -> Maybe Core.Term
walker167 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker166 arg) (\_ -> walker166 fun) (walker166 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker166 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker166 inner) (walker166 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 168; recurses to walker167.
walker168 :: Core.Term -> Maybe Core.Term
walker168 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker167 arg) (\_ -> walker167 fun) (walker167 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker167 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker167 inner) (walker167 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 169; recurses to walker168.
walker169 :: Core.Term -> Maybe Core.Term
walker169 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker168 arg) (\_ -> walker168 fun) (walker168 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker168 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker168 inner) (walker168 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 17; recurses to walker16.
walker17 :: Core.Term -> Maybe Core.Term
walker17 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker16 arg) (\_ -> walker16 fun) (walker16 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker16 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker16 inner) (walker16 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 170; recurses to walker169.
walker170 :: Core.Term -> Maybe Core.Term
walker170 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker169 arg) (\_ -> walker169 fun) (walker169 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker169 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker169 inner) (walker169 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 171; recurses to walker170.
walker171 :: Core.Term -> Maybe Core.Term
walker171 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker170 arg) (\_ -> walker170 fun) (walker170 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker170 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker170 inner) (walker170 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 172; recurses to walker171.
walker172 :: Core.Term -> Maybe Core.Term
walker172 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker171 arg) (\_ -> walker171 fun) (walker171 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker171 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker171 inner) (walker171 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 173; recurses to walker172.
walker173 :: Core.Term -> Maybe Core.Term
walker173 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker172 arg) (\_ -> walker172 fun) (walker172 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker172 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker172 inner) (walker172 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 174; recurses to walker173.
walker174 :: Core.Term -> Maybe Core.Term
walker174 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker173 arg) (\_ -> walker173 fun) (walker173 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker173 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker173 inner) (walker173 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 175; recurses to walker174.
walker175 :: Core.Term -> Maybe Core.Term
walker175 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker174 arg) (\_ -> walker174 fun) (walker174 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker174 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker174 inner) (walker174 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 176; recurses to walker175.
walker176 :: Core.Term -> Maybe Core.Term
walker176 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker175 arg) (\_ -> walker175 fun) (walker175 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker175 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker175 inner) (walker175 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 177; recurses to walker176.
walker177 :: Core.Term -> Maybe Core.Term
walker177 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker176 arg) (\_ -> walker176 fun) (walker176 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker176 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker176 inner) (walker176 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 178; recurses to walker177.
walker178 :: Core.Term -> Maybe Core.Term
walker178 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker177 arg) (\_ -> walker177 fun) (walker177 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker177 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker177 inner) (walker177 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 179; recurses to walker178.
walker179 :: Core.Term -> Maybe Core.Term
walker179 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker178 arg) (\_ -> walker178 fun) (walker178 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker178 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker178 inner) (walker178 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 18; recurses to walker17.
walker18 :: Core.Term -> Maybe Core.Term
walker18 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker17 arg) (\_ -> walker17 fun) (walker17 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker17 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker17 inner) (walker17 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 180; recurses to walker179.
walker180 :: Core.Term -> Maybe Core.Term
walker180 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker179 arg) (\_ -> walker179 fun) (walker179 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker179 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker179 inner) (walker179 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 181; recurses to walker180.
walker181 :: Core.Term -> Maybe Core.Term
walker181 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker180 arg) (\_ -> walker180 fun) (walker180 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker180 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker180 inner) (walker180 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 182; recurses to walker181.
walker182 :: Core.Term -> Maybe Core.Term
walker182 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker181 arg) (\_ -> walker181 fun) (walker181 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker181 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker181 inner) (walker181 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 183; recurses to walker182.
walker183 :: Core.Term -> Maybe Core.Term
walker183 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker182 arg) (\_ -> walker182 fun) (walker182 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker182 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker182 inner) (walker182 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 184; recurses to walker183.
walker184 :: Core.Term -> Maybe Core.Term
walker184 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker183 arg) (\_ -> walker183 fun) (walker183 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker183 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker183 inner) (walker183 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 185; recurses to walker184.
walker185 :: Core.Term -> Maybe Core.Term
walker185 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker184 arg) (\_ -> walker184 fun) (walker184 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker184 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker184 inner) (walker184 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 186; recurses to walker185.
walker186 :: Core.Term -> Maybe Core.Term
walker186 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker185 arg) (\_ -> walker185 fun) (walker185 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker185 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker185 inner) (walker185 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 187; recurses to walker186.
walker187 :: Core.Term -> Maybe Core.Term
walker187 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker186 arg) (\_ -> walker186 fun) (walker186 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker186 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker186 inner) (walker186 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 188; recurses to walker187.
walker188 :: Core.Term -> Maybe Core.Term
walker188 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker187 arg) (\_ -> walker187 fun) (walker187 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker187 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker187 inner) (walker187 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 189; recurses to walker188.
walker189 :: Core.Term -> Maybe Core.Term
walker189 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker188 arg) (\_ -> walker188 fun) (walker188 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker188 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker188 inner) (walker188 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 19; recurses to walker18.
walker19 :: Core.Term -> Maybe Core.Term
walker19 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker18 arg) (\_ -> walker18 fun) (walker18 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker18 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker18 inner) (walker18 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 190; recurses to walker189.
walker190 :: Core.Term -> Maybe Core.Term
walker190 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker189 arg) (\_ -> walker189 fun) (walker189 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker189 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker189 inner) (walker189 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 191; recurses to walker190.
walker191 :: Core.Term -> Maybe Core.Term
walker191 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker190 arg) (\_ -> walker190 fun) (walker190 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker190 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker190 inner) (walker190 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 192; recurses to walker191.
walker192 :: Core.Term -> Maybe Core.Term
walker192 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker191 arg) (\_ -> walker191 fun) (walker191 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker191 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker191 inner) (walker191 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 193; recurses to walker192.
walker193 :: Core.Term -> Maybe Core.Term
walker193 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker192 arg) (\_ -> walker192 fun) (walker192 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker192 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker192 inner) (walker192 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 194; recurses to walker193.
walker194 :: Core.Term -> Maybe Core.Term
walker194 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker193 arg) (\_ -> walker193 fun) (walker193 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker193 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker193 inner) (walker193 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 195; recurses to walker194.
walker195 :: Core.Term -> Maybe Core.Term
walker195 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker194 arg) (\_ -> walker194 fun) (walker194 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker194 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker194 inner) (walker194 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 196; recurses to walker195.
walker196 :: Core.Term -> Maybe Core.Term
walker196 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker195 arg) (\_ -> walker195 fun) (walker195 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker195 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker195 inner) (walker195 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 197; recurses to walker196.
walker197 :: Core.Term -> Maybe Core.Term
walker197 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker196 arg) (\_ -> walker196 fun) (walker196 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker196 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker196 inner) (walker196 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 198; recurses to walker197.
walker198 :: Core.Term -> Maybe Core.Term
walker198 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker197 arg) (\_ -> walker197 fun) (walker197 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker197 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker197 inner) (walker197 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 199; recurses to walker198.
walker199 :: Core.Term -> Maybe Core.Term
walker199 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker198 arg) (\_ -> walker198 fun) (walker198 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker198 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker198 inner) (walker198 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 2; recurses to walker1.
walker2 :: Core.Term -> Maybe Core.Term
walker2 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker1 arg) (\_ -> walker1 fun) (walker1 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker1 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker1 inner) (walker1 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 20; recurses to walker19.
walker20 :: Core.Term -> Maybe Core.Term
walker20 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker19 arg) (\_ -> walker19 fun) (walker19 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker19 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker19 inner) (walker19 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 200; recurses to walker199.
walker200 :: Core.Term -> Maybe Core.Term
walker200 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker199 arg) (\_ -> walker199 fun) (walker199 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker199 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker199 inner) (walker199 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 201; recurses to walker200.
walker201 :: Core.Term -> Maybe Core.Term
walker201 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker200 arg) (\_ -> walker200 fun) (walker200 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker200 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker200 inner) (walker200 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 202; recurses to walker201.
walker202 :: Core.Term -> Maybe Core.Term
walker202 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker201 arg) (\_ -> walker201 fun) (walker201 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker201 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker201 inner) (walker201 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 203; recurses to walker202.
walker203 :: Core.Term -> Maybe Core.Term
walker203 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker202 arg) (\_ -> walker202 fun) (walker202 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker202 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker202 inner) (walker202 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 204; recurses to walker203.
walker204 :: Core.Term -> Maybe Core.Term
walker204 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker203 arg) (\_ -> walker203 fun) (walker203 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker203 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker203 inner) (walker203 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 205; recurses to walker204.
walker205 :: Core.Term -> Maybe Core.Term
walker205 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker204 arg) (\_ -> walker204 fun) (walker204 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker204 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker204 inner) (walker204 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 206; recurses to walker205.
walker206 :: Core.Term -> Maybe Core.Term
walker206 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker205 arg) (\_ -> walker205 fun) (walker205 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker205 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker205 inner) (walker205 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 207; recurses to walker206.
walker207 :: Core.Term -> Maybe Core.Term
walker207 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker206 arg) (\_ -> walker206 fun) (walker206 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker206 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker206 inner) (walker206 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 208; recurses to walker207.
walker208 :: Core.Term -> Maybe Core.Term
walker208 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker207 arg) (\_ -> walker207 fun) (walker207 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker207 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker207 inner) (walker207 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 209; recurses to walker208.
walker209 :: Core.Term -> Maybe Core.Term
walker209 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker208 arg) (\_ -> walker208 fun) (walker208 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker208 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker208 inner) (walker208 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 21; recurses to walker20.
walker21 :: Core.Term -> Maybe Core.Term
walker21 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker20 arg) (\_ -> walker20 fun) (walker20 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker20 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker20 inner) (walker20 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 210; recurses to walker209.
walker210 :: Core.Term -> Maybe Core.Term
walker210 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker209 arg) (\_ -> walker209 fun) (walker209 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker209 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker209 inner) (walker209 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 211; recurses to walker210.
walker211 :: Core.Term -> Maybe Core.Term
walker211 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker210 arg) (\_ -> walker210 fun) (walker210 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker210 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker210 inner) (walker210 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 212; recurses to walker211.
walker212 :: Core.Term -> Maybe Core.Term
walker212 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker211 arg) (\_ -> walker211 fun) (walker211 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker211 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker211 inner) (walker211 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 213; recurses to walker212.
walker213 :: Core.Term -> Maybe Core.Term
walker213 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker212 arg) (\_ -> walker212 fun) (walker212 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker212 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker212 inner) (walker212 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 214; recurses to walker213.
walker214 :: Core.Term -> Maybe Core.Term
walker214 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker213 arg) (\_ -> walker213 fun) (walker213 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker213 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker213 inner) (walker213 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 215; recurses to walker214.
walker215 :: Core.Term -> Maybe Core.Term
walker215 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker214 arg) (\_ -> walker214 fun) (walker214 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker214 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker214 inner) (walker214 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 216; recurses to walker215.
walker216 :: Core.Term -> Maybe Core.Term
walker216 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker215 arg) (\_ -> walker215 fun) (walker215 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker215 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker215 inner) (walker215 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 217; recurses to walker216.
walker217 :: Core.Term -> Maybe Core.Term
walker217 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker216 arg) (\_ -> walker216 fun) (walker216 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker216 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker216 inner) (walker216 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 218; recurses to walker217.
walker218 :: Core.Term -> Maybe Core.Term
walker218 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker217 arg) (\_ -> walker217 fun) (walker217 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker217 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker217 inner) (walker217 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 219; recurses to walker218.
walker219 :: Core.Term -> Maybe Core.Term
walker219 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker218 arg) (\_ -> walker218 fun) (walker218 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker218 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker218 inner) (walker218 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 22; recurses to walker21.
walker22 :: Core.Term -> Maybe Core.Term
walker22 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker21 arg) (\_ -> walker21 fun) (walker21 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker21 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker21 inner) (walker21 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 220; recurses to walker219.
walker220 :: Core.Term -> Maybe Core.Term
walker220 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker219 arg) (\_ -> walker219 fun) (walker219 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker219 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker219 inner) (walker219 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 221; recurses to walker220.
walker221 :: Core.Term -> Maybe Core.Term
walker221 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker220 arg) (\_ -> walker220 fun) (walker220 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker220 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker220 inner) (walker220 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 222; recurses to walker221.
walker222 :: Core.Term -> Maybe Core.Term
walker222 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker221 arg) (\_ -> walker221 fun) (walker221 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker221 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker221 inner) (walker221 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 223; recurses to walker222.
walker223 :: Core.Term -> Maybe Core.Term
walker223 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker222 arg) (\_ -> walker222 fun) (walker222 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker222 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker222 inner) (walker222 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 224; recurses to walker223.
walker224 :: Core.Term -> Maybe Core.Term
walker224 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker223 arg) (\_ -> walker223 fun) (walker223 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker223 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker223 inner) (walker223 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 225; recurses to walker224.
walker225 :: Core.Term -> Maybe Core.Term
walker225 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker224 arg) (\_ -> walker224 fun) (walker224 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker224 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker224 inner) (walker224 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 226; recurses to walker225.
walker226 :: Core.Term -> Maybe Core.Term
walker226 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker225 arg) (\_ -> walker225 fun) (walker225 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker225 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker225 inner) (walker225 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 227; recurses to walker226.
walker227 :: Core.Term -> Maybe Core.Term
walker227 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker226 arg) (\_ -> walker226 fun) (walker226 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker226 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker226 inner) (walker226 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 228; recurses to walker227.
walker228 :: Core.Term -> Maybe Core.Term
walker228 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker227 arg) (\_ -> walker227 fun) (walker227 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker227 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker227 inner) (walker227 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 229; recurses to walker228.
walker229 :: Core.Term -> Maybe Core.Term
walker229 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker228 arg) (\_ -> walker228 fun) (walker228 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker228 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker228 inner) (walker228 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 23; recurses to walker22.
walker23 :: Core.Term -> Maybe Core.Term
walker23 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker22 arg) (\_ -> walker22 fun) (walker22 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker22 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker22 inner) (walker22 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 230; recurses to walker229.
walker230 :: Core.Term -> Maybe Core.Term
walker230 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker229 arg) (\_ -> walker229 fun) (walker229 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker229 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker229 inner) (walker229 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 231; recurses to walker230.
walker231 :: Core.Term -> Maybe Core.Term
walker231 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker230 arg) (\_ -> walker230 fun) (walker230 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker230 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker230 inner) (walker230 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 232; recurses to walker231.
walker232 :: Core.Term -> Maybe Core.Term
walker232 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker231 arg) (\_ -> walker231 fun) (walker231 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker231 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker231 inner) (walker231 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 233; recurses to walker232.
walker233 :: Core.Term -> Maybe Core.Term
walker233 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker232 arg) (\_ -> walker232 fun) (walker232 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker232 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker232 inner) (walker232 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 234; recurses to walker233.
walker234 :: Core.Term -> Maybe Core.Term
walker234 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker233 arg) (\_ -> walker233 fun) (walker233 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker233 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker233 inner) (walker233 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 235; recurses to walker234.
walker235 :: Core.Term -> Maybe Core.Term
walker235 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker234 arg) (\_ -> walker234 fun) (walker234 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker234 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker234 inner) (walker234 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 236; recurses to walker235.
walker236 :: Core.Term -> Maybe Core.Term
walker236 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker235 arg) (\_ -> walker235 fun) (walker235 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker235 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker235 inner) (walker235 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 237; recurses to walker236.
walker237 :: Core.Term -> Maybe Core.Term
walker237 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker236 arg) (\_ -> walker236 fun) (walker236 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker236 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker236 inner) (walker236 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 238; recurses to walker237.
walker238 :: Core.Term -> Maybe Core.Term
walker238 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker237 arg) (\_ -> walker237 fun) (walker237 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker237 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker237 inner) (walker237 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 239; recurses to walker238.
walker239 :: Core.Term -> Maybe Core.Term
walker239 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker238 arg) (\_ -> walker238 fun) (walker238 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker238 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker238 inner) (walker238 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 24; recurses to walker23.
walker24 :: Core.Term -> Maybe Core.Term
walker24 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker23 arg) (\_ -> walker23 fun) (walker23 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker23 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker23 inner) (walker23 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 240; recurses to walker239.
walker240 :: Core.Term -> Maybe Core.Term
walker240 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker239 arg) (\_ -> walker239 fun) (walker239 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker239 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker239 inner) (walker239 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 241; recurses to walker240.
walker241 :: Core.Term -> Maybe Core.Term
walker241 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker240 arg) (\_ -> walker240 fun) (walker240 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker240 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker240 inner) (walker240 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 242; recurses to walker241.
walker242 :: Core.Term -> Maybe Core.Term
walker242 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker241 arg) (\_ -> walker241 fun) (walker241 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker241 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker241 inner) (walker241 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 243; recurses to walker242.
walker243 :: Core.Term -> Maybe Core.Term
walker243 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker242 arg) (\_ -> walker242 fun) (walker242 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker242 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker242 inner) (walker242 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 244; recurses to walker243.
walker244 :: Core.Term -> Maybe Core.Term
walker244 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker243 arg) (\_ -> walker243 fun) (walker243 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker243 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker243 inner) (walker243 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 245; recurses to walker244.
walker245 :: Core.Term -> Maybe Core.Term
walker245 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker244 arg) (\_ -> walker244 fun) (walker244 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker244 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker244 inner) (walker244 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 246; recurses to walker245.
walker246 :: Core.Term -> Maybe Core.Term
walker246 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker245 arg) (\_ -> walker245 fun) (walker245 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker245 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker245 inner) (walker245 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 247; recurses to walker246.
walker247 :: Core.Term -> Maybe Core.Term
walker247 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker246 arg) (\_ -> walker246 fun) (walker246 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker246 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker246 inner) (walker246 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 248; recurses to walker247.
walker248 :: Core.Term -> Maybe Core.Term
walker248 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker247 arg) (\_ -> walker247 fun) (walker247 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker247 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker247 inner) (walker247 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 249; recurses to walker248.
walker249 :: Core.Term -> Maybe Core.Term
walker249 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker248 arg) (\_ -> walker248 fun) (walker248 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker248 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker248 inner) (walker248 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 25; recurses to walker24.
walker25 :: Core.Term -> Maybe Core.Term
walker25 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker24 arg) (\_ -> walker24 fun) (walker24 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker24 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker24 inner) (walker24 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 250; recurses to walker249.
walker250 :: Core.Term -> Maybe Core.Term
walker250 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker249 arg) (\_ -> walker249 fun) (walker249 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker249 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker249 inner) (walker249 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 251; recurses to walker250.
walker251 :: Core.Term -> Maybe Core.Term
walker251 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker250 arg) (\_ -> walker250 fun) (walker250 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker250 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker250 inner) (walker250 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 252; recurses to walker251.
walker252 :: Core.Term -> Maybe Core.Term
walker252 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker251 arg) (\_ -> walker251 fun) (walker251 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker251 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker251 inner) (walker251 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 253; recurses to walker252.
walker253 :: Core.Term -> Maybe Core.Term
walker253 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker252 arg) (\_ -> walker252 fun) (walker252 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker252 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker252 inner) (walker252 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 254; recurses to walker253.
walker254 :: Core.Term -> Maybe Core.Term
walker254 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker253 arg) (\_ -> walker253 fun) (walker253 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker253 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker253 inner) (walker253 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 255; recurses to walker254.
walker255 :: Core.Term -> Maybe Core.Term
walker255 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker254 arg) (\_ -> walker254 fun) (walker254 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker254 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker254 inner) (walker254 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 256; recurses to walker255.
walker256 :: Core.Term -> Maybe Core.Term
walker256 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker255 arg) (\_ -> walker255 fun) (walker255 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker255 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker255 inner) (walker255 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 257; recurses to walker256.
walker257 :: Core.Term -> Maybe Core.Term
walker257 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker256 arg) (\_ -> walker256 fun) (walker256 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker256 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker256 inner) (walker256 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 258; recurses to walker257.
walker258 :: Core.Term -> Maybe Core.Term
walker258 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker257 arg) (\_ -> walker257 fun) (walker257 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker257 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker257 inner) (walker257 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 259; recurses to walker258.
walker259 :: Core.Term -> Maybe Core.Term
walker259 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker258 arg) (\_ -> walker258 fun) (walker258 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker258 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker258 inner) (walker258 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 26; recurses to walker25.
walker26 :: Core.Term -> Maybe Core.Term
walker26 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker25 arg) (\_ -> walker25 fun) (walker25 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker25 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker25 inner) (walker25 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 260; recurses to walker259.
walker260 :: Core.Term -> Maybe Core.Term
walker260 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker259 arg) (\_ -> walker259 fun) (walker259 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker259 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker259 inner) (walker259 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 261; recurses to walker260.
walker261 :: Core.Term -> Maybe Core.Term
walker261 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker260 arg) (\_ -> walker260 fun) (walker260 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker260 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker260 inner) (walker260 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 262; recurses to walker261.
walker262 :: Core.Term -> Maybe Core.Term
walker262 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker261 arg) (\_ -> walker261 fun) (walker261 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker261 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker261 inner) (walker261 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 263; recurses to walker262.
walker263 :: Core.Term -> Maybe Core.Term
walker263 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker262 arg) (\_ -> walker262 fun) (walker262 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker262 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker262 inner) (walker262 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 264; recurses to walker263.
walker264 :: Core.Term -> Maybe Core.Term
walker264 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker263 arg) (\_ -> walker263 fun) (walker263 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker263 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker263 inner) (walker263 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 265; recurses to walker264.
walker265 :: Core.Term -> Maybe Core.Term
walker265 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker264 arg) (\_ -> walker264 fun) (walker264 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker264 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker264 inner) (walker264 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 266; recurses to walker265.
walker266 :: Core.Term -> Maybe Core.Term
walker266 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker265 arg) (\_ -> walker265 fun) (walker265 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker265 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker265 inner) (walker265 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 267; recurses to walker266.
walker267 :: Core.Term -> Maybe Core.Term
walker267 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker266 arg) (\_ -> walker266 fun) (walker266 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker266 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker266 inner) (walker266 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 268; recurses to walker267.
walker268 :: Core.Term -> Maybe Core.Term
walker268 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker267 arg) (\_ -> walker267 fun) (walker267 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker267 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker267 inner) (walker267 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 269; recurses to walker268.
walker269 :: Core.Term -> Maybe Core.Term
walker269 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker268 arg) (\_ -> walker268 fun) (walker268 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker268 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker268 inner) (walker268 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 27; recurses to walker26.
walker27 :: Core.Term -> Maybe Core.Term
walker27 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker26 arg) (\_ -> walker26 fun) (walker26 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker26 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker26 inner) (walker26 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 270; recurses to walker269.
walker270 :: Core.Term -> Maybe Core.Term
walker270 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker269 arg) (\_ -> walker269 fun) (walker269 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker269 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker269 inner) (walker269 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 271; recurses to walker270.
walker271 :: Core.Term -> Maybe Core.Term
walker271 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker270 arg) (\_ -> walker270 fun) (walker270 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker270 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker270 inner) (walker270 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 272; recurses to walker271.
walker272 :: Core.Term -> Maybe Core.Term
walker272 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker271 arg) (\_ -> walker271 fun) (walker271 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker271 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker271 inner) (walker271 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 273; recurses to walker272.
walker273 :: Core.Term -> Maybe Core.Term
walker273 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker272 arg) (\_ -> walker272 fun) (walker272 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker272 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker272 inner) (walker272 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 274; recurses to walker273.
walker274 :: Core.Term -> Maybe Core.Term
walker274 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker273 arg) (\_ -> walker273 fun) (walker273 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker273 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker273 inner) (walker273 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 275; recurses to walker274.
walker275 :: Core.Term -> Maybe Core.Term
walker275 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker274 arg) (\_ -> walker274 fun) (walker274 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker274 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker274 inner) (walker274 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 276; recurses to walker275.
walker276 :: Core.Term -> Maybe Core.Term
walker276 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker275 arg) (\_ -> walker275 fun) (walker275 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker275 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker275 inner) (walker275 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 277; recurses to walker276.
walker277 :: Core.Term -> Maybe Core.Term
walker277 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker276 arg) (\_ -> walker276 fun) (walker276 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker276 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker276 inner) (walker276 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 278; recurses to walker277.
walker278 :: Core.Term -> Maybe Core.Term
walker278 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker277 arg) (\_ -> walker277 fun) (walker277 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker277 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker277 inner) (walker277 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 279; recurses to walker278.
walker279 :: Core.Term -> Maybe Core.Term
walker279 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker278 arg) (\_ -> walker278 fun) (walker278 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker278 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker278 inner) (walker278 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 28; recurses to walker27.
walker28 :: Core.Term -> Maybe Core.Term
walker28 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker27 arg) (\_ -> walker27 fun) (walker27 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker27 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker27 inner) (walker27 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 280; recurses to walker279.
walker280 :: Core.Term -> Maybe Core.Term
walker280 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker279 arg) (\_ -> walker279 fun) (walker279 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker279 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker279 inner) (walker279 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 281; recurses to walker280.
walker281 :: Core.Term -> Maybe Core.Term
walker281 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker280 arg) (\_ -> walker280 fun) (walker280 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker280 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker280 inner) (walker280 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 282; recurses to walker281.
walker282 :: Core.Term -> Maybe Core.Term
walker282 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker281 arg) (\_ -> walker281 fun) (walker281 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker281 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker281 inner) (walker281 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 283; recurses to walker282.
walker283 :: Core.Term -> Maybe Core.Term
walker283 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker282 arg) (\_ -> walker282 fun) (walker282 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker282 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker282 inner) (walker282 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 284; recurses to walker283.
walker284 :: Core.Term -> Maybe Core.Term
walker284 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker283 arg) (\_ -> walker283 fun) (walker283 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker283 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker283 inner) (walker283 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 285; recurses to walker284.
walker285 :: Core.Term -> Maybe Core.Term
walker285 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker284 arg) (\_ -> walker284 fun) (walker284 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker284 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker284 inner) (walker284 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 286; recurses to walker285.
walker286 :: Core.Term -> Maybe Core.Term
walker286 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker285 arg) (\_ -> walker285 fun) (walker285 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker285 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker285 inner) (walker285 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 287; recurses to walker286.
walker287 :: Core.Term -> Maybe Core.Term
walker287 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker286 arg) (\_ -> walker286 fun) (walker286 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker286 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker286 inner) (walker286 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 288; recurses to walker287.
walker288 :: Core.Term -> Maybe Core.Term
walker288 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker287 arg) (\_ -> walker287 fun) (walker287 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker287 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker287 inner) (walker287 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 289; recurses to walker288.
walker289 :: Core.Term -> Maybe Core.Term
walker289 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker288 arg) (\_ -> walker288 fun) (walker288 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker288 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker288 inner) (walker288 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 29; recurses to walker28.
walker29 :: Core.Term -> Maybe Core.Term
walker29 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker28 arg) (\_ -> walker28 fun) (walker28 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker28 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker28 inner) (walker28 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 290; recurses to walker289.
walker290 :: Core.Term -> Maybe Core.Term
walker290 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker289 arg) (\_ -> walker289 fun) (walker289 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker289 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker289 inner) (walker289 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 291; recurses to walker290.
walker291 :: Core.Term -> Maybe Core.Term
walker291 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker290 arg) (\_ -> walker290 fun) (walker290 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker290 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker290 inner) (walker290 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 292; recurses to walker291.
walker292 :: Core.Term -> Maybe Core.Term
walker292 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker291 arg) (\_ -> walker291 fun) (walker291 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker291 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker291 inner) (walker291 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 293; recurses to walker292.
walker293 :: Core.Term -> Maybe Core.Term
walker293 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker292 arg) (\_ -> walker292 fun) (walker292 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker292 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker292 inner) (walker292 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 294; recurses to walker293.
walker294 :: Core.Term -> Maybe Core.Term
walker294 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker293 arg) (\_ -> walker293 fun) (walker293 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker293 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker293 inner) (walker293 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 295; recurses to walker294.
walker295 :: Core.Term -> Maybe Core.Term
walker295 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker294 arg) (\_ -> walker294 fun) (walker294 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker294 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker294 inner) (walker294 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 296; recurses to walker295.
walker296 :: Core.Term -> Maybe Core.Term
walker296 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker295 arg) (\_ -> walker295 fun) (walker295 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker295 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker295 inner) (walker295 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 297; recurses to walker296.
walker297 :: Core.Term -> Maybe Core.Term
walker297 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker296 arg) (\_ -> walker296 fun) (walker296 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker296 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker296 inner) (walker296 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 298; recurses to walker297.
walker298 :: Core.Term -> Maybe Core.Term
walker298 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker297 arg) (\_ -> walker297 fun) (walker297 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker297 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker297 inner) (walker297 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 299; recurses to walker298.
walker299 :: Core.Term -> Maybe Core.Term
walker299 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker298 arg) (\_ -> walker298 fun) (walker298 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker298 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker298 inner) (walker298 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 3; recurses to walker2.
walker3 :: Core.Term -> Maybe Core.Term
walker3 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker2 arg) (\_ -> walker2 fun) (walker2 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker2 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker2 inner) (walker2 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 30; recurses to walker29.
walker30 :: Core.Term -> Maybe Core.Term
walker30 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker29 arg) (\_ -> walker29 fun) (walker29 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker29 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker29 inner) (walker29 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 300; recurses to walker299.
walker300 :: Core.Term -> Maybe Core.Term
walker300 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker299 arg) (\_ -> walker299 fun) (walker299 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker299 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker299 inner) (walker299 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 301; recurses to walker300.
walker301 :: Core.Term -> Maybe Core.Term
walker301 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker300 arg) (\_ -> walker300 fun) (walker300 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker300 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker300 inner) (walker300 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 302; recurses to walker301.
walker302 :: Core.Term -> Maybe Core.Term
walker302 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker301 arg) (\_ -> walker301 fun) (walker301 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker301 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker301 inner) (walker301 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 303; recurses to walker302.
walker303 :: Core.Term -> Maybe Core.Term
walker303 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker302 arg) (\_ -> walker302 fun) (walker302 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker302 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker302 inner) (walker302 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 304; recurses to walker303.
walker304 :: Core.Term -> Maybe Core.Term
walker304 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker303 arg) (\_ -> walker303 fun) (walker303 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker303 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker303 inner) (walker303 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 305; recurses to walker304.
walker305 :: Core.Term -> Maybe Core.Term
walker305 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker304 arg) (\_ -> walker304 fun) (walker304 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker304 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker304 inner) (walker304 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 306; recurses to walker305.
walker306 :: Core.Term -> Maybe Core.Term
walker306 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker305 arg) (\_ -> walker305 fun) (walker305 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker305 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker305 inner) (walker305 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 307; recurses to walker306.
walker307 :: Core.Term -> Maybe Core.Term
walker307 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker306 arg) (\_ -> walker306 fun) (walker306 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker306 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker306 inner) (walker306 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 308; recurses to walker307.
walker308 :: Core.Term -> Maybe Core.Term
walker308 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker307 arg) (\_ -> walker307 fun) (walker307 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker307 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker307 inner) (walker307 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 309; recurses to walker308.
walker309 :: Core.Term -> Maybe Core.Term
walker309 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker308 arg) (\_ -> walker308 fun) (walker308 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker308 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker308 inner) (walker308 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 31; recurses to walker30.
walker31 :: Core.Term -> Maybe Core.Term
walker31 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker30 arg) (\_ -> walker30 fun) (walker30 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker30 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker30 inner) (walker30 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 310; recurses to walker309.
walker310 :: Core.Term -> Maybe Core.Term
walker310 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker309 arg) (\_ -> walker309 fun) (walker309 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker309 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker309 inner) (walker309 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 311; recurses to walker310.
walker311 :: Core.Term -> Maybe Core.Term
walker311 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker310 arg) (\_ -> walker310 fun) (walker310 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker310 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker310 inner) (walker310 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 312; recurses to walker311.
walker312 :: Core.Term -> Maybe Core.Term
walker312 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker311 arg) (\_ -> walker311 fun) (walker311 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker311 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker311 inner) (walker311 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 313; recurses to walker312.
walker313 :: Core.Term -> Maybe Core.Term
walker313 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker312 arg) (\_ -> walker312 fun) (walker312 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker312 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker312 inner) (walker312 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 314; recurses to walker313.
walker314 :: Core.Term -> Maybe Core.Term
walker314 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker313 arg) (\_ -> walker313 fun) (walker313 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker313 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker313 inner) (walker313 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 315; recurses to walker314.
walker315 :: Core.Term -> Maybe Core.Term
walker315 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker314 arg) (\_ -> walker314 fun) (walker314 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker314 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker314 inner) (walker314 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 316; recurses to walker315.
walker316 :: Core.Term -> Maybe Core.Term
walker316 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker315 arg) (\_ -> walker315 fun) (walker315 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker315 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker315 inner) (walker315 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 317; recurses to walker316.
walker317 :: Core.Term -> Maybe Core.Term
walker317 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker316 arg) (\_ -> walker316 fun) (walker316 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker316 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker316 inner) (walker316 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 318; recurses to walker317.
walker318 :: Core.Term -> Maybe Core.Term
walker318 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker317 arg) (\_ -> walker317 fun) (walker317 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker317 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker317 inner) (walker317 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 319; recurses to walker318.
walker319 :: Core.Term -> Maybe Core.Term
walker319 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker318 arg) (\_ -> walker318 fun) (walker318 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker318 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker318 inner) (walker318 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 32; recurses to walker31.
walker32 :: Core.Term -> Maybe Core.Term
walker32 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker31 arg) (\_ -> walker31 fun) (walker31 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker31 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker31 inner) (walker31 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 320; recurses to walker319.
walker320 :: Core.Term -> Maybe Core.Term
walker320 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker319 arg) (\_ -> walker319 fun) (walker319 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker319 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker319 inner) (walker319 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 321; recurses to walker320.
walker321 :: Core.Term -> Maybe Core.Term
walker321 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker320 arg) (\_ -> walker320 fun) (walker320 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker320 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker320 inner) (walker320 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 322; recurses to walker321.
walker322 :: Core.Term -> Maybe Core.Term
walker322 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker321 arg) (\_ -> walker321 fun) (walker321 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker321 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker321 inner) (walker321 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 323; recurses to walker322.
walker323 :: Core.Term -> Maybe Core.Term
walker323 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker322 arg) (\_ -> walker322 fun) (walker322 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker322 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker322 inner) (walker322 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 324; recurses to walker323.
walker324 :: Core.Term -> Maybe Core.Term
walker324 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker323 arg) (\_ -> walker323 fun) (walker323 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker323 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker323 inner) (walker323 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 325; recurses to walker324.
walker325 :: Core.Term -> Maybe Core.Term
walker325 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker324 arg) (\_ -> walker324 fun) (walker324 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker324 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker324 inner) (walker324 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 326; recurses to walker325.
walker326 :: Core.Term -> Maybe Core.Term
walker326 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker325 arg) (\_ -> walker325 fun) (walker325 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker325 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker325 inner) (walker325 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 327; recurses to walker326.
walker327 :: Core.Term -> Maybe Core.Term
walker327 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker326 arg) (\_ -> walker326 fun) (walker326 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker326 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker326 inner) (walker326 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 328; recurses to walker327.
walker328 :: Core.Term -> Maybe Core.Term
walker328 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker327 arg) (\_ -> walker327 fun) (walker327 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker327 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker327 inner) (walker327 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 329; recurses to walker328.
walker329 :: Core.Term -> Maybe Core.Term
walker329 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker328 arg) (\_ -> walker328 fun) (walker328 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker328 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker328 inner) (walker328 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 33; recurses to walker32.
walker33 :: Core.Term -> Maybe Core.Term
walker33 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker32 arg) (\_ -> walker32 fun) (walker32 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker32 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker32 inner) (walker32 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 330; recurses to walker329.
walker330 :: Core.Term -> Maybe Core.Term
walker330 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker329 arg) (\_ -> walker329 fun) (walker329 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker329 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker329 inner) (walker329 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 331; recurses to walker330.
walker331 :: Core.Term -> Maybe Core.Term
walker331 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker330 arg) (\_ -> walker330 fun) (walker330 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker330 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker330 inner) (walker330 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 332; recurses to walker331.
walker332 :: Core.Term -> Maybe Core.Term
walker332 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker331 arg) (\_ -> walker331 fun) (walker331 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker331 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker331 inner) (walker331 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 333; recurses to walker332.
walker333 :: Core.Term -> Maybe Core.Term
walker333 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker332 arg) (\_ -> walker332 fun) (walker332 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker332 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker332 inner) (walker332 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 334; recurses to walker333.
walker334 :: Core.Term -> Maybe Core.Term
walker334 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker333 arg) (\_ -> walker333 fun) (walker333 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker333 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker333 inner) (walker333 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 335; recurses to walker334.
walker335 :: Core.Term -> Maybe Core.Term
walker335 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker334 arg) (\_ -> walker334 fun) (walker334 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker334 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker334 inner) (walker334 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 336; recurses to walker335.
walker336 :: Core.Term -> Maybe Core.Term
walker336 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker335 arg) (\_ -> walker335 fun) (walker335 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker335 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker335 inner) (walker335 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 337; recurses to walker336.
walker337 :: Core.Term -> Maybe Core.Term
walker337 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker336 arg) (\_ -> walker336 fun) (walker336 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker336 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker336 inner) (walker336 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 338; recurses to walker337.
walker338 :: Core.Term -> Maybe Core.Term
walker338 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker337 arg) (\_ -> walker337 fun) (walker337 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker337 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker337 inner) (walker337 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 339; recurses to walker338.
walker339 :: Core.Term -> Maybe Core.Term
walker339 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker338 arg) (\_ -> walker338 fun) (walker338 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker338 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker338 inner) (walker338 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 34; recurses to walker33.
walker34 :: Core.Term -> Maybe Core.Term
walker34 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker33 arg) (\_ -> walker33 fun) (walker33 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker33 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker33 inner) (walker33 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 340; recurses to walker339.
walker340 :: Core.Term -> Maybe Core.Term
walker340 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker339 arg) (\_ -> walker339 fun) (walker339 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker339 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker339 inner) (walker339 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 341; recurses to walker340.
walker341 :: Core.Term -> Maybe Core.Term
walker341 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker340 arg) (\_ -> walker340 fun) (walker340 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker340 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker340 inner) (walker340 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 342; recurses to walker341.
walker342 :: Core.Term -> Maybe Core.Term
walker342 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker341 arg) (\_ -> walker341 fun) (walker341 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker341 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker341 inner) (walker341 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 343; recurses to walker342.
walker343 :: Core.Term -> Maybe Core.Term
walker343 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker342 arg) (\_ -> walker342 fun) (walker342 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker342 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker342 inner) (walker342 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 344; recurses to walker343.
walker344 :: Core.Term -> Maybe Core.Term
walker344 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker343 arg) (\_ -> walker343 fun) (walker343 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker343 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker343 inner) (walker343 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 345; recurses to walker344.
walker345 :: Core.Term -> Maybe Core.Term
walker345 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker344 arg) (\_ -> walker344 fun) (walker344 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker344 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker344 inner) (walker344 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 346; recurses to walker345.
walker346 :: Core.Term -> Maybe Core.Term
walker346 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker345 arg) (\_ -> walker345 fun) (walker345 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker345 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker345 inner) (walker345 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 347; recurses to walker346.
walker347 :: Core.Term -> Maybe Core.Term
walker347 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker346 arg) (\_ -> walker346 fun) (walker346 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker346 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker346 inner) (walker346 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 348; recurses to walker347.
walker348 :: Core.Term -> Maybe Core.Term
walker348 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker347 arg) (\_ -> walker347 fun) (walker347 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker347 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker347 inner) (walker347 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 349; recurses to walker348.
walker349 :: Core.Term -> Maybe Core.Term
walker349 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker348 arg) (\_ -> walker348 fun) (walker348 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker348 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker348 inner) (walker348 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 35; recurses to walker34.
walker35 :: Core.Term -> Maybe Core.Term
walker35 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker34 arg) (\_ -> walker34 fun) (walker34 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker34 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker34 inner) (walker34 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 350; recurses to walker349.
walker350 :: Core.Term -> Maybe Core.Term
walker350 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker349 arg) (\_ -> walker349 fun) (walker349 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker349 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker349 inner) (walker349 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 351; recurses to walker350.
walker351 :: Core.Term -> Maybe Core.Term
walker351 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker350 arg) (\_ -> walker350 fun) (walker350 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker350 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker350 inner) (walker350 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 352; recurses to walker351.
walker352 :: Core.Term -> Maybe Core.Term
walker352 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker351 arg) (\_ -> walker351 fun) (walker351 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker351 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker351 inner) (walker351 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 353; recurses to walker352.
walker353 :: Core.Term -> Maybe Core.Term
walker353 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker352 arg) (\_ -> walker352 fun) (walker352 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker352 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker352 inner) (walker352 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 354; recurses to walker353.
walker354 :: Core.Term -> Maybe Core.Term
walker354 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker353 arg) (\_ -> walker353 fun) (walker353 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker353 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker353 inner) (walker353 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 355; recurses to walker354.
walker355 :: Core.Term -> Maybe Core.Term
walker355 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker354 arg) (\_ -> walker354 fun) (walker354 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker354 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker354 inner) (walker354 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 356; recurses to walker355.
walker356 :: Core.Term -> Maybe Core.Term
walker356 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker355 arg) (\_ -> walker355 fun) (walker355 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker355 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker355 inner) (walker355 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 357; recurses to walker356.
walker357 :: Core.Term -> Maybe Core.Term
walker357 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker356 arg) (\_ -> walker356 fun) (walker356 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker356 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker356 inner) (walker356 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 358; recurses to walker357.
walker358 :: Core.Term -> Maybe Core.Term
walker358 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker357 arg) (\_ -> walker357 fun) (walker357 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker357 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker357 inner) (walker357 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 359; recurses to walker358.
walker359 :: Core.Term -> Maybe Core.Term
walker359 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker358 arg) (\_ -> walker358 fun) (walker358 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker358 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker358 inner) (walker358 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 36; recurses to walker35.
walker36 :: Core.Term -> Maybe Core.Term
walker36 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker35 arg) (\_ -> walker35 fun) (walker35 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker35 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker35 inner) (walker35 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 360; recurses to walker359.
walker360 :: Core.Term -> Maybe Core.Term
walker360 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker359 arg) (\_ -> walker359 fun) (walker359 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker359 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker359 inner) (walker359 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 361; recurses to walker360.
walker361 :: Core.Term -> Maybe Core.Term
walker361 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker360 arg) (\_ -> walker360 fun) (walker360 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker360 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker360 inner) (walker360 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 362; recurses to walker361.
walker362 :: Core.Term -> Maybe Core.Term
walker362 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker361 arg) (\_ -> walker361 fun) (walker361 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker361 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker361 inner) (walker361 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 363; recurses to walker362.
walker363 :: Core.Term -> Maybe Core.Term
walker363 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker362 arg) (\_ -> walker362 fun) (walker362 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker362 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker362 inner) (walker362 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 364; recurses to walker363.
walker364 :: Core.Term -> Maybe Core.Term
walker364 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker363 arg) (\_ -> walker363 fun) (walker363 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker363 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker363 inner) (walker363 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 365; recurses to walker364.
walker365 :: Core.Term -> Maybe Core.Term
walker365 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker364 arg) (\_ -> walker364 fun) (walker364 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker364 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker364 inner) (walker364 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 366; recurses to walker365.
walker366 :: Core.Term -> Maybe Core.Term
walker366 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker365 arg) (\_ -> walker365 fun) (walker365 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker365 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker365 inner) (walker365 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 367; recurses to walker366.
walker367 :: Core.Term -> Maybe Core.Term
walker367 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker366 arg) (\_ -> walker366 fun) (walker366 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker366 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker366 inner) (walker366 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 368; recurses to walker367.
walker368 :: Core.Term -> Maybe Core.Term
walker368 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker367 arg) (\_ -> walker367 fun) (walker367 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker367 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker367 inner) (walker367 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 369; recurses to walker368.
walker369 :: Core.Term -> Maybe Core.Term
walker369 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker368 arg) (\_ -> walker368 fun) (walker368 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker368 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker368 inner) (walker368 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 37; recurses to walker36.
walker37 :: Core.Term -> Maybe Core.Term
walker37 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker36 arg) (\_ -> walker36 fun) (walker36 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker36 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker36 inner) (walker36 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 370; recurses to walker369.
walker370 :: Core.Term -> Maybe Core.Term
walker370 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker369 arg) (\_ -> walker369 fun) (walker369 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker369 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker369 inner) (walker369 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 371; recurses to walker370.
walker371 :: Core.Term -> Maybe Core.Term
walker371 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker370 arg) (\_ -> walker370 fun) (walker370 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker370 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker370 inner) (walker370 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 372; recurses to walker371.
walker372 :: Core.Term -> Maybe Core.Term
walker372 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker371 arg) (\_ -> walker371 fun) (walker371 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker371 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker371 inner) (walker371 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 373; recurses to walker372.
walker373 :: Core.Term -> Maybe Core.Term
walker373 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker372 arg) (\_ -> walker372 fun) (walker372 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker372 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker372 inner) (walker372 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 374; recurses to walker373.
walker374 :: Core.Term -> Maybe Core.Term
walker374 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker373 arg) (\_ -> walker373 fun) (walker373 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker373 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker373 inner) (walker373 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 375; recurses to walker374.
walker375 :: Core.Term -> Maybe Core.Term
walker375 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker374 arg) (\_ -> walker374 fun) (walker374 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker374 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker374 inner) (walker374 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 376; recurses to walker375.
walker376 :: Core.Term -> Maybe Core.Term
walker376 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker375 arg) (\_ -> walker375 fun) (walker375 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker375 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker375 inner) (walker375 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 377; recurses to walker376.
walker377 :: Core.Term -> Maybe Core.Term
walker377 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker376 arg) (\_ -> walker376 fun) (walker376 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker376 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker376 inner) (walker376 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 378; recurses to walker377.
walker378 :: Core.Term -> Maybe Core.Term
walker378 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker377 arg) (\_ -> walker377 fun) (walker377 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker377 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker377 inner) (walker377 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 379; recurses to walker378.
walker379 :: Core.Term -> Maybe Core.Term
walker379 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker378 arg) (\_ -> walker378 fun) (walker378 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker378 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker378 inner) (walker378 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 38; recurses to walker37.
walker38 :: Core.Term -> Maybe Core.Term
walker38 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker37 arg) (\_ -> walker37 fun) (walker37 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker37 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker37 inner) (walker37 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 380; recurses to walker379.
walker380 :: Core.Term -> Maybe Core.Term
walker380 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker379 arg) (\_ -> walker379 fun) (walker379 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker379 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker379 inner) (walker379 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 381; recurses to walker380.
walker381 :: Core.Term -> Maybe Core.Term
walker381 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker380 arg) (\_ -> walker380 fun) (walker380 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker380 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker380 inner) (walker380 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 382; recurses to walker381.
walker382 :: Core.Term -> Maybe Core.Term
walker382 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker381 arg) (\_ -> walker381 fun) (walker381 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker381 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker381 inner) (walker381 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 383; recurses to walker382.
walker383 :: Core.Term -> Maybe Core.Term
walker383 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker382 arg) (\_ -> walker382 fun) (walker382 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker382 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker382 inner) (walker382 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 384; recurses to walker383.
walker384 :: Core.Term -> Maybe Core.Term
walker384 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker383 arg) (\_ -> walker383 fun) (walker383 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker383 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker383 inner) (walker383 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 385; recurses to walker384.
walker385 :: Core.Term -> Maybe Core.Term
walker385 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker384 arg) (\_ -> walker384 fun) (walker384 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker384 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker384 inner) (walker384 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 386; recurses to walker385.
walker386 :: Core.Term -> Maybe Core.Term
walker386 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker385 arg) (\_ -> walker385 fun) (walker385 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker385 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker385 inner) (walker385 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 387; recurses to walker386.
walker387 :: Core.Term -> Maybe Core.Term
walker387 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker386 arg) (\_ -> walker386 fun) (walker386 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker386 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker386 inner) (walker386 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 388; recurses to walker387.
walker388 :: Core.Term -> Maybe Core.Term
walker388 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker387 arg) (\_ -> walker387 fun) (walker387 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker387 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker387 inner) (walker387 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 389; recurses to walker388.
walker389 :: Core.Term -> Maybe Core.Term
walker389 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker388 arg) (\_ -> walker388 fun) (walker388 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker388 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker388 inner) (walker388 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 39; recurses to walker38.
walker39 :: Core.Term -> Maybe Core.Term
walker39 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker38 arg) (\_ -> walker38 fun) (walker38 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker38 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker38 inner) (walker38 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 390; recurses to walker389.
walker390 :: Core.Term -> Maybe Core.Term
walker390 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker389 arg) (\_ -> walker389 fun) (walker389 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker389 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker389 inner) (walker389 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 391; recurses to walker390.
walker391 :: Core.Term -> Maybe Core.Term
walker391 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker390 arg) (\_ -> walker390 fun) (walker390 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker390 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker390 inner) (walker390 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 392; recurses to walker391.
walker392 :: Core.Term -> Maybe Core.Term
walker392 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker391 arg) (\_ -> walker391 fun) (walker391 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker391 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker391 inner) (walker391 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 393; recurses to walker392.
walker393 :: Core.Term -> Maybe Core.Term
walker393 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker392 arg) (\_ -> walker392 fun) (walker392 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker392 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker392 inner) (walker392 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 394; recurses to walker393.
walker394 :: Core.Term -> Maybe Core.Term
walker394 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker393 arg) (\_ -> walker393 fun) (walker393 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker393 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker393 inner) (walker393 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 395; recurses to walker394.
walker395 :: Core.Term -> Maybe Core.Term
walker395 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker394 arg) (\_ -> walker394 fun) (walker394 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker394 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker394 inner) (walker394 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 396; recurses to walker395.
walker396 :: Core.Term -> Maybe Core.Term
walker396 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker395 arg) (\_ -> walker395 fun) (walker395 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker395 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker395 inner) (walker395 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 397; recurses to walker396.
walker397 :: Core.Term -> Maybe Core.Term
walker397 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker396 arg) (\_ -> walker396 fun) (walker396 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker396 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker396 inner) (walker396 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 398; recurses to walker397.
walker398 :: Core.Term -> Maybe Core.Term
walker398 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker397 arg) (\_ -> walker397 fun) (walker397 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker397 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker397 inner) (walker397 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 399; recurses to walker398.
walker399 :: Core.Term -> Maybe Core.Term
walker399 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker398 arg) (\_ -> walker398 fun) (walker398 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker398 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker398 inner) (walker398 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 4; recurses to walker3.
walker4 :: Core.Term -> Maybe Core.Term
walker4 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker3 arg) (\_ -> walker3 fun) (walker3 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker3 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker3 inner) (walker3 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 40; recurses to walker39.
walker40 :: Core.Term -> Maybe Core.Term
walker40 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker39 arg) (\_ -> walker39 fun) (walker39 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker39 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker39 inner) (walker39 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 41; recurses to walker40.
walker41 :: Core.Term -> Maybe Core.Term
walker41 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker40 arg) (\_ -> walker40 fun) (walker40 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker40 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker40 inner) (walker40 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 42; recurses to walker41.
walker42 :: Core.Term -> Maybe Core.Term
walker42 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker41 arg) (\_ -> walker41 fun) (walker41 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker41 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker41 inner) (walker41 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 43; recurses to walker42.
walker43 :: Core.Term -> Maybe Core.Term
walker43 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker42 arg) (\_ -> walker42 fun) (walker42 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker42 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker42 inner) (walker42 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 44; recurses to walker43.
walker44 :: Core.Term -> Maybe Core.Term
walker44 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker43 arg) (\_ -> walker43 fun) (walker43 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker43 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker43 inner) (walker43 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 45; recurses to walker44.
walker45 :: Core.Term -> Maybe Core.Term
walker45 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker44 arg) (\_ -> walker44 fun) (walker44 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker44 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker44 inner) (walker44 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 46; recurses to walker45.
walker46 :: Core.Term -> Maybe Core.Term
walker46 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker45 arg) (\_ -> walker45 fun) (walker45 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker45 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker45 inner) (walker45 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 47; recurses to walker46.
walker47 :: Core.Term -> Maybe Core.Term
walker47 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker46 arg) (\_ -> walker46 fun) (walker46 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker46 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker46 inner) (walker46 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 48; recurses to walker47.
walker48 :: Core.Term -> Maybe Core.Term
walker48 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker47 arg) (\_ -> walker47 fun) (walker47 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker47 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker47 inner) (walker47 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 49; recurses to walker48.
walker49 :: Core.Term -> Maybe Core.Term
walker49 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker48 arg) (\_ -> walker48 fun) (walker48 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker48 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker48 inner) (walker48 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 5; recurses to walker4.
walker5 :: Core.Term -> Maybe Core.Term
walker5 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker4 arg) (\_ -> walker4 fun) (walker4 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker4 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker4 inner) (walker4 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 50; recurses to walker49.
walker50 :: Core.Term -> Maybe Core.Term
walker50 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker49 arg) (\_ -> walker49 fun) (walker49 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker49 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker49 inner) (walker49 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 51; recurses to walker50.
walker51 :: Core.Term -> Maybe Core.Term
walker51 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker50 arg) (\_ -> walker50 fun) (walker50 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker50 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker50 inner) (walker50 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 52; recurses to walker51.
walker52 :: Core.Term -> Maybe Core.Term
walker52 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker51 arg) (\_ -> walker51 fun) (walker51 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker51 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker51 inner) (walker51 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 53; recurses to walker52.
walker53 :: Core.Term -> Maybe Core.Term
walker53 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker52 arg) (\_ -> walker52 fun) (walker52 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker52 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker52 inner) (walker52 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 54; recurses to walker53.
walker54 :: Core.Term -> Maybe Core.Term
walker54 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker53 arg) (\_ -> walker53 fun) (walker53 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker53 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker53 inner) (walker53 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 55; recurses to walker54.
walker55 :: Core.Term -> Maybe Core.Term
walker55 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker54 arg) (\_ -> walker54 fun) (walker54 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker54 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker54 inner) (walker54 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 56; recurses to walker55.
walker56 :: Core.Term -> Maybe Core.Term
walker56 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker55 arg) (\_ -> walker55 fun) (walker55 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker55 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker55 inner) (walker55 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 57; recurses to walker56.
walker57 :: Core.Term -> Maybe Core.Term
walker57 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker56 arg) (\_ -> walker56 fun) (walker56 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker56 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker56 inner) (walker56 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 58; recurses to walker57.
walker58 :: Core.Term -> Maybe Core.Term
walker58 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker57 arg) (\_ -> walker57 fun) (walker57 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker57 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker57 inner) (walker57 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 59; recurses to walker58.
walker59 :: Core.Term -> Maybe Core.Term
walker59 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker58 arg) (\_ -> walker58 fun) (walker58 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker58 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker58 inner) (walker58 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 6; recurses to walker5.
walker6 :: Core.Term -> Maybe Core.Term
walker6 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker5 arg) (\_ -> walker5 fun) (walker5 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker5 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker5 inner) (walker5 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 60; recurses to walker59.
walker60 :: Core.Term -> Maybe Core.Term
walker60 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker59 arg) (\_ -> walker59 fun) (walker59 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker59 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker59 inner) (walker59 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 61; recurses to walker60.
walker61 :: Core.Term -> Maybe Core.Term
walker61 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker60 arg) (\_ -> walker60 fun) (walker60 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker60 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker60 inner) (walker60 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 62; recurses to walker61.
walker62 :: Core.Term -> Maybe Core.Term
walker62 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker61 arg) (\_ -> walker61 fun) (walker61 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker61 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker61 inner) (walker61 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 63; recurses to walker62.
walker63 :: Core.Term -> Maybe Core.Term
walker63 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker62 arg) (\_ -> walker62 fun) (walker62 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker62 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker62 inner) (walker62 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 64; recurses to walker63.
walker64 :: Core.Term -> Maybe Core.Term
walker64 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker63 arg) (\_ -> walker63 fun) (walker63 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker63 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker63 inner) (walker63 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 65; recurses to walker64.
walker65 :: Core.Term -> Maybe Core.Term
walker65 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker64 arg) (\_ -> walker64 fun) (walker64 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker64 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker64 inner) (walker64 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 66; recurses to walker65.
walker66 :: Core.Term -> Maybe Core.Term
walker66 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker65 arg) (\_ -> walker65 fun) (walker65 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker65 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker65 inner) (walker65 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 67; recurses to walker66.
walker67 :: Core.Term -> Maybe Core.Term
walker67 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker66 arg) (\_ -> walker66 fun) (walker66 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker66 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker66 inner) (walker66 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 68; recurses to walker67.
walker68 :: Core.Term -> Maybe Core.Term
walker68 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker67 arg) (\_ -> walker67 fun) (walker67 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker67 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker67 inner) (walker67 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 69; recurses to walker68.
walker69 :: Core.Term -> Maybe Core.Term
walker69 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker68 arg) (\_ -> walker68 fun) (walker68 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker68 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker68 inner) (walker68 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 7; recurses to walker6.
walker7 :: Core.Term -> Maybe Core.Term
walker7 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker6 arg) (\_ -> walker6 fun) (walker6 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker6 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker6 inner) (walker6 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 70; recurses to walker69.
walker70 :: Core.Term -> Maybe Core.Term
walker70 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker69 arg) (\_ -> walker69 fun) (walker69 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker69 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker69 inner) (walker69 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 71; recurses to walker70.
walker71 :: Core.Term -> Maybe Core.Term
walker71 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker70 arg) (\_ -> walker70 fun) (walker70 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker70 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker70 inner) (walker70 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 72; recurses to walker71.
walker72 :: Core.Term -> Maybe Core.Term
walker72 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker71 arg) (\_ -> walker71 fun) (walker71 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker71 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker71 inner) (walker71 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 73; recurses to walker72.
walker73 :: Core.Term -> Maybe Core.Term
walker73 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker72 arg) (\_ -> walker72 fun) (walker72 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker72 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker72 inner) (walker72 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 74; recurses to walker73.
walker74 :: Core.Term -> Maybe Core.Term
walker74 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker73 arg) (\_ -> walker73 fun) (walker73 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker73 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker73 inner) (walker73 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 75; recurses to walker74.
walker75 :: Core.Term -> Maybe Core.Term
walker75 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker74 arg) (\_ -> walker74 fun) (walker74 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker74 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker74 inner) (walker74 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 76; recurses to walker75.
walker76 :: Core.Term -> Maybe Core.Term
walker76 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker75 arg) (\_ -> walker75 fun) (walker75 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker75 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker75 inner) (walker75 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 77; recurses to walker76.
walker77 :: Core.Term -> Maybe Core.Term
walker77 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker76 arg) (\_ -> walker76 fun) (walker76 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker76 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker76 inner) (walker76 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 78; recurses to walker77.
walker78 :: Core.Term -> Maybe Core.Term
walker78 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker77 arg) (\_ -> walker77 fun) (walker77 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker77 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker77 inner) (walker77 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 79; recurses to walker78.
walker79 :: Core.Term -> Maybe Core.Term
walker79 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker78 arg) (\_ -> walker78 fun) (walker78 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker78 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker78 inner) (walker78 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 8; recurses to walker7.
walker8 :: Core.Term -> Maybe Core.Term
walker8 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker7 arg) (\_ -> walker7 fun) (walker7 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker7 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker7 inner) (walker7 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 80; recurses to walker79.
walker80 :: Core.Term -> Maybe Core.Term
walker80 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker79 arg) (\_ -> walker79 fun) (walker79 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker79 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker79 inner) (walker79 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 81; recurses to walker80.
walker81 :: Core.Term -> Maybe Core.Term
walker81 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker80 arg) (\_ -> walker80 fun) (walker80 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker80 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker80 inner) (walker80 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 82; recurses to walker81.
walker82 :: Core.Term -> Maybe Core.Term
walker82 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker81 arg) (\_ -> walker81 fun) (walker81 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker81 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker81 inner) (walker81 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 83; recurses to walker82.
walker83 :: Core.Term -> Maybe Core.Term
walker83 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker82 arg) (\_ -> walker82 fun) (walker82 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker82 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker82 inner) (walker82 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 84; recurses to walker83.
walker84 :: Core.Term -> Maybe Core.Term
walker84 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker83 arg) (\_ -> walker83 fun) (walker83 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker83 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker83 inner) (walker83 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 85; recurses to walker84.
walker85 :: Core.Term -> Maybe Core.Term
walker85 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker84 arg) (\_ -> walker84 fun) (walker84 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker84 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker84 inner) (walker84 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 86; recurses to walker85.
walker86 :: Core.Term -> Maybe Core.Term
walker86 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker85 arg) (\_ -> walker85 fun) (walker85 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker85 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker85 inner) (walker85 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 87; recurses to walker86.
walker87 :: Core.Term -> Maybe Core.Term
walker87 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker86 arg) (\_ -> walker86 fun) (walker86 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker86 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker86 inner) (walker86 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 88; recurses to walker87.
walker88 :: Core.Term -> Maybe Core.Term
walker88 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker87 arg) (\_ -> walker87 fun) (walker87 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker87 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker87 inner) (walker87 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 89; recurses to walker88.
walker89 :: Core.Term -> Maybe Core.Term
walker89 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker88 arg) (\_ -> walker88 fun) (walker88 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker88 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker88 inner) (walker88 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 9; recurses to walker8.
walker9 :: Core.Term -> Maybe Core.Term
walker9 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker8 arg) (\_ -> walker8 fun) (walker8 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker8 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker8 inner) (walker8 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 90; recurses to walker89.
walker90 :: Core.Term -> Maybe Core.Term
walker90 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker89 arg) (\_ -> walker89 fun) (walker89 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker89 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker89 inner) (walker89 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 91; recurses to walker90.
walker91 :: Core.Term -> Maybe Core.Term
walker91 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker90 arg) (\_ -> walker90 fun) (walker90 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker90 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker90 inner) (walker90 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 92; recurses to walker91.
walker92 :: Core.Term -> Maybe Core.Term
walker92 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker91 arg) (\_ -> walker91 fun) (walker91 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker91 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker91 inner) (walker91 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 93; recurses to walker92.
walker93 :: Core.Term -> Maybe Core.Term
walker93 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker92 arg) (\_ -> walker92 fun) (walker92 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker92 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker92 inner) (walker92 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 94; recurses to walker93.
walker94 :: Core.Term -> Maybe Core.Term
walker94 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker93 arg) (\_ -> walker93 fun) (walker93 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker93 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker93 inner) (walker93 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 95; recurses to walker94.
walker95 :: Core.Term -> Maybe Core.Term
walker95 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker94 arg) (\_ -> walker94 fun) (walker94 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker94 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker94 inner) (walker94 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 96; recurses to walker95.
walker96 :: Core.Term -> Maybe Core.Term
walker96 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker95 arg) (\_ -> walker95 fun) (walker95 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker95 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker95 inner) (walker95 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 97; recurses to walker96.
walker97 :: Core.Term -> Maybe Core.Term
walker97 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker96 arg) (\_ -> walker96 fun) (walker96 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker96 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker96 inner) (walker96 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 98; recurses to walker97.
walker98 :: Core.Term -> Maybe Core.Term
walker98 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker97 arg) (\_ -> walker97 fun) (walker97 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker97 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker97 inner) (walker97 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 99; recurses to walker98.
walker99 :: Core.Term -> Maybe Core.Term
walker99 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker98 arg) (\_ -> walker98 fun) (walker98 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker98 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker98 inner) (walker98 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
