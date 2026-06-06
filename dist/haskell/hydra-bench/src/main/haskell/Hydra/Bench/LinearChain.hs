-- Note: this is an automatically generated file. Do not edit.
-- | Linear-chain inference benchmark. walkerK cases on _Term variants and recurses to walker(K-1) — depth-N type-resolution stress test.

module Hydra.Bench.LinearChain where
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Haskell.Lib.Maybes as Maybes
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Strip as Strip
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Topology as Topology
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
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
-- | Term walker level 400; recurses to walker399.
walker400 :: Core.Term -> Maybe Core.Term
walker400 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker399 arg) (\_ -> walker399 fun) (walker399 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker399 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker399 inner) (walker399 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 401; recurses to walker400.
walker401 :: Core.Term -> Maybe Core.Term
walker401 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker400 arg) (\_ -> walker400 fun) (walker400 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker400 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker400 inner) (walker400 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 402; recurses to walker401.
walker402 :: Core.Term -> Maybe Core.Term
walker402 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker401 arg) (\_ -> walker401 fun) (walker401 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker401 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker401 inner) (walker401 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 403; recurses to walker402.
walker403 :: Core.Term -> Maybe Core.Term
walker403 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker402 arg) (\_ -> walker402 fun) (walker402 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker402 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker402 inner) (walker402 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 404; recurses to walker403.
walker404 :: Core.Term -> Maybe Core.Term
walker404 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker403 arg) (\_ -> walker403 fun) (walker403 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker403 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker403 inner) (walker403 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 405; recurses to walker404.
walker405 :: Core.Term -> Maybe Core.Term
walker405 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker404 arg) (\_ -> walker404 fun) (walker404 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker404 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker404 inner) (walker404 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 406; recurses to walker405.
walker406 :: Core.Term -> Maybe Core.Term
walker406 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker405 arg) (\_ -> walker405 fun) (walker405 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker405 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker405 inner) (walker405 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 407; recurses to walker406.
walker407 :: Core.Term -> Maybe Core.Term
walker407 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker406 arg) (\_ -> walker406 fun) (walker406 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker406 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker406 inner) (walker406 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 408; recurses to walker407.
walker408 :: Core.Term -> Maybe Core.Term
walker408 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker407 arg) (\_ -> walker407 fun) (walker407 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker407 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker407 inner) (walker407 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 409; recurses to walker408.
walker409 :: Core.Term -> Maybe Core.Term
walker409 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker408 arg) (\_ -> walker408 fun) (walker408 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker408 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker408 inner) (walker408 body))
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
-- | Term walker level 410; recurses to walker409.
walker410 :: Core.Term -> Maybe Core.Term
walker410 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker409 arg) (\_ -> walker409 fun) (walker409 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker409 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker409 inner) (walker409 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 411; recurses to walker410.
walker411 :: Core.Term -> Maybe Core.Term
walker411 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker410 arg) (\_ -> walker410 fun) (walker410 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker410 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker410 inner) (walker410 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 412; recurses to walker411.
walker412 :: Core.Term -> Maybe Core.Term
walker412 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker411 arg) (\_ -> walker411 fun) (walker411 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker411 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker411 inner) (walker411 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 413; recurses to walker412.
walker413 :: Core.Term -> Maybe Core.Term
walker413 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker412 arg) (\_ -> walker412 fun) (walker412 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker412 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker412 inner) (walker412 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 414; recurses to walker413.
walker414 :: Core.Term -> Maybe Core.Term
walker414 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker413 arg) (\_ -> walker413 fun) (walker413 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker413 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker413 inner) (walker413 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 415; recurses to walker414.
walker415 :: Core.Term -> Maybe Core.Term
walker415 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker414 arg) (\_ -> walker414 fun) (walker414 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker414 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker414 inner) (walker414 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 416; recurses to walker415.
walker416 :: Core.Term -> Maybe Core.Term
walker416 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker415 arg) (\_ -> walker415 fun) (walker415 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker415 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker415 inner) (walker415 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 417; recurses to walker416.
walker417 :: Core.Term -> Maybe Core.Term
walker417 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker416 arg) (\_ -> walker416 fun) (walker416 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker416 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker416 inner) (walker416 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 418; recurses to walker417.
walker418 :: Core.Term -> Maybe Core.Term
walker418 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker417 arg) (\_ -> walker417 fun) (walker417 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker417 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker417 inner) (walker417 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 419; recurses to walker418.
walker419 :: Core.Term -> Maybe Core.Term
walker419 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker418 arg) (\_ -> walker418 fun) (walker418 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker418 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker418 inner) (walker418 body))
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
-- | Term walker level 420; recurses to walker419.
walker420 :: Core.Term -> Maybe Core.Term
walker420 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker419 arg) (\_ -> walker419 fun) (walker419 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker419 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker419 inner) (walker419 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 421; recurses to walker420.
walker421 :: Core.Term -> Maybe Core.Term
walker421 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker420 arg) (\_ -> walker420 fun) (walker420 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker420 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker420 inner) (walker420 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 422; recurses to walker421.
walker422 :: Core.Term -> Maybe Core.Term
walker422 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker421 arg) (\_ -> walker421 fun) (walker421 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker421 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker421 inner) (walker421 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 423; recurses to walker422.
walker423 :: Core.Term -> Maybe Core.Term
walker423 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker422 arg) (\_ -> walker422 fun) (walker422 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker422 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker422 inner) (walker422 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 424; recurses to walker423.
walker424 :: Core.Term -> Maybe Core.Term
walker424 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker423 arg) (\_ -> walker423 fun) (walker423 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker423 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker423 inner) (walker423 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 425; recurses to walker424.
walker425 :: Core.Term -> Maybe Core.Term
walker425 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker424 arg) (\_ -> walker424 fun) (walker424 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker424 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker424 inner) (walker424 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 426; recurses to walker425.
walker426 :: Core.Term -> Maybe Core.Term
walker426 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker425 arg) (\_ -> walker425 fun) (walker425 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker425 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker425 inner) (walker425 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 427; recurses to walker426.
walker427 :: Core.Term -> Maybe Core.Term
walker427 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker426 arg) (\_ -> walker426 fun) (walker426 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker426 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker426 inner) (walker426 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 428; recurses to walker427.
walker428 :: Core.Term -> Maybe Core.Term
walker428 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker427 arg) (\_ -> walker427 fun) (walker427 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker427 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker427 inner) (walker427 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 429; recurses to walker428.
walker429 :: Core.Term -> Maybe Core.Term
walker429 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker428 arg) (\_ -> walker428 fun) (walker428 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker428 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker428 inner) (walker428 body))
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
-- | Term walker level 430; recurses to walker429.
walker430 :: Core.Term -> Maybe Core.Term
walker430 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker429 arg) (\_ -> walker429 fun) (walker429 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker429 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker429 inner) (walker429 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 431; recurses to walker430.
walker431 :: Core.Term -> Maybe Core.Term
walker431 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker430 arg) (\_ -> walker430 fun) (walker430 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker430 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker430 inner) (walker430 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 432; recurses to walker431.
walker432 :: Core.Term -> Maybe Core.Term
walker432 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker431 arg) (\_ -> walker431 fun) (walker431 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker431 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker431 inner) (walker431 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 433; recurses to walker432.
walker433 :: Core.Term -> Maybe Core.Term
walker433 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker432 arg) (\_ -> walker432 fun) (walker432 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker432 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker432 inner) (walker432 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 434; recurses to walker433.
walker434 :: Core.Term -> Maybe Core.Term
walker434 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker433 arg) (\_ -> walker433 fun) (walker433 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker433 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker433 inner) (walker433 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 435; recurses to walker434.
walker435 :: Core.Term -> Maybe Core.Term
walker435 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker434 arg) (\_ -> walker434 fun) (walker434 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker434 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker434 inner) (walker434 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 436; recurses to walker435.
walker436 :: Core.Term -> Maybe Core.Term
walker436 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker435 arg) (\_ -> walker435 fun) (walker435 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker435 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker435 inner) (walker435 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 437; recurses to walker436.
walker437 :: Core.Term -> Maybe Core.Term
walker437 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker436 arg) (\_ -> walker436 fun) (walker436 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker436 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker436 inner) (walker436 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 438; recurses to walker437.
walker438 :: Core.Term -> Maybe Core.Term
walker438 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker437 arg) (\_ -> walker437 fun) (walker437 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker437 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker437 inner) (walker437 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 439; recurses to walker438.
walker439 :: Core.Term -> Maybe Core.Term
walker439 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker438 arg) (\_ -> walker438 fun) (walker438 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker438 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker438 inner) (walker438 body))
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
-- | Term walker level 440; recurses to walker439.
walker440 :: Core.Term -> Maybe Core.Term
walker440 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker439 arg) (\_ -> walker439 fun) (walker439 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker439 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker439 inner) (walker439 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 441; recurses to walker440.
walker441 :: Core.Term -> Maybe Core.Term
walker441 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker440 arg) (\_ -> walker440 fun) (walker440 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker440 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker440 inner) (walker440 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 442; recurses to walker441.
walker442 :: Core.Term -> Maybe Core.Term
walker442 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker441 arg) (\_ -> walker441 fun) (walker441 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker441 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker441 inner) (walker441 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 443; recurses to walker442.
walker443 :: Core.Term -> Maybe Core.Term
walker443 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker442 arg) (\_ -> walker442 fun) (walker442 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker442 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker442 inner) (walker442 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 444; recurses to walker443.
walker444 :: Core.Term -> Maybe Core.Term
walker444 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker443 arg) (\_ -> walker443 fun) (walker443 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker443 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker443 inner) (walker443 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 445; recurses to walker444.
walker445 :: Core.Term -> Maybe Core.Term
walker445 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker444 arg) (\_ -> walker444 fun) (walker444 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker444 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker444 inner) (walker444 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 446; recurses to walker445.
walker446 :: Core.Term -> Maybe Core.Term
walker446 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker445 arg) (\_ -> walker445 fun) (walker445 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker445 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker445 inner) (walker445 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 447; recurses to walker446.
walker447 :: Core.Term -> Maybe Core.Term
walker447 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker446 arg) (\_ -> walker446 fun) (walker446 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker446 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker446 inner) (walker446 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 448; recurses to walker447.
walker448 :: Core.Term -> Maybe Core.Term
walker448 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker447 arg) (\_ -> walker447 fun) (walker447 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker447 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker447 inner) (walker447 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 449; recurses to walker448.
walker449 :: Core.Term -> Maybe Core.Term
walker449 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker448 arg) (\_ -> walker448 fun) (walker448 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker448 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker448 inner) (walker448 body))
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
-- | Term walker level 450; recurses to walker449.
walker450 :: Core.Term -> Maybe Core.Term
walker450 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker449 arg) (\_ -> walker449 fun) (walker449 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker449 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker449 inner) (walker449 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 451; recurses to walker450.
walker451 :: Core.Term -> Maybe Core.Term
walker451 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker450 arg) (\_ -> walker450 fun) (walker450 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker450 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker450 inner) (walker450 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 452; recurses to walker451.
walker452 :: Core.Term -> Maybe Core.Term
walker452 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker451 arg) (\_ -> walker451 fun) (walker451 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker451 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker451 inner) (walker451 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 453; recurses to walker452.
walker453 :: Core.Term -> Maybe Core.Term
walker453 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker452 arg) (\_ -> walker452 fun) (walker452 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker452 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker452 inner) (walker452 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 454; recurses to walker453.
walker454 :: Core.Term -> Maybe Core.Term
walker454 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker453 arg) (\_ -> walker453 fun) (walker453 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker453 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker453 inner) (walker453 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 455; recurses to walker454.
walker455 :: Core.Term -> Maybe Core.Term
walker455 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker454 arg) (\_ -> walker454 fun) (walker454 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker454 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker454 inner) (walker454 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 456; recurses to walker455.
walker456 :: Core.Term -> Maybe Core.Term
walker456 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker455 arg) (\_ -> walker455 fun) (walker455 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker455 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker455 inner) (walker455 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 457; recurses to walker456.
walker457 :: Core.Term -> Maybe Core.Term
walker457 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker456 arg) (\_ -> walker456 fun) (walker456 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker456 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker456 inner) (walker456 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 458; recurses to walker457.
walker458 :: Core.Term -> Maybe Core.Term
walker458 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker457 arg) (\_ -> walker457 fun) (walker457 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker457 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker457 inner) (walker457 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 459; recurses to walker458.
walker459 :: Core.Term -> Maybe Core.Term
walker459 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker458 arg) (\_ -> walker458 fun) (walker458 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker458 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker458 inner) (walker458 body))
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
-- | Term walker level 460; recurses to walker459.
walker460 :: Core.Term -> Maybe Core.Term
walker460 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker459 arg) (\_ -> walker459 fun) (walker459 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker459 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker459 inner) (walker459 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 461; recurses to walker460.
walker461 :: Core.Term -> Maybe Core.Term
walker461 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker460 arg) (\_ -> walker460 fun) (walker460 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker460 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker460 inner) (walker460 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 462; recurses to walker461.
walker462 :: Core.Term -> Maybe Core.Term
walker462 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker461 arg) (\_ -> walker461 fun) (walker461 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker461 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker461 inner) (walker461 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 463; recurses to walker462.
walker463 :: Core.Term -> Maybe Core.Term
walker463 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker462 arg) (\_ -> walker462 fun) (walker462 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker462 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker462 inner) (walker462 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 464; recurses to walker463.
walker464 :: Core.Term -> Maybe Core.Term
walker464 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker463 arg) (\_ -> walker463 fun) (walker463 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker463 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker463 inner) (walker463 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 465; recurses to walker464.
walker465 :: Core.Term -> Maybe Core.Term
walker465 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker464 arg) (\_ -> walker464 fun) (walker464 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker464 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker464 inner) (walker464 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 466; recurses to walker465.
walker466 :: Core.Term -> Maybe Core.Term
walker466 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker465 arg) (\_ -> walker465 fun) (walker465 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker465 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker465 inner) (walker465 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 467; recurses to walker466.
walker467 :: Core.Term -> Maybe Core.Term
walker467 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker466 arg) (\_ -> walker466 fun) (walker466 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker466 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker466 inner) (walker466 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 468; recurses to walker467.
walker468 :: Core.Term -> Maybe Core.Term
walker468 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker467 arg) (\_ -> walker467 fun) (walker467 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker467 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker467 inner) (walker467 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 469; recurses to walker468.
walker469 :: Core.Term -> Maybe Core.Term
walker469 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker468 arg) (\_ -> walker468 fun) (walker468 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker468 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker468 inner) (walker468 body))
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
-- | Term walker level 470; recurses to walker469.
walker470 :: Core.Term -> Maybe Core.Term
walker470 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker469 arg) (\_ -> walker469 fun) (walker469 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker469 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker469 inner) (walker469 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 471; recurses to walker470.
walker471 :: Core.Term -> Maybe Core.Term
walker471 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker470 arg) (\_ -> walker470 fun) (walker470 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker470 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker470 inner) (walker470 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 472; recurses to walker471.
walker472 :: Core.Term -> Maybe Core.Term
walker472 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker471 arg) (\_ -> walker471 fun) (walker471 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker471 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker471 inner) (walker471 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 473; recurses to walker472.
walker473 :: Core.Term -> Maybe Core.Term
walker473 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker472 arg) (\_ -> walker472 fun) (walker472 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker472 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker472 inner) (walker472 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 474; recurses to walker473.
walker474 :: Core.Term -> Maybe Core.Term
walker474 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker473 arg) (\_ -> walker473 fun) (walker473 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker473 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker473 inner) (walker473 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 475; recurses to walker474.
walker475 :: Core.Term -> Maybe Core.Term
walker475 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker474 arg) (\_ -> walker474 fun) (walker474 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker474 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker474 inner) (walker474 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 476; recurses to walker475.
walker476 :: Core.Term -> Maybe Core.Term
walker476 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker475 arg) (\_ -> walker475 fun) (walker475 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker475 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker475 inner) (walker475 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 477; recurses to walker476.
walker477 :: Core.Term -> Maybe Core.Term
walker477 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker476 arg) (\_ -> walker476 fun) (walker476 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker476 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker476 inner) (walker476 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 478; recurses to walker477.
walker478 :: Core.Term -> Maybe Core.Term
walker478 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker477 arg) (\_ -> walker477 fun) (walker477 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker477 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker477 inner) (walker477 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 479; recurses to walker478.
walker479 :: Core.Term -> Maybe Core.Term
walker479 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker478 arg) (\_ -> walker478 fun) (walker478 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker478 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker478 inner) (walker478 body))
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
-- | Term walker level 480; recurses to walker479.
walker480 :: Core.Term -> Maybe Core.Term
walker480 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker479 arg) (\_ -> walker479 fun) (walker479 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker479 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker479 inner) (walker479 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 481; recurses to walker480.
walker481 :: Core.Term -> Maybe Core.Term
walker481 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker480 arg) (\_ -> walker480 fun) (walker480 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker480 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker480 inner) (walker480 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 482; recurses to walker481.
walker482 :: Core.Term -> Maybe Core.Term
walker482 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker481 arg) (\_ -> walker481 fun) (walker481 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker481 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker481 inner) (walker481 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 483; recurses to walker482.
walker483 :: Core.Term -> Maybe Core.Term
walker483 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker482 arg) (\_ -> walker482 fun) (walker482 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker482 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker482 inner) (walker482 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 484; recurses to walker483.
walker484 :: Core.Term -> Maybe Core.Term
walker484 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker483 arg) (\_ -> walker483 fun) (walker483 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker483 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker483 inner) (walker483 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 485; recurses to walker484.
walker485 :: Core.Term -> Maybe Core.Term
walker485 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker484 arg) (\_ -> walker484 fun) (walker484 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker484 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker484 inner) (walker484 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 486; recurses to walker485.
walker486 :: Core.Term -> Maybe Core.Term
walker486 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker485 arg) (\_ -> walker485 fun) (walker485 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker485 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker485 inner) (walker485 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 487; recurses to walker486.
walker487 :: Core.Term -> Maybe Core.Term
walker487 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker486 arg) (\_ -> walker486 fun) (walker486 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker486 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker486 inner) (walker486 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 488; recurses to walker487.
walker488 :: Core.Term -> Maybe Core.Term
walker488 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker487 arg) (\_ -> walker487 fun) (walker487 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker487 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker487 inner) (walker487 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 489; recurses to walker488.
walker489 :: Core.Term -> Maybe Core.Term
walker489 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker488 arg) (\_ -> walker488 fun) (walker488 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker488 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker488 inner) (walker488 body))
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
-- | Term walker level 490; recurses to walker489.
walker490 :: Core.Term -> Maybe Core.Term
walker490 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker489 arg) (\_ -> walker489 fun) (walker489 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker489 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker489 inner) (walker489 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 491; recurses to walker490.
walker491 :: Core.Term -> Maybe Core.Term
walker491 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker490 arg) (\_ -> walker490 fun) (walker490 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker490 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker490 inner) (walker490 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 492; recurses to walker491.
walker492 :: Core.Term -> Maybe Core.Term
walker492 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker491 arg) (\_ -> walker491 fun) (walker491 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker491 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker491 inner) (walker491 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 493; recurses to walker492.
walker493 :: Core.Term -> Maybe Core.Term
walker493 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker492 arg) (\_ -> walker492 fun) (walker492 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker492 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker492 inner) (walker492 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 494; recurses to walker493.
walker494 :: Core.Term -> Maybe Core.Term
walker494 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker493 arg) (\_ -> walker493 fun) (walker493 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker493 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker493 inner) (walker493 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 495; recurses to walker494.
walker495 :: Core.Term -> Maybe Core.Term
walker495 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker494 arg) (\_ -> walker494 fun) (walker494 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker494 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker494 inner) (walker494 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 496; recurses to walker495.
walker496 :: Core.Term -> Maybe Core.Term
walker496 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker495 arg) (\_ -> walker495 fun) (walker495 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker495 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker495 inner) (walker495 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 497; recurses to walker496.
walker497 :: Core.Term -> Maybe Core.Term
walker497 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker496 arg) (\_ -> walker496 fun) (walker496 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker496 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker496 inner) (walker496 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 498; recurses to walker497.
walker498 :: Core.Term -> Maybe Core.Term
walker498 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker497 arg) (\_ -> walker497 fun) (walker497 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker497 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker497 inner) (walker497 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 499; recurses to walker498.
walker499 :: Core.Term -> Maybe Core.Term
walker499 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker498 arg) (\_ -> walker498 fun) (walker498 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker498 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker498 inner) (walker498 body))
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
-- | Term walker level 500; recurses to walker499.
walker500 :: Core.Term -> Maybe Core.Term
walker500 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker499 arg) (\_ -> walker499 fun) (walker499 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker499 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker499 inner) (walker499 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 501; recurses to walker500.
walker501 :: Core.Term -> Maybe Core.Term
walker501 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker500 arg) (\_ -> walker500 fun) (walker500 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker500 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker500 inner) (walker500 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 502; recurses to walker501.
walker502 :: Core.Term -> Maybe Core.Term
walker502 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker501 arg) (\_ -> walker501 fun) (walker501 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker501 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker501 inner) (walker501 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 503; recurses to walker502.
walker503 :: Core.Term -> Maybe Core.Term
walker503 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker502 arg) (\_ -> walker502 fun) (walker502 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker502 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker502 inner) (walker502 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 504; recurses to walker503.
walker504 :: Core.Term -> Maybe Core.Term
walker504 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker503 arg) (\_ -> walker503 fun) (walker503 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker503 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker503 inner) (walker503 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 505; recurses to walker504.
walker505 :: Core.Term -> Maybe Core.Term
walker505 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker504 arg) (\_ -> walker504 fun) (walker504 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker504 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker504 inner) (walker504 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 506; recurses to walker505.
walker506 :: Core.Term -> Maybe Core.Term
walker506 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker505 arg) (\_ -> walker505 fun) (walker505 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker505 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker505 inner) (walker505 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 507; recurses to walker506.
walker507 :: Core.Term -> Maybe Core.Term
walker507 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker506 arg) (\_ -> walker506 fun) (walker506 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker506 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker506 inner) (walker506 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 508; recurses to walker507.
walker508 :: Core.Term -> Maybe Core.Term
walker508 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker507 arg) (\_ -> walker507 fun) (walker507 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker507 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker507 inner) (walker507 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 509; recurses to walker508.
walker509 :: Core.Term -> Maybe Core.Term
walker509 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker508 arg) (\_ -> walker508 fun) (walker508 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker508 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker508 inner) (walker508 body))
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
-- | Term walker level 510; recurses to walker509.
walker510 :: Core.Term -> Maybe Core.Term
walker510 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker509 arg) (\_ -> walker509 fun) (walker509 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker509 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker509 inner) (walker509 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 511; recurses to walker510.
walker511 :: Core.Term -> Maybe Core.Term
walker511 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker510 arg) (\_ -> walker510 fun) (walker510 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker510 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker510 inner) (walker510 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 512; recurses to walker511.
walker512 :: Core.Term -> Maybe Core.Term
walker512 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker511 arg) (\_ -> walker511 fun) (walker511 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker511 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker511 inner) (walker511 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 513; recurses to walker512.
walker513 :: Core.Term -> Maybe Core.Term
walker513 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker512 arg) (\_ -> walker512 fun) (walker512 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker512 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker512 inner) (walker512 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 514; recurses to walker513.
walker514 :: Core.Term -> Maybe Core.Term
walker514 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker513 arg) (\_ -> walker513 fun) (walker513 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker513 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker513 inner) (walker513 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 515; recurses to walker514.
walker515 :: Core.Term -> Maybe Core.Term
walker515 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker514 arg) (\_ -> walker514 fun) (walker514 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker514 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker514 inner) (walker514 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 516; recurses to walker515.
walker516 :: Core.Term -> Maybe Core.Term
walker516 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker515 arg) (\_ -> walker515 fun) (walker515 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker515 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker515 inner) (walker515 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 517; recurses to walker516.
walker517 :: Core.Term -> Maybe Core.Term
walker517 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker516 arg) (\_ -> walker516 fun) (walker516 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker516 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker516 inner) (walker516 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 518; recurses to walker517.
walker518 :: Core.Term -> Maybe Core.Term
walker518 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker517 arg) (\_ -> walker517 fun) (walker517 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker517 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker517 inner) (walker517 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 519; recurses to walker518.
walker519 :: Core.Term -> Maybe Core.Term
walker519 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker518 arg) (\_ -> walker518 fun) (walker518 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker518 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker518 inner) (walker518 body))
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
-- | Term walker level 520; recurses to walker519.
walker520 :: Core.Term -> Maybe Core.Term
walker520 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker519 arg) (\_ -> walker519 fun) (walker519 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker519 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker519 inner) (walker519 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 521; recurses to walker520.
walker521 :: Core.Term -> Maybe Core.Term
walker521 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker520 arg) (\_ -> walker520 fun) (walker520 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker520 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker520 inner) (walker520 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 522; recurses to walker521.
walker522 :: Core.Term -> Maybe Core.Term
walker522 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker521 arg) (\_ -> walker521 fun) (walker521 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker521 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker521 inner) (walker521 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 523; recurses to walker522.
walker523 :: Core.Term -> Maybe Core.Term
walker523 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker522 arg) (\_ -> walker522 fun) (walker522 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker522 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker522 inner) (walker522 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 524; recurses to walker523.
walker524 :: Core.Term -> Maybe Core.Term
walker524 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker523 arg) (\_ -> walker523 fun) (walker523 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker523 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker523 inner) (walker523 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 525; recurses to walker524.
walker525 :: Core.Term -> Maybe Core.Term
walker525 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker524 arg) (\_ -> walker524 fun) (walker524 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker524 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker524 inner) (walker524 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 526; recurses to walker525.
walker526 :: Core.Term -> Maybe Core.Term
walker526 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker525 arg) (\_ -> walker525 fun) (walker525 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker525 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker525 inner) (walker525 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 527; recurses to walker526.
walker527 :: Core.Term -> Maybe Core.Term
walker527 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker526 arg) (\_ -> walker526 fun) (walker526 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker526 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker526 inner) (walker526 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 528; recurses to walker527.
walker528 :: Core.Term -> Maybe Core.Term
walker528 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker527 arg) (\_ -> walker527 fun) (walker527 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker527 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker527 inner) (walker527 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 529; recurses to walker528.
walker529 :: Core.Term -> Maybe Core.Term
walker529 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker528 arg) (\_ -> walker528 fun) (walker528 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker528 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker528 inner) (walker528 body))
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
-- | Term walker level 530; recurses to walker529.
walker530 :: Core.Term -> Maybe Core.Term
walker530 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker529 arg) (\_ -> walker529 fun) (walker529 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker529 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker529 inner) (walker529 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 531; recurses to walker530.
walker531 :: Core.Term -> Maybe Core.Term
walker531 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker530 arg) (\_ -> walker530 fun) (walker530 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker530 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker530 inner) (walker530 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 532; recurses to walker531.
walker532 :: Core.Term -> Maybe Core.Term
walker532 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker531 arg) (\_ -> walker531 fun) (walker531 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker531 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker531 inner) (walker531 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 533; recurses to walker532.
walker533 :: Core.Term -> Maybe Core.Term
walker533 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker532 arg) (\_ -> walker532 fun) (walker532 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker532 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker532 inner) (walker532 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 534; recurses to walker533.
walker534 :: Core.Term -> Maybe Core.Term
walker534 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker533 arg) (\_ -> walker533 fun) (walker533 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker533 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker533 inner) (walker533 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 535; recurses to walker534.
walker535 :: Core.Term -> Maybe Core.Term
walker535 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker534 arg) (\_ -> walker534 fun) (walker534 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker534 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker534 inner) (walker534 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 536; recurses to walker535.
walker536 :: Core.Term -> Maybe Core.Term
walker536 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker535 arg) (\_ -> walker535 fun) (walker535 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker535 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker535 inner) (walker535 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 537; recurses to walker536.
walker537 :: Core.Term -> Maybe Core.Term
walker537 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker536 arg) (\_ -> walker536 fun) (walker536 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker536 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker536 inner) (walker536 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 538; recurses to walker537.
walker538 :: Core.Term -> Maybe Core.Term
walker538 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker537 arg) (\_ -> walker537 fun) (walker537 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker537 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker537 inner) (walker537 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 539; recurses to walker538.
walker539 :: Core.Term -> Maybe Core.Term
walker539 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker538 arg) (\_ -> walker538 fun) (walker538 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker538 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker538 inner) (walker538 body))
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
-- | Term walker level 540; recurses to walker539.
walker540 :: Core.Term -> Maybe Core.Term
walker540 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker539 arg) (\_ -> walker539 fun) (walker539 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker539 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker539 inner) (walker539 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 541; recurses to walker540.
walker541 :: Core.Term -> Maybe Core.Term
walker541 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker540 arg) (\_ -> walker540 fun) (walker540 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker540 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker540 inner) (walker540 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 542; recurses to walker541.
walker542 :: Core.Term -> Maybe Core.Term
walker542 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker541 arg) (\_ -> walker541 fun) (walker541 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker541 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker541 inner) (walker541 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 543; recurses to walker542.
walker543 :: Core.Term -> Maybe Core.Term
walker543 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker542 arg) (\_ -> walker542 fun) (walker542 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker542 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker542 inner) (walker542 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 544; recurses to walker543.
walker544 :: Core.Term -> Maybe Core.Term
walker544 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker543 arg) (\_ -> walker543 fun) (walker543 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker543 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker543 inner) (walker543 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 545; recurses to walker544.
walker545 :: Core.Term -> Maybe Core.Term
walker545 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker544 arg) (\_ -> walker544 fun) (walker544 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker544 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker544 inner) (walker544 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 546; recurses to walker545.
walker546 :: Core.Term -> Maybe Core.Term
walker546 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker545 arg) (\_ -> walker545 fun) (walker545 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker545 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker545 inner) (walker545 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 547; recurses to walker546.
walker547 :: Core.Term -> Maybe Core.Term
walker547 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker546 arg) (\_ -> walker546 fun) (walker546 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker546 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker546 inner) (walker546 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 548; recurses to walker547.
walker548 :: Core.Term -> Maybe Core.Term
walker548 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker547 arg) (\_ -> walker547 fun) (walker547 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker547 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker547 inner) (walker547 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 549; recurses to walker548.
walker549 :: Core.Term -> Maybe Core.Term
walker549 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker548 arg) (\_ -> walker548 fun) (walker548 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker548 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker548 inner) (walker548 body))
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
-- | Term walker level 550; recurses to walker549.
walker550 :: Core.Term -> Maybe Core.Term
walker550 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker549 arg) (\_ -> walker549 fun) (walker549 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker549 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker549 inner) (walker549 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 551; recurses to walker550.
walker551 :: Core.Term -> Maybe Core.Term
walker551 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker550 arg) (\_ -> walker550 fun) (walker550 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker550 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker550 inner) (walker550 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 552; recurses to walker551.
walker552 :: Core.Term -> Maybe Core.Term
walker552 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker551 arg) (\_ -> walker551 fun) (walker551 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker551 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker551 inner) (walker551 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 553; recurses to walker552.
walker553 :: Core.Term -> Maybe Core.Term
walker553 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker552 arg) (\_ -> walker552 fun) (walker552 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker552 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker552 inner) (walker552 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 554; recurses to walker553.
walker554 :: Core.Term -> Maybe Core.Term
walker554 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker553 arg) (\_ -> walker553 fun) (walker553 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker553 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker553 inner) (walker553 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 555; recurses to walker554.
walker555 :: Core.Term -> Maybe Core.Term
walker555 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker554 arg) (\_ -> walker554 fun) (walker554 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker554 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker554 inner) (walker554 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 556; recurses to walker555.
walker556 :: Core.Term -> Maybe Core.Term
walker556 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker555 arg) (\_ -> walker555 fun) (walker555 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker555 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker555 inner) (walker555 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 557; recurses to walker556.
walker557 :: Core.Term -> Maybe Core.Term
walker557 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker556 arg) (\_ -> walker556 fun) (walker556 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker556 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker556 inner) (walker556 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 558; recurses to walker557.
walker558 :: Core.Term -> Maybe Core.Term
walker558 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker557 arg) (\_ -> walker557 fun) (walker557 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker557 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker557 inner) (walker557 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 559; recurses to walker558.
walker559 :: Core.Term -> Maybe Core.Term
walker559 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker558 arg) (\_ -> walker558 fun) (walker558 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker558 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker558 inner) (walker558 body))
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
-- | Term walker level 560; recurses to walker559.
walker560 :: Core.Term -> Maybe Core.Term
walker560 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker559 arg) (\_ -> walker559 fun) (walker559 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker559 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker559 inner) (walker559 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 561; recurses to walker560.
walker561 :: Core.Term -> Maybe Core.Term
walker561 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker560 arg) (\_ -> walker560 fun) (walker560 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker560 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker560 inner) (walker560 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 562; recurses to walker561.
walker562 :: Core.Term -> Maybe Core.Term
walker562 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker561 arg) (\_ -> walker561 fun) (walker561 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker561 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker561 inner) (walker561 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 563; recurses to walker562.
walker563 :: Core.Term -> Maybe Core.Term
walker563 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker562 arg) (\_ -> walker562 fun) (walker562 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker562 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker562 inner) (walker562 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 564; recurses to walker563.
walker564 :: Core.Term -> Maybe Core.Term
walker564 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker563 arg) (\_ -> walker563 fun) (walker563 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker563 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker563 inner) (walker563 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 565; recurses to walker564.
walker565 :: Core.Term -> Maybe Core.Term
walker565 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker564 arg) (\_ -> walker564 fun) (walker564 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker564 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker564 inner) (walker564 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 566; recurses to walker565.
walker566 :: Core.Term -> Maybe Core.Term
walker566 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker565 arg) (\_ -> walker565 fun) (walker565 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker565 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker565 inner) (walker565 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 567; recurses to walker566.
walker567 :: Core.Term -> Maybe Core.Term
walker567 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker566 arg) (\_ -> walker566 fun) (walker566 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker566 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker566 inner) (walker566 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 568; recurses to walker567.
walker568 :: Core.Term -> Maybe Core.Term
walker568 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker567 arg) (\_ -> walker567 fun) (walker567 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker567 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker567 inner) (walker567 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 569; recurses to walker568.
walker569 :: Core.Term -> Maybe Core.Term
walker569 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker568 arg) (\_ -> walker568 fun) (walker568 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker568 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker568 inner) (walker568 body))
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
-- | Term walker level 570; recurses to walker569.
walker570 :: Core.Term -> Maybe Core.Term
walker570 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker569 arg) (\_ -> walker569 fun) (walker569 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker569 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker569 inner) (walker569 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 571; recurses to walker570.
walker571 :: Core.Term -> Maybe Core.Term
walker571 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker570 arg) (\_ -> walker570 fun) (walker570 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker570 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker570 inner) (walker570 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 572; recurses to walker571.
walker572 :: Core.Term -> Maybe Core.Term
walker572 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker571 arg) (\_ -> walker571 fun) (walker571 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker571 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker571 inner) (walker571 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 573; recurses to walker572.
walker573 :: Core.Term -> Maybe Core.Term
walker573 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker572 arg) (\_ -> walker572 fun) (walker572 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker572 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker572 inner) (walker572 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 574; recurses to walker573.
walker574 :: Core.Term -> Maybe Core.Term
walker574 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker573 arg) (\_ -> walker573 fun) (walker573 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker573 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker573 inner) (walker573 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 575; recurses to walker574.
walker575 :: Core.Term -> Maybe Core.Term
walker575 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker574 arg) (\_ -> walker574 fun) (walker574 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker574 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker574 inner) (walker574 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 576; recurses to walker575.
walker576 :: Core.Term -> Maybe Core.Term
walker576 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker575 arg) (\_ -> walker575 fun) (walker575 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker575 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker575 inner) (walker575 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 577; recurses to walker576.
walker577 :: Core.Term -> Maybe Core.Term
walker577 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker576 arg) (\_ -> walker576 fun) (walker576 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker576 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker576 inner) (walker576 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 578; recurses to walker577.
walker578 :: Core.Term -> Maybe Core.Term
walker578 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker577 arg) (\_ -> walker577 fun) (walker577 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker577 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker577 inner) (walker577 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 579; recurses to walker578.
walker579 :: Core.Term -> Maybe Core.Term
walker579 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker578 arg) (\_ -> walker578 fun) (walker578 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker578 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker578 inner) (walker578 body))
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
-- | Term walker level 580; recurses to walker579.
walker580 :: Core.Term -> Maybe Core.Term
walker580 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker579 arg) (\_ -> walker579 fun) (walker579 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker579 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker579 inner) (walker579 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 581; recurses to walker580.
walker581 :: Core.Term -> Maybe Core.Term
walker581 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker580 arg) (\_ -> walker580 fun) (walker580 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker580 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker580 inner) (walker580 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 582; recurses to walker581.
walker582 :: Core.Term -> Maybe Core.Term
walker582 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker581 arg) (\_ -> walker581 fun) (walker581 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker581 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker581 inner) (walker581 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 583; recurses to walker582.
walker583 :: Core.Term -> Maybe Core.Term
walker583 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker582 arg) (\_ -> walker582 fun) (walker582 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker582 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker582 inner) (walker582 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 584; recurses to walker583.
walker584 :: Core.Term -> Maybe Core.Term
walker584 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker583 arg) (\_ -> walker583 fun) (walker583 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker583 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker583 inner) (walker583 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 585; recurses to walker584.
walker585 :: Core.Term -> Maybe Core.Term
walker585 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker584 arg) (\_ -> walker584 fun) (walker584 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker584 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker584 inner) (walker584 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 586; recurses to walker585.
walker586 :: Core.Term -> Maybe Core.Term
walker586 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker585 arg) (\_ -> walker585 fun) (walker585 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker585 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker585 inner) (walker585 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 587; recurses to walker586.
walker587 :: Core.Term -> Maybe Core.Term
walker587 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker586 arg) (\_ -> walker586 fun) (walker586 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker586 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker586 inner) (walker586 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 588; recurses to walker587.
walker588 :: Core.Term -> Maybe Core.Term
walker588 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker587 arg) (\_ -> walker587 fun) (walker587 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker587 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker587 inner) (walker587 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 589; recurses to walker588.
walker589 :: Core.Term -> Maybe Core.Term
walker589 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker588 arg) (\_ -> walker588 fun) (walker588 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker588 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker588 inner) (walker588 body))
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
-- | Term walker level 590; recurses to walker589.
walker590 :: Core.Term -> Maybe Core.Term
walker590 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker589 arg) (\_ -> walker589 fun) (walker589 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker589 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker589 inner) (walker589 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 591; recurses to walker590.
walker591 :: Core.Term -> Maybe Core.Term
walker591 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker590 arg) (\_ -> walker590 fun) (walker590 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker590 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker590 inner) (walker590 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 592; recurses to walker591.
walker592 :: Core.Term -> Maybe Core.Term
walker592 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker591 arg) (\_ -> walker591 fun) (walker591 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker591 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker591 inner) (walker591 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 593; recurses to walker592.
walker593 :: Core.Term -> Maybe Core.Term
walker593 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker592 arg) (\_ -> walker592 fun) (walker592 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker592 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker592 inner) (walker592 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 594; recurses to walker593.
walker594 :: Core.Term -> Maybe Core.Term
walker594 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker593 arg) (\_ -> walker593 fun) (walker593 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker593 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker593 inner) (walker593 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 595; recurses to walker594.
walker595 :: Core.Term -> Maybe Core.Term
walker595 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker594 arg) (\_ -> walker594 fun) (walker594 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker594 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker594 inner) (walker594 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 596; recurses to walker595.
walker596 :: Core.Term -> Maybe Core.Term
walker596 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker595 arg) (\_ -> walker595 fun) (walker595 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker595 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker595 inner) (walker595 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 597; recurses to walker596.
walker597 :: Core.Term -> Maybe Core.Term
walker597 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker596 arg) (\_ -> walker596 fun) (walker596 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker596 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker596 inner) (walker596 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 598; recurses to walker597.
walker598 :: Core.Term -> Maybe Core.Term
walker598 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker597 arg) (\_ -> walker597 fun) (walker597 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker597 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker597 inner) (walker597 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 599; recurses to walker598.
walker599 :: Core.Term -> Maybe Core.Term
walker599 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker598 arg) (\_ -> walker598 fun) (walker598 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker598 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker598 inner) (walker598 body))
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
-- | Term walker level 600; recurses to walker599.
walker600 :: Core.Term -> Maybe Core.Term
walker600 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker599 arg) (\_ -> walker599 fun) (walker599 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker599 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker599 inner) (walker599 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 601; recurses to walker600.
walker601 :: Core.Term -> Maybe Core.Term
walker601 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker600 arg) (\_ -> walker600 fun) (walker600 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker600 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker600 inner) (walker600 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 602; recurses to walker601.
walker602 :: Core.Term -> Maybe Core.Term
walker602 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker601 arg) (\_ -> walker601 fun) (walker601 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker601 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker601 inner) (walker601 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 603; recurses to walker602.
walker603 :: Core.Term -> Maybe Core.Term
walker603 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker602 arg) (\_ -> walker602 fun) (walker602 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker602 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker602 inner) (walker602 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 604; recurses to walker603.
walker604 :: Core.Term -> Maybe Core.Term
walker604 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker603 arg) (\_ -> walker603 fun) (walker603 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker603 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker603 inner) (walker603 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 605; recurses to walker604.
walker605 :: Core.Term -> Maybe Core.Term
walker605 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker604 arg) (\_ -> walker604 fun) (walker604 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker604 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker604 inner) (walker604 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 606; recurses to walker605.
walker606 :: Core.Term -> Maybe Core.Term
walker606 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker605 arg) (\_ -> walker605 fun) (walker605 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker605 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker605 inner) (walker605 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 607; recurses to walker606.
walker607 :: Core.Term -> Maybe Core.Term
walker607 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker606 arg) (\_ -> walker606 fun) (walker606 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker606 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker606 inner) (walker606 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 608; recurses to walker607.
walker608 :: Core.Term -> Maybe Core.Term
walker608 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker607 arg) (\_ -> walker607 fun) (walker607 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker607 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker607 inner) (walker607 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 609; recurses to walker608.
walker609 :: Core.Term -> Maybe Core.Term
walker609 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker608 arg) (\_ -> walker608 fun) (walker608 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker608 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker608 inner) (walker608 body))
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
-- | Term walker level 610; recurses to walker609.
walker610 :: Core.Term -> Maybe Core.Term
walker610 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker609 arg) (\_ -> walker609 fun) (walker609 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker609 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker609 inner) (walker609 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 611; recurses to walker610.
walker611 :: Core.Term -> Maybe Core.Term
walker611 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker610 arg) (\_ -> walker610 fun) (walker610 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker610 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker610 inner) (walker610 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 612; recurses to walker611.
walker612 :: Core.Term -> Maybe Core.Term
walker612 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker611 arg) (\_ -> walker611 fun) (walker611 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker611 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker611 inner) (walker611 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 613; recurses to walker612.
walker613 :: Core.Term -> Maybe Core.Term
walker613 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker612 arg) (\_ -> walker612 fun) (walker612 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker612 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker612 inner) (walker612 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 614; recurses to walker613.
walker614 :: Core.Term -> Maybe Core.Term
walker614 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker613 arg) (\_ -> walker613 fun) (walker613 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker613 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker613 inner) (walker613 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 615; recurses to walker614.
walker615 :: Core.Term -> Maybe Core.Term
walker615 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker614 arg) (\_ -> walker614 fun) (walker614 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker614 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker614 inner) (walker614 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 616; recurses to walker615.
walker616 :: Core.Term -> Maybe Core.Term
walker616 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker615 arg) (\_ -> walker615 fun) (walker615 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker615 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker615 inner) (walker615 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 617; recurses to walker616.
walker617 :: Core.Term -> Maybe Core.Term
walker617 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker616 arg) (\_ -> walker616 fun) (walker616 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker616 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker616 inner) (walker616 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 618; recurses to walker617.
walker618 :: Core.Term -> Maybe Core.Term
walker618 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker617 arg) (\_ -> walker617 fun) (walker617 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker617 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker617 inner) (walker617 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 619; recurses to walker618.
walker619 :: Core.Term -> Maybe Core.Term
walker619 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker618 arg) (\_ -> walker618 fun) (walker618 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker618 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker618 inner) (walker618 body))
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
-- | Term walker level 620; recurses to walker619.
walker620 :: Core.Term -> Maybe Core.Term
walker620 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker619 arg) (\_ -> walker619 fun) (walker619 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker619 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker619 inner) (walker619 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 621; recurses to walker620.
walker621 :: Core.Term -> Maybe Core.Term
walker621 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker620 arg) (\_ -> walker620 fun) (walker620 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker620 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker620 inner) (walker620 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 622; recurses to walker621.
walker622 :: Core.Term -> Maybe Core.Term
walker622 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker621 arg) (\_ -> walker621 fun) (walker621 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker621 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker621 inner) (walker621 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 623; recurses to walker622.
walker623 :: Core.Term -> Maybe Core.Term
walker623 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker622 arg) (\_ -> walker622 fun) (walker622 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker622 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker622 inner) (walker622 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 624; recurses to walker623.
walker624 :: Core.Term -> Maybe Core.Term
walker624 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker623 arg) (\_ -> walker623 fun) (walker623 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker623 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker623 inner) (walker623 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 625; recurses to walker624.
walker625 :: Core.Term -> Maybe Core.Term
walker625 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker624 arg) (\_ -> walker624 fun) (walker624 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker624 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker624 inner) (walker624 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 626; recurses to walker625.
walker626 :: Core.Term -> Maybe Core.Term
walker626 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker625 arg) (\_ -> walker625 fun) (walker625 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker625 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker625 inner) (walker625 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 627; recurses to walker626.
walker627 :: Core.Term -> Maybe Core.Term
walker627 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker626 arg) (\_ -> walker626 fun) (walker626 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker626 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker626 inner) (walker626 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 628; recurses to walker627.
walker628 :: Core.Term -> Maybe Core.Term
walker628 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker627 arg) (\_ -> walker627 fun) (walker627 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker627 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker627 inner) (walker627 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 629; recurses to walker628.
walker629 :: Core.Term -> Maybe Core.Term
walker629 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker628 arg) (\_ -> walker628 fun) (walker628 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker628 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker628 inner) (walker628 body))
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
-- | Term walker level 630; recurses to walker629.
walker630 :: Core.Term -> Maybe Core.Term
walker630 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker629 arg) (\_ -> walker629 fun) (walker629 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker629 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker629 inner) (walker629 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 631; recurses to walker630.
walker631 :: Core.Term -> Maybe Core.Term
walker631 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker630 arg) (\_ -> walker630 fun) (walker630 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker630 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker630 inner) (walker630 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 632; recurses to walker631.
walker632 :: Core.Term -> Maybe Core.Term
walker632 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker631 arg) (\_ -> walker631 fun) (walker631 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker631 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker631 inner) (walker631 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 633; recurses to walker632.
walker633 :: Core.Term -> Maybe Core.Term
walker633 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker632 arg) (\_ -> walker632 fun) (walker632 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker632 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker632 inner) (walker632 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 634; recurses to walker633.
walker634 :: Core.Term -> Maybe Core.Term
walker634 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker633 arg) (\_ -> walker633 fun) (walker633 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker633 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker633 inner) (walker633 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 635; recurses to walker634.
walker635 :: Core.Term -> Maybe Core.Term
walker635 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker634 arg) (\_ -> walker634 fun) (walker634 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker634 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker634 inner) (walker634 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 636; recurses to walker635.
walker636 :: Core.Term -> Maybe Core.Term
walker636 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker635 arg) (\_ -> walker635 fun) (walker635 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker635 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker635 inner) (walker635 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 637; recurses to walker636.
walker637 :: Core.Term -> Maybe Core.Term
walker637 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker636 arg) (\_ -> walker636 fun) (walker636 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker636 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker636 inner) (walker636 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 638; recurses to walker637.
walker638 :: Core.Term -> Maybe Core.Term
walker638 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker637 arg) (\_ -> walker637 fun) (walker637 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker637 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker637 inner) (walker637 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 639; recurses to walker638.
walker639 :: Core.Term -> Maybe Core.Term
walker639 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker638 arg) (\_ -> walker638 fun) (walker638 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker638 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker638 inner) (walker638 body))
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
-- | Term walker level 640; recurses to walker639.
walker640 :: Core.Term -> Maybe Core.Term
walker640 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker639 arg) (\_ -> walker639 fun) (walker639 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker639 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker639 inner) (walker639 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 641; recurses to walker640.
walker641 :: Core.Term -> Maybe Core.Term
walker641 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker640 arg) (\_ -> walker640 fun) (walker640 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker640 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker640 inner) (walker640 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 642; recurses to walker641.
walker642 :: Core.Term -> Maybe Core.Term
walker642 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker641 arg) (\_ -> walker641 fun) (walker641 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker641 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker641 inner) (walker641 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 643; recurses to walker642.
walker643 :: Core.Term -> Maybe Core.Term
walker643 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker642 arg) (\_ -> walker642 fun) (walker642 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker642 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker642 inner) (walker642 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 644; recurses to walker643.
walker644 :: Core.Term -> Maybe Core.Term
walker644 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker643 arg) (\_ -> walker643 fun) (walker643 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker643 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker643 inner) (walker643 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 645; recurses to walker644.
walker645 :: Core.Term -> Maybe Core.Term
walker645 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker644 arg) (\_ -> walker644 fun) (walker644 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker644 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker644 inner) (walker644 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 646; recurses to walker645.
walker646 :: Core.Term -> Maybe Core.Term
walker646 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker645 arg) (\_ -> walker645 fun) (walker645 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker645 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker645 inner) (walker645 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 647; recurses to walker646.
walker647 :: Core.Term -> Maybe Core.Term
walker647 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker646 arg) (\_ -> walker646 fun) (walker646 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker646 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker646 inner) (walker646 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 648; recurses to walker647.
walker648 :: Core.Term -> Maybe Core.Term
walker648 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker647 arg) (\_ -> walker647 fun) (walker647 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker647 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker647 inner) (walker647 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 649; recurses to walker648.
walker649 :: Core.Term -> Maybe Core.Term
walker649 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker648 arg) (\_ -> walker648 fun) (walker648 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker648 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker648 inner) (walker648 body))
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
-- | Term walker level 650; recurses to walker649.
walker650 :: Core.Term -> Maybe Core.Term
walker650 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker649 arg) (\_ -> walker649 fun) (walker649 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker649 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker649 inner) (walker649 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 651; recurses to walker650.
walker651 :: Core.Term -> Maybe Core.Term
walker651 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker650 arg) (\_ -> walker650 fun) (walker650 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker650 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker650 inner) (walker650 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 652; recurses to walker651.
walker652 :: Core.Term -> Maybe Core.Term
walker652 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker651 arg) (\_ -> walker651 fun) (walker651 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker651 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker651 inner) (walker651 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 653; recurses to walker652.
walker653 :: Core.Term -> Maybe Core.Term
walker653 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker652 arg) (\_ -> walker652 fun) (walker652 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker652 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker652 inner) (walker652 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 654; recurses to walker653.
walker654 :: Core.Term -> Maybe Core.Term
walker654 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker653 arg) (\_ -> walker653 fun) (walker653 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker653 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker653 inner) (walker653 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 655; recurses to walker654.
walker655 :: Core.Term -> Maybe Core.Term
walker655 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker654 arg) (\_ -> walker654 fun) (walker654 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker654 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker654 inner) (walker654 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 656; recurses to walker655.
walker656 :: Core.Term -> Maybe Core.Term
walker656 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker655 arg) (\_ -> walker655 fun) (walker655 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker655 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker655 inner) (walker655 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 657; recurses to walker656.
walker657 :: Core.Term -> Maybe Core.Term
walker657 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker656 arg) (\_ -> walker656 fun) (walker656 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker656 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker656 inner) (walker656 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 658; recurses to walker657.
walker658 :: Core.Term -> Maybe Core.Term
walker658 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker657 arg) (\_ -> walker657 fun) (walker657 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker657 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker657 inner) (walker657 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 659; recurses to walker658.
walker659 :: Core.Term -> Maybe Core.Term
walker659 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker658 arg) (\_ -> walker658 fun) (walker658 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker658 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker658 inner) (walker658 body))
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
-- | Term walker level 660; recurses to walker659.
walker660 :: Core.Term -> Maybe Core.Term
walker660 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker659 arg) (\_ -> walker659 fun) (walker659 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker659 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker659 inner) (walker659 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 661; recurses to walker660.
walker661 :: Core.Term -> Maybe Core.Term
walker661 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker660 arg) (\_ -> walker660 fun) (walker660 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker660 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker660 inner) (walker660 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 662; recurses to walker661.
walker662 :: Core.Term -> Maybe Core.Term
walker662 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker661 arg) (\_ -> walker661 fun) (walker661 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker661 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker661 inner) (walker661 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 663; recurses to walker662.
walker663 :: Core.Term -> Maybe Core.Term
walker663 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker662 arg) (\_ -> walker662 fun) (walker662 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker662 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker662 inner) (walker662 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 664; recurses to walker663.
walker664 :: Core.Term -> Maybe Core.Term
walker664 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker663 arg) (\_ -> walker663 fun) (walker663 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker663 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker663 inner) (walker663 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 665; recurses to walker664.
walker665 :: Core.Term -> Maybe Core.Term
walker665 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker664 arg) (\_ -> walker664 fun) (walker664 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker664 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker664 inner) (walker664 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 666; recurses to walker665.
walker666 :: Core.Term -> Maybe Core.Term
walker666 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker665 arg) (\_ -> walker665 fun) (walker665 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker665 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker665 inner) (walker665 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 667; recurses to walker666.
walker667 :: Core.Term -> Maybe Core.Term
walker667 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker666 arg) (\_ -> walker666 fun) (walker666 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker666 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker666 inner) (walker666 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 668; recurses to walker667.
walker668 :: Core.Term -> Maybe Core.Term
walker668 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker667 arg) (\_ -> walker667 fun) (walker667 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker667 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker667 inner) (walker667 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 669; recurses to walker668.
walker669 :: Core.Term -> Maybe Core.Term
walker669 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker668 arg) (\_ -> walker668 fun) (walker668 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker668 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker668 inner) (walker668 body))
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
-- | Term walker level 670; recurses to walker669.
walker670 :: Core.Term -> Maybe Core.Term
walker670 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker669 arg) (\_ -> walker669 fun) (walker669 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker669 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker669 inner) (walker669 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 671; recurses to walker670.
walker671 :: Core.Term -> Maybe Core.Term
walker671 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker670 arg) (\_ -> walker670 fun) (walker670 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker670 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker670 inner) (walker670 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 672; recurses to walker671.
walker672 :: Core.Term -> Maybe Core.Term
walker672 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker671 arg) (\_ -> walker671 fun) (walker671 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker671 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker671 inner) (walker671 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 673; recurses to walker672.
walker673 :: Core.Term -> Maybe Core.Term
walker673 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker672 arg) (\_ -> walker672 fun) (walker672 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker672 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker672 inner) (walker672 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 674; recurses to walker673.
walker674 :: Core.Term -> Maybe Core.Term
walker674 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker673 arg) (\_ -> walker673 fun) (walker673 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker673 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker673 inner) (walker673 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 675; recurses to walker674.
walker675 :: Core.Term -> Maybe Core.Term
walker675 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker674 arg) (\_ -> walker674 fun) (walker674 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker674 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker674 inner) (walker674 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 676; recurses to walker675.
walker676 :: Core.Term -> Maybe Core.Term
walker676 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker675 arg) (\_ -> walker675 fun) (walker675 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker675 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker675 inner) (walker675 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 677; recurses to walker676.
walker677 :: Core.Term -> Maybe Core.Term
walker677 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker676 arg) (\_ -> walker676 fun) (walker676 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker676 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker676 inner) (walker676 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 678; recurses to walker677.
walker678 :: Core.Term -> Maybe Core.Term
walker678 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker677 arg) (\_ -> walker677 fun) (walker677 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker677 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker677 inner) (walker677 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 679; recurses to walker678.
walker679 :: Core.Term -> Maybe Core.Term
walker679 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker678 arg) (\_ -> walker678 fun) (walker678 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker678 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker678 inner) (walker678 body))
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
-- | Term walker level 680; recurses to walker679.
walker680 :: Core.Term -> Maybe Core.Term
walker680 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker679 arg) (\_ -> walker679 fun) (walker679 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker679 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker679 inner) (walker679 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 681; recurses to walker680.
walker681 :: Core.Term -> Maybe Core.Term
walker681 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker680 arg) (\_ -> walker680 fun) (walker680 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker680 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker680 inner) (walker680 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 682; recurses to walker681.
walker682 :: Core.Term -> Maybe Core.Term
walker682 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker681 arg) (\_ -> walker681 fun) (walker681 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker681 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker681 inner) (walker681 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 683; recurses to walker682.
walker683 :: Core.Term -> Maybe Core.Term
walker683 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker682 arg) (\_ -> walker682 fun) (walker682 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker682 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker682 inner) (walker682 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 684; recurses to walker683.
walker684 :: Core.Term -> Maybe Core.Term
walker684 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker683 arg) (\_ -> walker683 fun) (walker683 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker683 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker683 inner) (walker683 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 685; recurses to walker684.
walker685 :: Core.Term -> Maybe Core.Term
walker685 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker684 arg) (\_ -> walker684 fun) (walker684 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker684 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker684 inner) (walker684 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 686; recurses to walker685.
walker686 :: Core.Term -> Maybe Core.Term
walker686 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker685 arg) (\_ -> walker685 fun) (walker685 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker685 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker685 inner) (walker685 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 687; recurses to walker686.
walker687 :: Core.Term -> Maybe Core.Term
walker687 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker686 arg) (\_ -> walker686 fun) (walker686 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker686 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker686 inner) (walker686 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 688; recurses to walker687.
walker688 :: Core.Term -> Maybe Core.Term
walker688 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker687 arg) (\_ -> walker687 fun) (walker687 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker687 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker687 inner) (walker687 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 689; recurses to walker688.
walker689 :: Core.Term -> Maybe Core.Term
walker689 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker688 arg) (\_ -> walker688 fun) (walker688 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker688 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker688 inner) (walker688 body))
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
-- | Term walker level 690; recurses to walker689.
walker690 :: Core.Term -> Maybe Core.Term
walker690 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker689 arg) (\_ -> walker689 fun) (walker689 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker689 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker689 inner) (walker689 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 691; recurses to walker690.
walker691 :: Core.Term -> Maybe Core.Term
walker691 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker690 arg) (\_ -> walker690 fun) (walker690 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker690 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker690 inner) (walker690 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 692; recurses to walker691.
walker692 :: Core.Term -> Maybe Core.Term
walker692 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker691 arg) (\_ -> walker691 fun) (walker691 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker691 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker691 inner) (walker691 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 693; recurses to walker692.
walker693 :: Core.Term -> Maybe Core.Term
walker693 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker692 arg) (\_ -> walker692 fun) (walker692 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker692 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker692 inner) (walker692 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 694; recurses to walker693.
walker694 :: Core.Term -> Maybe Core.Term
walker694 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker693 arg) (\_ -> walker693 fun) (walker693 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker693 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker693 inner) (walker693 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 695; recurses to walker694.
walker695 :: Core.Term -> Maybe Core.Term
walker695 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker694 arg) (\_ -> walker694 fun) (walker694 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker694 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker694 inner) (walker694 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 696; recurses to walker695.
walker696 :: Core.Term -> Maybe Core.Term
walker696 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker695 arg) (\_ -> walker695 fun) (walker695 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker695 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker695 inner) (walker695 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 697; recurses to walker696.
walker697 :: Core.Term -> Maybe Core.Term
walker697 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker696 arg) (\_ -> walker696 fun) (walker696 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker696 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker696 inner) (walker696 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 698; recurses to walker697.
walker698 :: Core.Term -> Maybe Core.Term
walker698 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker697 arg) (\_ -> walker697 fun) (walker697 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker697 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker697 inner) (walker697 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 699; recurses to walker698.
walker699 :: Core.Term -> Maybe Core.Term
walker699 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker698 arg) (\_ -> walker698 fun) (walker698 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker698 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker698 inner) (walker698 body))
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
-- | Term walker level 700; recurses to walker699.
walker700 :: Core.Term -> Maybe Core.Term
walker700 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker699 arg) (\_ -> walker699 fun) (walker699 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker699 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker699 inner) (walker699 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 701; recurses to walker700.
walker701 :: Core.Term -> Maybe Core.Term
walker701 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker700 arg) (\_ -> walker700 fun) (walker700 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker700 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker700 inner) (walker700 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 702; recurses to walker701.
walker702 :: Core.Term -> Maybe Core.Term
walker702 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker701 arg) (\_ -> walker701 fun) (walker701 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker701 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker701 inner) (walker701 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 703; recurses to walker702.
walker703 :: Core.Term -> Maybe Core.Term
walker703 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker702 arg) (\_ -> walker702 fun) (walker702 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker702 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker702 inner) (walker702 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 704; recurses to walker703.
walker704 :: Core.Term -> Maybe Core.Term
walker704 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker703 arg) (\_ -> walker703 fun) (walker703 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker703 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker703 inner) (walker703 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 705; recurses to walker704.
walker705 :: Core.Term -> Maybe Core.Term
walker705 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker704 arg) (\_ -> walker704 fun) (walker704 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker704 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker704 inner) (walker704 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 706; recurses to walker705.
walker706 :: Core.Term -> Maybe Core.Term
walker706 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker705 arg) (\_ -> walker705 fun) (walker705 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker705 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker705 inner) (walker705 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 707; recurses to walker706.
walker707 :: Core.Term -> Maybe Core.Term
walker707 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker706 arg) (\_ -> walker706 fun) (walker706 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker706 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker706 inner) (walker706 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 708; recurses to walker707.
walker708 :: Core.Term -> Maybe Core.Term
walker708 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker707 arg) (\_ -> walker707 fun) (walker707 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker707 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker707 inner) (walker707 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 709; recurses to walker708.
walker709 :: Core.Term -> Maybe Core.Term
walker709 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker708 arg) (\_ -> walker708 fun) (walker708 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker708 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker708 inner) (walker708 body))
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
-- | Term walker level 710; recurses to walker709.
walker710 :: Core.Term -> Maybe Core.Term
walker710 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker709 arg) (\_ -> walker709 fun) (walker709 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker709 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker709 inner) (walker709 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 711; recurses to walker710.
walker711 :: Core.Term -> Maybe Core.Term
walker711 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker710 arg) (\_ -> walker710 fun) (walker710 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker710 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker710 inner) (walker710 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 712; recurses to walker711.
walker712 :: Core.Term -> Maybe Core.Term
walker712 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker711 arg) (\_ -> walker711 fun) (walker711 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker711 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker711 inner) (walker711 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 713; recurses to walker712.
walker713 :: Core.Term -> Maybe Core.Term
walker713 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker712 arg) (\_ -> walker712 fun) (walker712 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker712 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker712 inner) (walker712 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 714; recurses to walker713.
walker714 :: Core.Term -> Maybe Core.Term
walker714 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker713 arg) (\_ -> walker713 fun) (walker713 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker713 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker713 inner) (walker713 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 715; recurses to walker714.
walker715 :: Core.Term -> Maybe Core.Term
walker715 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker714 arg) (\_ -> walker714 fun) (walker714 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker714 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker714 inner) (walker714 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 716; recurses to walker715.
walker716 :: Core.Term -> Maybe Core.Term
walker716 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker715 arg) (\_ -> walker715 fun) (walker715 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker715 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker715 inner) (walker715 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 717; recurses to walker716.
walker717 :: Core.Term -> Maybe Core.Term
walker717 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker716 arg) (\_ -> walker716 fun) (walker716 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker716 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker716 inner) (walker716 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 718; recurses to walker717.
walker718 :: Core.Term -> Maybe Core.Term
walker718 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker717 arg) (\_ -> walker717 fun) (walker717 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker717 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker717 inner) (walker717 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 719; recurses to walker718.
walker719 :: Core.Term -> Maybe Core.Term
walker719 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker718 arg) (\_ -> walker718 fun) (walker718 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker718 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker718 inner) (walker718 body))
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
-- | Term walker level 720; recurses to walker719.
walker720 :: Core.Term -> Maybe Core.Term
walker720 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker719 arg) (\_ -> walker719 fun) (walker719 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker719 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker719 inner) (walker719 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 721; recurses to walker720.
walker721 :: Core.Term -> Maybe Core.Term
walker721 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker720 arg) (\_ -> walker720 fun) (walker720 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker720 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker720 inner) (walker720 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 722; recurses to walker721.
walker722 :: Core.Term -> Maybe Core.Term
walker722 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker721 arg) (\_ -> walker721 fun) (walker721 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker721 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker721 inner) (walker721 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 723; recurses to walker722.
walker723 :: Core.Term -> Maybe Core.Term
walker723 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker722 arg) (\_ -> walker722 fun) (walker722 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker722 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker722 inner) (walker722 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 724; recurses to walker723.
walker724 :: Core.Term -> Maybe Core.Term
walker724 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker723 arg) (\_ -> walker723 fun) (walker723 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker723 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker723 inner) (walker723 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 725; recurses to walker724.
walker725 :: Core.Term -> Maybe Core.Term
walker725 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker724 arg) (\_ -> walker724 fun) (walker724 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker724 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker724 inner) (walker724 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 726; recurses to walker725.
walker726 :: Core.Term -> Maybe Core.Term
walker726 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker725 arg) (\_ -> walker725 fun) (walker725 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker725 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker725 inner) (walker725 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 727; recurses to walker726.
walker727 :: Core.Term -> Maybe Core.Term
walker727 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker726 arg) (\_ -> walker726 fun) (walker726 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker726 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker726 inner) (walker726 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 728; recurses to walker727.
walker728 :: Core.Term -> Maybe Core.Term
walker728 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker727 arg) (\_ -> walker727 fun) (walker727 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker727 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker727 inner) (walker727 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 729; recurses to walker728.
walker729 :: Core.Term -> Maybe Core.Term
walker729 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker728 arg) (\_ -> walker728 fun) (walker728 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker728 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker728 inner) (walker728 body))
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
-- | Term walker level 730; recurses to walker729.
walker730 :: Core.Term -> Maybe Core.Term
walker730 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker729 arg) (\_ -> walker729 fun) (walker729 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker729 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker729 inner) (walker729 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 731; recurses to walker730.
walker731 :: Core.Term -> Maybe Core.Term
walker731 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker730 arg) (\_ -> walker730 fun) (walker730 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker730 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker730 inner) (walker730 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 732; recurses to walker731.
walker732 :: Core.Term -> Maybe Core.Term
walker732 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker731 arg) (\_ -> walker731 fun) (walker731 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker731 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker731 inner) (walker731 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 733; recurses to walker732.
walker733 :: Core.Term -> Maybe Core.Term
walker733 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker732 arg) (\_ -> walker732 fun) (walker732 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker732 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker732 inner) (walker732 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 734; recurses to walker733.
walker734 :: Core.Term -> Maybe Core.Term
walker734 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker733 arg) (\_ -> walker733 fun) (walker733 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker733 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker733 inner) (walker733 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 735; recurses to walker734.
walker735 :: Core.Term -> Maybe Core.Term
walker735 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker734 arg) (\_ -> walker734 fun) (walker734 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker734 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker734 inner) (walker734 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 736; recurses to walker735.
walker736 :: Core.Term -> Maybe Core.Term
walker736 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker735 arg) (\_ -> walker735 fun) (walker735 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker735 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker735 inner) (walker735 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 737; recurses to walker736.
walker737 :: Core.Term -> Maybe Core.Term
walker737 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker736 arg) (\_ -> walker736 fun) (walker736 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker736 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker736 inner) (walker736 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 738; recurses to walker737.
walker738 :: Core.Term -> Maybe Core.Term
walker738 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker737 arg) (\_ -> walker737 fun) (walker737 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker737 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker737 inner) (walker737 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 739; recurses to walker738.
walker739 :: Core.Term -> Maybe Core.Term
walker739 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker738 arg) (\_ -> walker738 fun) (walker738 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker738 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker738 inner) (walker738 body))
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
-- | Term walker level 740; recurses to walker739.
walker740 :: Core.Term -> Maybe Core.Term
walker740 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker739 arg) (\_ -> walker739 fun) (walker739 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker739 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker739 inner) (walker739 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 741; recurses to walker740.
walker741 :: Core.Term -> Maybe Core.Term
walker741 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker740 arg) (\_ -> walker740 fun) (walker740 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker740 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker740 inner) (walker740 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 742; recurses to walker741.
walker742 :: Core.Term -> Maybe Core.Term
walker742 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker741 arg) (\_ -> walker741 fun) (walker741 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker741 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker741 inner) (walker741 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 743; recurses to walker742.
walker743 :: Core.Term -> Maybe Core.Term
walker743 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker742 arg) (\_ -> walker742 fun) (walker742 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker742 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker742 inner) (walker742 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 744; recurses to walker743.
walker744 :: Core.Term -> Maybe Core.Term
walker744 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker743 arg) (\_ -> walker743 fun) (walker743 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker743 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker743 inner) (walker743 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 745; recurses to walker744.
walker745 :: Core.Term -> Maybe Core.Term
walker745 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker744 arg) (\_ -> walker744 fun) (walker744 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker744 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker744 inner) (walker744 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 746; recurses to walker745.
walker746 :: Core.Term -> Maybe Core.Term
walker746 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker745 arg) (\_ -> walker745 fun) (walker745 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker745 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker745 inner) (walker745 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 747; recurses to walker746.
walker747 :: Core.Term -> Maybe Core.Term
walker747 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker746 arg) (\_ -> walker746 fun) (walker746 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker746 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker746 inner) (walker746 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 748; recurses to walker747.
walker748 :: Core.Term -> Maybe Core.Term
walker748 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker747 arg) (\_ -> walker747 fun) (walker747 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker747 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker747 inner) (walker747 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 749; recurses to walker748.
walker749 :: Core.Term -> Maybe Core.Term
walker749 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker748 arg) (\_ -> walker748 fun) (walker748 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker748 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker748 inner) (walker748 body))
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
-- | Term walker level 750; recurses to walker749.
walker750 :: Core.Term -> Maybe Core.Term
walker750 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker749 arg) (\_ -> walker749 fun) (walker749 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker749 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker749 inner) (walker749 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 751; recurses to walker750.
walker751 :: Core.Term -> Maybe Core.Term
walker751 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker750 arg) (\_ -> walker750 fun) (walker750 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker750 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker750 inner) (walker750 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 752; recurses to walker751.
walker752 :: Core.Term -> Maybe Core.Term
walker752 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker751 arg) (\_ -> walker751 fun) (walker751 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker751 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker751 inner) (walker751 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 753; recurses to walker752.
walker753 :: Core.Term -> Maybe Core.Term
walker753 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker752 arg) (\_ -> walker752 fun) (walker752 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker752 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker752 inner) (walker752 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 754; recurses to walker753.
walker754 :: Core.Term -> Maybe Core.Term
walker754 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker753 arg) (\_ -> walker753 fun) (walker753 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker753 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker753 inner) (walker753 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 755; recurses to walker754.
walker755 :: Core.Term -> Maybe Core.Term
walker755 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker754 arg) (\_ -> walker754 fun) (walker754 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker754 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker754 inner) (walker754 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 756; recurses to walker755.
walker756 :: Core.Term -> Maybe Core.Term
walker756 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker755 arg) (\_ -> walker755 fun) (walker755 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker755 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker755 inner) (walker755 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 757; recurses to walker756.
walker757 :: Core.Term -> Maybe Core.Term
walker757 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker756 arg) (\_ -> walker756 fun) (walker756 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker756 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker756 inner) (walker756 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 758; recurses to walker757.
walker758 :: Core.Term -> Maybe Core.Term
walker758 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker757 arg) (\_ -> walker757 fun) (walker757 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker757 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker757 inner) (walker757 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 759; recurses to walker758.
walker759 :: Core.Term -> Maybe Core.Term
walker759 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker758 arg) (\_ -> walker758 fun) (walker758 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker758 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker758 inner) (walker758 body))
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
-- | Term walker level 760; recurses to walker759.
walker760 :: Core.Term -> Maybe Core.Term
walker760 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker759 arg) (\_ -> walker759 fun) (walker759 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker759 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker759 inner) (walker759 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 761; recurses to walker760.
walker761 :: Core.Term -> Maybe Core.Term
walker761 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker760 arg) (\_ -> walker760 fun) (walker760 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker760 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker760 inner) (walker760 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 762; recurses to walker761.
walker762 :: Core.Term -> Maybe Core.Term
walker762 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker761 arg) (\_ -> walker761 fun) (walker761 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker761 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker761 inner) (walker761 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 763; recurses to walker762.
walker763 :: Core.Term -> Maybe Core.Term
walker763 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker762 arg) (\_ -> walker762 fun) (walker762 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker762 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker762 inner) (walker762 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 764; recurses to walker763.
walker764 :: Core.Term -> Maybe Core.Term
walker764 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker763 arg) (\_ -> walker763 fun) (walker763 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker763 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker763 inner) (walker763 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 765; recurses to walker764.
walker765 :: Core.Term -> Maybe Core.Term
walker765 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker764 arg) (\_ -> walker764 fun) (walker764 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker764 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker764 inner) (walker764 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 766; recurses to walker765.
walker766 :: Core.Term -> Maybe Core.Term
walker766 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker765 arg) (\_ -> walker765 fun) (walker765 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker765 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker765 inner) (walker765 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 767; recurses to walker766.
walker767 :: Core.Term -> Maybe Core.Term
walker767 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker766 arg) (\_ -> walker766 fun) (walker766 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker766 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker766 inner) (walker766 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 768; recurses to walker767.
walker768 :: Core.Term -> Maybe Core.Term
walker768 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker767 arg) (\_ -> walker767 fun) (walker767 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker767 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker767 inner) (walker767 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 769; recurses to walker768.
walker769 :: Core.Term -> Maybe Core.Term
walker769 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker768 arg) (\_ -> walker768 fun) (walker768 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker768 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker768 inner) (walker768 body))
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
-- | Term walker level 770; recurses to walker769.
walker770 :: Core.Term -> Maybe Core.Term
walker770 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker769 arg) (\_ -> walker769 fun) (walker769 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker769 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker769 inner) (walker769 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 771; recurses to walker770.
walker771 :: Core.Term -> Maybe Core.Term
walker771 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker770 arg) (\_ -> walker770 fun) (walker770 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker770 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker770 inner) (walker770 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 772; recurses to walker771.
walker772 :: Core.Term -> Maybe Core.Term
walker772 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker771 arg) (\_ -> walker771 fun) (walker771 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker771 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker771 inner) (walker771 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 773; recurses to walker772.
walker773 :: Core.Term -> Maybe Core.Term
walker773 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker772 arg) (\_ -> walker772 fun) (walker772 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker772 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker772 inner) (walker772 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 774; recurses to walker773.
walker774 :: Core.Term -> Maybe Core.Term
walker774 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker773 arg) (\_ -> walker773 fun) (walker773 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker773 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker773 inner) (walker773 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 775; recurses to walker774.
walker775 :: Core.Term -> Maybe Core.Term
walker775 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker774 arg) (\_ -> walker774 fun) (walker774 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker774 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker774 inner) (walker774 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 776; recurses to walker775.
walker776 :: Core.Term -> Maybe Core.Term
walker776 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker775 arg) (\_ -> walker775 fun) (walker775 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker775 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker775 inner) (walker775 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 777; recurses to walker776.
walker777 :: Core.Term -> Maybe Core.Term
walker777 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker776 arg) (\_ -> walker776 fun) (walker776 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker776 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker776 inner) (walker776 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 778; recurses to walker777.
walker778 :: Core.Term -> Maybe Core.Term
walker778 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker777 arg) (\_ -> walker777 fun) (walker777 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker777 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker777 inner) (walker777 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 779; recurses to walker778.
walker779 :: Core.Term -> Maybe Core.Term
walker779 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker778 arg) (\_ -> walker778 fun) (walker778 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker778 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker778 inner) (walker778 body))
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
-- | Term walker level 780; recurses to walker779.
walker780 :: Core.Term -> Maybe Core.Term
walker780 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker779 arg) (\_ -> walker779 fun) (walker779 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker779 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker779 inner) (walker779 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 781; recurses to walker780.
walker781 :: Core.Term -> Maybe Core.Term
walker781 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker780 arg) (\_ -> walker780 fun) (walker780 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker780 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker780 inner) (walker780 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 782; recurses to walker781.
walker782 :: Core.Term -> Maybe Core.Term
walker782 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker781 arg) (\_ -> walker781 fun) (walker781 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker781 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker781 inner) (walker781 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 783; recurses to walker782.
walker783 :: Core.Term -> Maybe Core.Term
walker783 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker782 arg) (\_ -> walker782 fun) (walker782 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker782 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker782 inner) (walker782 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 784; recurses to walker783.
walker784 :: Core.Term -> Maybe Core.Term
walker784 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker783 arg) (\_ -> walker783 fun) (walker783 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker783 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker783 inner) (walker783 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 785; recurses to walker784.
walker785 :: Core.Term -> Maybe Core.Term
walker785 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker784 arg) (\_ -> walker784 fun) (walker784 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker784 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker784 inner) (walker784 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 786; recurses to walker785.
walker786 :: Core.Term -> Maybe Core.Term
walker786 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker785 arg) (\_ -> walker785 fun) (walker785 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker785 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker785 inner) (walker785 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 787; recurses to walker786.
walker787 :: Core.Term -> Maybe Core.Term
walker787 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker786 arg) (\_ -> walker786 fun) (walker786 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker786 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker786 inner) (walker786 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 788; recurses to walker787.
walker788 :: Core.Term -> Maybe Core.Term
walker788 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker787 arg) (\_ -> walker787 fun) (walker787 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker787 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker787 inner) (walker787 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 789; recurses to walker788.
walker789 :: Core.Term -> Maybe Core.Term
walker789 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker788 arg) (\_ -> walker788 fun) (walker788 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker788 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker788 inner) (walker788 body))
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
-- | Term walker level 790; recurses to walker789.
walker790 :: Core.Term -> Maybe Core.Term
walker790 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker789 arg) (\_ -> walker789 fun) (walker789 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker789 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker789 inner) (walker789 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 791; recurses to walker790.
walker791 :: Core.Term -> Maybe Core.Term
walker791 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker790 arg) (\_ -> walker790 fun) (walker790 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker790 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker790 inner) (walker790 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 792; recurses to walker791.
walker792 :: Core.Term -> Maybe Core.Term
walker792 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker791 arg) (\_ -> walker791 fun) (walker791 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker791 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker791 inner) (walker791 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 793; recurses to walker792.
walker793 :: Core.Term -> Maybe Core.Term
walker793 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker792 arg) (\_ -> walker792 fun) (walker792 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker792 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker792 inner) (walker792 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 794; recurses to walker793.
walker794 :: Core.Term -> Maybe Core.Term
walker794 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker793 arg) (\_ -> walker793 fun) (walker793 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker793 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker793 inner) (walker793 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 795; recurses to walker794.
walker795 :: Core.Term -> Maybe Core.Term
walker795 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker794 arg) (\_ -> walker794 fun) (walker794 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker794 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker794 inner) (walker794 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 796; recurses to walker795.
walker796 :: Core.Term -> Maybe Core.Term
walker796 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker795 arg) (\_ -> walker795 fun) (walker795 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker795 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker795 inner) (walker795 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 797; recurses to walker796.
walker797 :: Core.Term -> Maybe Core.Term
walker797 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker796 arg) (\_ -> walker796 fun) (walker796 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker796 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker796 inner) (walker796 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 798; recurses to walker797.
walker798 :: Core.Term -> Maybe Core.Term
walker798 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker797 arg) (\_ -> walker797 fun) (walker797 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker797 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker797 inner) (walker797 body))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Term walker level 799; recurses to walker798.
walker799 :: Core.Term -> Maybe Core.Term
walker799 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Maybes.maybe (walker798 arg) (\_ -> walker798 fun) (walker798 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker798 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Maybes.maybe Nothing (\inner -> walker798 inner) (walker798 body))
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
