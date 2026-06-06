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
import qualified Hydra.Haskell.Lib.Optionals as Optionals
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
          in (Optionals.cases (walker0 fun) (walker0 arg) (\_ -> walker0 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker0 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker0 body) Nothing (\inner -> walker0 inner))
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
          in (Optionals.cases (walker9 fun) (walker9 arg) (\_ -> walker9 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker9 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker9 body) Nothing (\inner -> walker9 inner))
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
          in (Optionals.cases (walker99 fun) (walker99 arg) (\_ -> walker99 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker99 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker99 body) Nothing (\inner -> walker99 inner))
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
          in (Optionals.cases (walker100 fun) (walker100 arg) (\_ -> walker100 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker100 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker100 body) Nothing (\inner -> walker100 inner))
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
          in (Optionals.cases (walker101 fun) (walker101 arg) (\_ -> walker101 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker101 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker101 body) Nothing (\inner -> walker101 inner))
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
          in (Optionals.cases (walker102 fun) (walker102 arg) (\_ -> walker102 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker102 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker102 body) Nothing (\inner -> walker102 inner))
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
          in (Optionals.cases (walker103 fun) (walker103 arg) (\_ -> walker103 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker103 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker103 body) Nothing (\inner -> walker103 inner))
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
          in (Optionals.cases (walker104 fun) (walker104 arg) (\_ -> walker104 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker104 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker104 body) Nothing (\inner -> walker104 inner))
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
          in (Optionals.cases (walker105 fun) (walker105 arg) (\_ -> walker105 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker105 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker105 body) Nothing (\inner -> walker105 inner))
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
          in (Optionals.cases (walker106 fun) (walker106 arg) (\_ -> walker106 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker106 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker106 body) Nothing (\inner -> walker106 inner))
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
          in (Optionals.cases (walker107 fun) (walker107 arg) (\_ -> walker107 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker107 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker107 body) Nothing (\inner -> walker107 inner))
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
          in (Optionals.cases (walker108 fun) (walker108 arg) (\_ -> walker108 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker108 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker108 body) Nothing (\inner -> walker108 inner))
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
          in (Optionals.cases (walker10 fun) (walker10 arg) (\_ -> walker10 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker10 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker10 body) Nothing (\inner -> walker10 inner))
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
          in (Optionals.cases (walker109 fun) (walker109 arg) (\_ -> walker109 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker109 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker109 body) Nothing (\inner -> walker109 inner))
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
          in (Optionals.cases (walker110 fun) (walker110 arg) (\_ -> walker110 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker110 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker110 body) Nothing (\inner -> walker110 inner))
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
          in (Optionals.cases (walker111 fun) (walker111 arg) (\_ -> walker111 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker111 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker111 body) Nothing (\inner -> walker111 inner))
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
          in (Optionals.cases (walker112 fun) (walker112 arg) (\_ -> walker112 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker112 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker112 body) Nothing (\inner -> walker112 inner))
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
          in (Optionals.cases (walker113 fun) (walker113 arg) (\_ -> walker113 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker113 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker113 body) Nothing (\inner -> walker113 inner))
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
          in (Optionals.cases (walker114 fun) (walker114 arg) (\_ -> walker114 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker114 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker114 body) Nothing (\inner -> walker114 inner))
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
          in (Optionals.cases (walker115 fun) (walker115 arg) (\_ -> walker115 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker115 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker115 body) Nothing (\inner -> walker115 inner))
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
          in (Optionals.cases (walker116 fun) (walker116 arg) (\_ -> walker116 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker116 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker116 body) Nothing (\inner -> walker116 inner))
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
          in (Optionals.cases (walker117 fun) (walker117 arg) (\_ -> walker117 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker117 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker117 body) Nothing (\inner -> walker117 inner))
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
          in (Optionals.cases (walker118 fun) (walker118 arg) (\_ -> walker118 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker118 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker118 body) Nothing (\inner -> walker118 inner))
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
          in (Optionals.cases (walker11 fun) (walker11 arg) (\_ -> walker11 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker11 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker11 body) Nothing (\inner -> walker11 inner))
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
          in (Optionals.cases (walker119 fun) (walker119 arg) (\_ -> walker119 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker119 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker119 body) Nothing (\inner -> walker119 inner))
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
          in (Optionals.cases (walker120 fun) (walker120 arg) (\_ -> walker120 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker120 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker120 body) Nothing (\inner -> walker120 inner))
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
          in (Optionals.cases (walker121 fun) (walker121 arg) (\_ -> walker121 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker121 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker121 body) Nothing (\inner -> walker121 inner))
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
          in (Optionals.cases (walker122 fun) (walker122 arg) (\_ -> walker122 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker122 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker122 body) Nothing (\inner -> walker122 inner))
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
          in (Optionals.cases (walker123 fun) (walker123 arg) (\_ -> walker123 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker123 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker123 body) Nothing (\inner -> walker123 inner))
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
          in (Optionals.cases (walker124 fun) (walker124 arg) (\_ -> walker124 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker124 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker124 body) Nothing (\inner -> walker124 inner))
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
          in (Optionals.cases (walker125 fun) (walker125 arg) (\_ -> walker125 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker125 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker125 body) Nothing (\inner -> walker125 inner))
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
          in (Optionals.cases (walker126 fun) (walker126 arg) (\_ -> walker126 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker126 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker126 body) Nothing (\inner -> walker126 inner))
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
          in (Optionals.cases (walker127 fun) (walker127 arg) (\_ -> walker127 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker127 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker127 body) Nothing (\inner -> walker127 inner))
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
          in (Optionals.cases (walker128 fun) (walker128 arg) (\_ -> walker128 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker128 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker128 body) Nothing (\inner -> walker128 inner))
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
          in (Optionals.cases (walker12 fun) (walker12 arg) (\_ -> walker12 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker12 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker12 body) Nothing (\inner -> walker12 inner))
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
          in (Optionals.cases (walker129 fun) (walker129 arg) (\_ -> walker129 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker129 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker129 body) Nothing (\inner -> walker129 inner))
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
          in (Optionals.cases (walker130 fun) (walker130 arg) (\_ -> walker130 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker130 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker130 body) Nothing (\inner -> walker130 inner))
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
          in (Optionals.cases (walker131 fun) (walker131 arg) (\_ -> walker131 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker131 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker131 body) Nothing (\inner -> walker131 inner))
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
          in (Optionals.cases (walker132 fun) (walker132 arg) (\_ -> walker132 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker132 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker132 body) Nothing (\inner -> walker132 inner))
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
          in (Optionals.cases (walker133 fun) (walker133 arg) (\_ -> walker133 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker133 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker133 body) Nothing (\inner -> walker133 inner))
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
          in (Optionals.cases (walker134 fun) (walker134 arg) (\_ -> walker134 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker134 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker134 body) Nothing (\inner -> walker134 inner))
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
          in (Optionals.cases (walker135 fun) (walker135 arg) (\_ -> walker135 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker135 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker135 body) Nothing (\inner -> walker135 inner))
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
          in (Optionals.cases (walker136 fun) (walker136 arg) (\_ -> walker136 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker136 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker136 body) Nothing (\inner -> walker136 inner))
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
          in (Optionals.cases (walker137 fun) (walker137 arg) (\_ -> walker137 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker137 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker137 body) Nothing (\inner -> walker137 inner))
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
          in (Optionals.cases (walker138 fun) (walker138 arg) (\_ -> walker138 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker138 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker138 body) Nothing (\inner -> walker138 inner))
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
          in (Optionals.cases (walker13 fun) (walker13 arg) (\_ -> walker13 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker13 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker13 body) Nothing (\inner -> walker13 inner))
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
          in (Optionals.cases (walker139 fun) (walker139 arg) (\_ -> walker139 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker139 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker139 body) Nothing (\inner -> walker139 inner))
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
          in (Optionals.cases (walker140 fun) (walker140 arg) (\_ -> walker140 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker140 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker140 body) Nothing (\inner -> walker140 inner))
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
          in (Optionals.cases (walker141 fun) (walker141 arg) (\_ -> walker141 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker141 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker141 body) Nothing (\inner -> walker141 inner))
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
          in (Optionals.cases (walker142 fun) (walker142 arg) (\_ -> walker142 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker142 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker142 body) Nothing (\inner -> walker142 inner))
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
          in (Optionals.cases (walker143 fun) (walker143 arg) (\_ -> walker143 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker143 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker143 body) Nothing (\inner -> walker143 inner))
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
          in (Optionals.cases (walker144 fun) (walker144 arg) (\_ -> walker144 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker144 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker144 body) Nothing (\inner -> walker144 inner))
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
          in (Optionals.cases (walker145 fun) (walker145 arg) (\_ -> walker145 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker145 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker145 body) Nothing (\inner -> walker145 inner))
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
          in (Optionals.cases (walker146 fun) (walker146 arg) (\_ -> walker146 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker146 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker146 body) Nothing (\inner -> walker146 inner))
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
          in (Optionals.cases (walker147 fun) (walker147 arg) (\_ -> walker147 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker147 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker147 body) Nothing (\inner -> walker147 inner))
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
          in (Optionals.cases (walker148 fun) (walker148 arg) (\_ -> walker148 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker148 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker148 body) Nothing (\inner -> walker148 inner))
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
          in (Optionals.cases (walker14 fun) (walker14 arg) (\_ -> walker14 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker14 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker14 body) Nothing (\inner -> walker14 inner))
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
          in (Optionals.cases (walker149 fun) (walker149 arg) (\_ -> walker149 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker149 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker149 body) Nothing (\inner -> walker149 inner))
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
          in (Optionals.cases (walker150 fun) (walker150 arg) (\_ -> walker150 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker150 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker150 body) Nothing (\inner -> walker150 inner))
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
          in (Optionals.cases (walker151 fun) (walker151 arg) (\_ -> walker151 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker151 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker151 body) Nothing (\inner -> walker151 inner))
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
          in (Optionals.cases (walker152 fun) (walker152 arg) (\_ -> walker152 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker152 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker152 body) Nothing (\inner -> walker152 inner))
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
          in (Optionals.cases (walker153 fun) (walker153 arg) (\_ -> walker153 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker153 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker153 body) Nothing (\inner -> walker153 inner))
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
          in (Optionals.cases (walker154 fun) (walker154 arg) (\_ -> walker154 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker154 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker154 body) Nothing (\inner -> walker154 inner))
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
          in (Optionals.cases (walker155 fun) (walker155 arg) (\_ -> walker155 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker155 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker155 body) Nothing (\inner -> walker155 inner))
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
          in (Optionals.cases (walker156 fun) (walker156 arg) (\_ -> walker156 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker156 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker156 body) Nothing (\inner -> walker156 inner))
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
          in (Optionals.cases (walker157 fun) (walker157 arg) (\_ -> walker157 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker157 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker157 body) Nothing (\inner -> walker157 inner))
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
          in (Optionals.cases (walker158 fun) (walker158 arg) (\_ -> walker158 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker158 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker158 body) Nothing (\inner -> walker158 inner))
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
          in (Optionals.cases (walker15 fun) (walker15 arg) (\_ -> walker15 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker15 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker15 body) Nothing (\inner -> walker15 inner))
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
          in (Optionals.cases (walker159 fun) (walker159 arg) (\_ -> walker159 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker159 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker159 body) Nothing (\inner -> walker159 inner))
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
          in (Optionals.cases (walker160 fun) (walker160 arg) (\_ -> walker160 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker160 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker160 body) Nothing (\inner -> walker160 inner))
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
          in (Optionals.cases (walker161 fun) (walker161 arg) (\_ -> walker161 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker161 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker161 body) Nothing (\inner -> walker161 inner))
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
          in (Optionals.cases (walker162 fun) (walker162 arg) (\_ -> walker162 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker162 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker162 body) Nothing (\inner -> walker162 inner))
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
          in (Optionals.cases (walker163 fun) (walker163 arg) (\_ -> walker163 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker163 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker163 body) Nothing (\inner -> walker163 inner))
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
          in (Optionals.cases (walker164 fun) (walker164 arg) (\_ -> walker164 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker164 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker164 body) Nothing (\inner -> walker164 inner))
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
          in (Optionals.cases (walker165 fun) (walker165 arg) (\_ -> walker165 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker165 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker165 body) Nothing (\inner -> walker165 inner))
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
          in (Optionals.cases (walker166 fun) (walker166 arg) (\_ -> walker166 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker166 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker166 body) Nothing (\inner -> walker166 inner))
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
          in (Optionals.cases (walker167 fun) (walker167 arg) (\_ -> walker167 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker167 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker167 body) Nothing (\inner -> walker167 inner))
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
          in (Optionals.cases (walker168 fun) (walker168 arg) (\_ -> walker168 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker168 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker168 body) Nothing (\inner -> walker168 inner))
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
          in (Optionals.cases (walker16 fun) (walker16 arg) (\_ -> walker16 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker16 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker16 body) Nothing (\inner -> walker16 inner))
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
          in (Optionals.cases (walker169 fun) (walker169 arg) (\_ -> walker169 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker169 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker169 body) Nothing (\inner -> walker169 inner))
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
          in (Optionals.cases (walker170 fun) (walker170 arg) (\_ -> walker170 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker170 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker170 body) Nothing (\inner -> walker170 inner))
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
          in (Optionals.cases (walker171 fun) (walker171 arg) (\_ -> walker171 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker171 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker171 body) Nothing (\inner -> walker171 inner))
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
          in (Optionals.cases (walker172 fun) (walker172 arg) (\_ -> walker172 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker172 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker172 body) Nothing (\inner -> walker172 inner))
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
          in (Optionals.cases (walker173 fun) (walker173 arg) (\_ -> walker173 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker173 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker173 body) Nothing (\inner -> walker173 inner))
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
          in (Optionals.cases (walker174 fun) (walker174 arg) (\_ -> walker174 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker174 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker174 body) Nothing (\inner -> walker174 inner))
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
          in (Optionals.cases (walker175 fun) (walker175 arg) (\_ -> walker175 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker175 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker175 body) Nothing (\inner -> walker175 inner))
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
          in (Optionals.cases (walker176 fun) (walker176 arg) (\_ -> walker176 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker176 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker176 body) Nothing (\inner -> walker176 inner))
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
          in (Optionals.cases (walker177 fun) (walker177 arg) (\_ -> walker177 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker177 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker177 body) Nothing (\inner -> walker177 inner))
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
          in (Optionals.cases (walker178 fun) (walker178 arg) (\_ -> walker178 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker178 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker178 body) Nothing (\inner -> walker178 inner))
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
          in (Optionals.cases (walker17 fun) (walker17 arg) (\_ -> walker17 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker17 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker17 body) Nothing (\inner -> walker17 inner))
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
          in (Optionals.cases (walker179 fun) (walker179 arg) (\_ -> walker179 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker179 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker179 body) Nothing (\inner -> walker179 inner))
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
          in (Optionals.cases (walker180 fun) (walker180 arg) (\_ -> walker180 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker180 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker180 body) Nothing (\inner -> walker180 inner))
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
          in (Optionals.cases (walker181 fun) (walker181 arg) (\_ -> walker181 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker181 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker181 body) Nothing (\inner -> walker181 inner))
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
          in (Optionals.cases (walker182 fun) (walker182 arg) (\_ -> walker182 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker182 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker182 body) Nothing (\inner -> walker182 inner))
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
          in (Optionals.cases (walker183 fun) (walker183 arg) (\_ -> walker183 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker183 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker183 body) Nothing (\inner -> walker183 inner))
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
          in (Optionals.cases (walker184 fun) (walker184 arg) (\_ -> walker184 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker184 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker184 body) Nothing (\inner -> walker184 inner))
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
          in (Optionals.cases (walker185 fun) (walker185 arg) (\_ -> walker185 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker185 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker185 body) Nothing (\inner -> walker185 inner))
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
          in (Optionals.cases (walker186 fun) (walker186 arg) (\_ -> walker186 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker186 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker186 body) Nothing (\inner -> walker186 inner))
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
          in (Optionals.cases (walker187 fun) (walker187 arg) (\_ -> walker187 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker187 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker187 body) Nothing (\inner -> walker187 inner))
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
          in (Optionals.cases (walker188 fun) (walker188 arg) (\_ -> walker188 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker188 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker188 body) Nothing (\inner -> walker188 inner))
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
          in (Optionals.cases (walker18 fun) (walker18 arg) (\_ -> walker18 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker18 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker18 body) Nothing (\inner -> walker18 inner))
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
          in (Optionals.cases (walker189 fun) (walker189 arg) (\_ -> walker189 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker189 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker189 body) Nothing (\inner -> walker189 inner))
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
          in (Optionals.cases (walker190 fun) (walker190 arg) (\_ -> walker190 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker190 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker190 body) Nothing (\inner -> walker190 inner))
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
          in (Optionals.cases (walker191 fun) (walker191 arg) (\_ -> walker191 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker191 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker191 body) Nothing (\inner -> walker191 inner))
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
          in (Optionals.cases (walker192 fun) (walker192 arg) (\_ -> walker192 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker192 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker192 body) Nothing (\inner -> walker192 inner))
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
          in (Optionals.cases (walker193 fun) (walker193 arg) (\_ -> walker193 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker193 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker193 body) Nothing (\inner -> walker193 inner))
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
          in (Optionals.cases (walker194 fun) (walker194 arg) (\_ -> walker194 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker194 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker194 body) Nothing (\inner -> walker194 inner))
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
          in (Optionals.cases (walker195 fun) (walker195 arg) (\_ -> walker195 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker195 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker195 body) Nothing (\inner -> walker195 inner))
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
          in (Optionals.cases (walker196 fun) (walker196 arg) (\_ -> walker196 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker196 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker196 body) Nothing (\inner -> walker196 inner))
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
          in (Optionals.cases (walker197 fun) (walker197 arg) (\_ -> walker197 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker197 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker197 body) Nothing (\inner -> walker197 inner))
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
          in (Optionals.cases (walker198 fun) (walker198 arg) (\_ -> walker198 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker198 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker198 body) Nothing (\inner -> walker198 inner))
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
          in (Optionals.cases (walker1 fun) (walker1 arg) (\_ -> walker1 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker1 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker1 body) Nothing (\inner -> walker1 inner))
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
          in (Optionals.cases (walker19 fun) (walker19 arg) (\_ -> walker19 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker19 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker19 body) Nothing (\inner -> walker19 inner))
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
          in (Optionals.cases (walker199 fun) (walker199 arg) (\_ -> walker199 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker199 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker199 body) Nothing (\inner -> walker199 inner))
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
          in (Optionals.cases (walker200 fun) (walker200 arg) (\_ -> walker200 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker200 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker200 body) Nothing (\inner -> walker200 inner))
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
          in (Optionals.cases (walker201 fun) (walker201 arg) (\_ -> walker201 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker201 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker201 body) Nothing (\inner -> walker201 inner))
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
          in (Optionals.cases (walker202 fun) (walker202 arg) (\_ -> walker202 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker202 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker202 body) Nothing (\inner -> walker202 inner))
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
          in (Optionals.cases (walker203 fun) (walker203 arg) (\_ -> walker203 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker203 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker203 body) Nothing (\inner -> walker203 inner))
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
          in (Optionals.cases (walker204 fun) (walker204 arg) (\_ -> walker204 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker204 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker204 body) Nothing (\inner -> walker204 inner))
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
          in (Optionals.cases (walker205 fun) (walker205 arg) (\_ -> walker205 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker205 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker205 body) Nothing (\inner -> walker205 inner))
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
          in (Optionals.cases (walker206 fun) (walker206 arg) (\_ -> walker206 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker206 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker206 body) Nothing (\inner -> walker206 inner))
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
          in (Optionals.cases (walker207 fun) (walker207 arg) (\_ -> walker207 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker207 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker207 body) Nothing (\inner -> walker207 inner))
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
          in (Optionals.cases (walker208 fun) (walker208 arg) (\_ -> walker208 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker208 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker208 body) Nothing (\inner -> walker208 inner))
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
          in (Optionals.cases (walker20 fun) (walker20 arg) (\_ -> walker20 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker20 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker20 body) Nothing (\inner -> walker20 inner))
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
          in (Optionals.cases (walker209 fun) (walker209 arg) (\_ -> walker209 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker209 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker209 body) Nothing (\inner -> walker209 inner))
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
          in (Optionals.cases (walker210 fun) (walker210 arg) (\_ -> walker210 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker210 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker210 body) Nothing (\inner -> walker210 inner))
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
          in (Optionals.cases (walker211 fun) (walker211 arg) (\_ -> walker211 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker211 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker211 body) Nothing (\inner -> walker211 inner))
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
          in (Optionals.cases (walker212 fun) (walker212 arg) (\_ -> walker212 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker212 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker212 body) Nothing (\inner -> walker212 inner))
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
          in (Optionals.cases (walker213 fun) (walker213 arg) (\_ -> walker213 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker213 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker213 body) Nothing (\inner -> walker213 inner))
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
          in (Optionals.cases (walker214 fun) (walker214 arg) (\_ -> walker214 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker214 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker214 body) Nothing (\inner -> walker214 inner))
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
          in (Optionals.cases (walker215 fun) (walker215 arg) (\_ -> walker215 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker215 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker215 body) Nothing (\inner -> walker215 inner))
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
          in (Optionals.cases (walker216 fun) (walker216 arg) (\_ -> walker216 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker216 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker216 body) Nothing (\inner -> walker216 inner))
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
          in (Optionals.cases (walker217 fun) (walker217 arg) (\_ -> walker217 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker217 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker217 body) Nothing (\inner -> walker217 inner))
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
          in (Optionals.cases (walker218 fun) (walker218 arg) (\_ -> walker218 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker218 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker218 body) Nothing (\inner -> walker218 inner))
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
          in (Optionals.cases (walker21 fun) (walker21 arg) (\_ -> walker21 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker21 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker21 body) Nothing (\inner -> walker21 inner))
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
          in (Optionals.cases (walker219 fun) (walker219 arg) (\_ -> walker219 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker219 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker219 body) Nothing (\inner -> walker219 inner))
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
          in (Optionals.cases (walker220 fun) (walker220 arg) (\_ -> walker220 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker220 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker220 body) Nothing (\inner -> walker220 inner))
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
          in (Optionals.cases (walker221 fun) (walker221 arg) (\_ -> walker221 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker221 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker221 body) Nothing (\inner -> walker221 inner))
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
          in (Optionals.cases (walker222 fun) (walker222 arg) (\_ -> walker222 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker222 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker222 body) Nothing (\inner -> walker222 inner))
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
          in (Optionals.cases (walker223 fun) (walker223 arg) (\_ -> walker223 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker223 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker223 body) Nothing (\inner -> walker223 inner))
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
          in (Optionals.cases (walker224 fun) (walker224 arg) (\_ -> walker224 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker224 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker224 body) Nothing (\inner -> walker224 inner))
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
          in (Optionals.cases (walker225 fun) (walker225 arg) (\_ -> walker225 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker225 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker225 body) Nothing (\inner -> walker225 inner))
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
          in (Optionals.cases (walker226 fun) (walker226 arg) (\_ -> walker226 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker226 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker226 body) Nothing (\inner -> walker226 inner))
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
          in (Optionals.cases (walker227 fun) (walker227 arg) (\_ -> walker227 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker227 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker227 body) Nothing (\inner -> walker227 inner))
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
          in (Optionals.cases (walker228 fun) (walker228 arg) (\_ -> walker228 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker228 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker228 body) Nothing (\inner -> walker228 inner))
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
          in (Optionals.cases (walker22 fun) (walker22 arg) (\_ -> walker22 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker22 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker22 body) Nothing (\inner -> walker22 inner))
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
          in (Optionals.cases (walker229 fun) (walker229 arg) (\_ -> walker229 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker229 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker229 body) Nothing (\inner -> walker229 inner))
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
          in (Optionals.cases (walker230 fun) (walker230 arg) (\_ -> walker230 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker230 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker230 body) Nothing (\inner -> walker230 inner))
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
          in (Optionals.cases (walker231 fun) (walker231 arg) (\_ -> walker231 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker231 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker231 body) Nothing (\inner -> walker231 inner))
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
          in (Optionals.cases (walker232 fun) (walker232 arg) (\_ -> walker232 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker232 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker232 body) Nothing (\inner -> walker232 inner))
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
          in (Optionals.cases (walker233 fun) (walker233 arg) (\_ -> walker233 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker233 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker233 body) Nothing (\inner -> walker233 inner))
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
          in (Optionals.cases (walker234 fun) (walker234 arg) (\_ -> walker234 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker234 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker234 body) Nothing (\inner -> walker234 inner))
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
          in (Optionals.cases (walker235 fun) (walker235 arg) (\_ -> walker235 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker235 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker235 body) Nothing (\inner -> walker235 inner))
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
          in (Optionals.cases (walker236 fun) (walker236 arg) (\_ -> walker236 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker236 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker236 body) Nothing (\inner -> walker236 inner))
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
          in (Optionals.cases (walker237 fun) (walker237 arg) (\_ -> walker237 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker237 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker237 body) Nothing (\inner -> walker237 inner))
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
          in (Optionals.cases (walker238 fun) (walker238 arg) (\_ -> walker238 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker238 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker238 body) Nothing (\inner -> walker238 inner))
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
          in (Optionals.cases (walker23 fun) (walker23 arg) (\_ -> walker23 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker23 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker23 body) Nothing (\inner -> walker23 inner))
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
          in (Optionals.cases (walker239 fun) (walker239 arg) (\_ -> walker239 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker239 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker239 body) Nothing (\inner -> walker239 inner))
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
          in (Optionals.cases (walker240 fun) (walker240 arg) (\_ -> walker240 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker240 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker240 body) Nothing (\inner -> walker240 inner))
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
          in (Optionals.cases (walker241 fun) (walker241 arg) (\_ -> walker241 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker241 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker241 body) Nothing (\inner -> walker241 inner))
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
          in (Optionals.cases (walker242 fun) (walker242 arg) (\_ -> walker242 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker242 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker242 body) Nothing (\inner -> walker242 inner))
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
          in (Optionals.cases (walker243 fun) (walker243 arg) (\_ -> walker243 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker243 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker243 body) Nothing (\inner -> walker243 inner))
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
          in (Optionals.cases (walker244 fun) (walker244 arg) (\_ -> walker244 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker244 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker244 body) Nothing (\inner -> walker244 inner))
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
          in (Optionals.cases (walker245 fun) (walker245 arg) (\_ -> walker245 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker245 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker245 body) Nothing (\inner -> walker245 inner))
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
          in (Optionals.cases (walker246 fun) (walker246 arg) (\_ -> walker246 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker246 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker246 body) Nothing (\inner -> walker246 inner))
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
          in (Optionals.cases (walker247 fun) (walker247 arg) (\_ -> walker247 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker247 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker247 body) Nothing (\inner -> walker247 inner))
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
          in (Optionals.cases (walker248 fun) (walker248 arg) (\_ -> walker248 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker248 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker248 body) Nothing (\inner -> walker248 inner))
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
          in (Optionals.cases (walker24 fun) (walker24 arg) (\_ -> walker24 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker24 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker24 body) Nothing (\inner -> walker24 inner))
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
          in (Optionals.cases (walker249 fun) (walker249 arg) (\_ -> walker249 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker249 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker249 body) Nothing (\inner -> walker249 inner))
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
          in (Optionals.cases (walker250 fun) (walker250 arg) (\_ -> walker250 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker250 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker250 body) Nothing (\inner -> walker250 inner))
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
          in (Optionals.cases (walker251 fun) (walker251 arg) (\_ -> walker251 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker251 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker251 body) Nothing (\inner -> walker251 inner))
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
          in (Optionals.cases (walker252 fun) (walker252 arg) (\_ -> walker252 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker252 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker252 body) Nothing (\inner -> walker252 inner))
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
          in (Optionals.cases (walker253 fun) (walker253 arg) (\_ -> walker253 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker253 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker253 body) Nothing (\inner -> walker253 inner))
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
          in (Optionals.cases (walker254 fun) (walker254 arg) (\_ -> walker254 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker254 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker254 body) Nothing (\inner -> walker254 inner))
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
          in (Optionals.cases (walker255 fun) (walker255 arg) (\_ -> walker255 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker255 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker255 body) Nothing (\inner -> walker255 inner))
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
          in (Optionals.cases (walker256 fun) (walker256 arg) (\_ -> walker256 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker256 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker256 body) Nothing (\inner -> walker256 inner))
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
          in (Optionals.cases (walker257 fun) (walker257 arg) (\_ -> walker257 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker257 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker257 body) Nothing (\inner -> walker257 inner))
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
          in (Optionals.cases (walker258 fun) (walker258 arg) (\_ -> walker258 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker258 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker258 body) Nothing (\inner -> walker258 inner))
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
          in (Optionals.cases (walker25 fun) (walker25 arg) (\_ -> walker25 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker25 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker25 body) Nothing (\inner -> walker25 inner))
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
          in (Optionals.cases (walker259 fun) (walker259 arg) (\_ -> walker259 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker259 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker259 body) Nothing (\inner -> walker259 inner))
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
          in (Optionals.cases (walker260 fun) (walker260 arg) (\_ -> walker260 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker260 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker260 body) Nothing (\inner -> walker260 inner))
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
          in (Optionals.cases (walker261 fun) (walker261 arg) (\_ -> walker261 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker261 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker261 body) Nothing (\inner -> walker261 inner))
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
          in (Optionals.cases (walker262 fun) (walker262 arg) (\_ -> walker262 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker262 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker262 body) Nothing (\inner -> walker262 inner))
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
          in (Optionals.cases (walker263 fun) (walker263 arg) (\_ -> walker263 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker263 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker263 body) Nothing (\inner -> walker263 inner))
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
          in (Optionals.cases (walker264 fun) (walker264 arg) (\_ -> walker264 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker264 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker264 body) Nothing (\inner -> walker264 inner))
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
          in (Optionals.cases (walker265 fun) (walker265 arg) (\_ -> walker265 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker265 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker265 body) Nothing (\inner -> walker265 inner))
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
          in (Optionals.cases (walker266 fun) (walker266 arg) (\_ -> walker266 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker266 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker266 body) Nothing (\inner -> walker266 inner))
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
          in (Optionals.cases (walker267 fun) (walker267 arg) (\_ -> walker267 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker267 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker267 body) Nothing (\inner -> walker267 inner))
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
          in (Optionals.cases (walker268 fun) (walker268 arg) (\_ -> walker268 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker268 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker268 body) Nothing (\inner -> walker268 inner))
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
          in (Optionals.cases (walker26 fun) (walker26 arg) (\_ -> walker26 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker26 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker26 body) Nothing (\inner -> walker26 inner))
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
          in (Optionals.cases (walker269 fun) (walker269 arg) (\_ -> walker269 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker269 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker269 body) Nothing (\inner -> walker269 inner))
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
          in (Optionals.cases (walker270 fun) (walker270 arg) (\_ -> walker270 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker270 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker270 body) Nothing (\inner -> walker270 inner))
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
          in (Optionals.cases (walker271 fun) (walker271 arg) (\_ -> walker271 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker271 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker271 body) Nothing (\inner -> walker271 inner))
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
          in (Optionals.cases (walker272 fun) (walker272 arg) (\_ -> walker272 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker272 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker272 body) Nothing (\inner -> walker272 inner))
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
          in (Optionals.cases (walker273 fun) (walker273 arg) (\_ -> walker273 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker273 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker273 body) Nothing (\inner -> walker273 inner))
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
          in (Optionals.cases (walker274 fun) (walker274 arg) (\_ -> walker274 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker274 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker274 body) Nothing (\inner -> walker274 inner))
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
          in (Optionals.cases (walker275 fun) (walker275 arg) (\_ -> walker275 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker275 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker275 body) Nothing (\inner -> walker275 inner))
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
          in (Optionals.cases (walker276 fun) (walker276 arg) (\_ -> walker276 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker276 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker276 body) Nothing (\inner -> walker276 inner))
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
          in (Optionals.cases (walker277 fun) (walker277 arg) (\_ -> walker277 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker277 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker277 body) Nothing (\inner -> walker277 inner))
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
          in (Optionals.cases (walker278 fun) (walker278 arg) (\_ -> walker278 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker278 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker278 body) Nothing (\inner -> walker278 inner))
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
          in (Optionals.cases (walker27 fun) (walker27 arg) (\_ -> walker27 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker27 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker27 body) Nothing (\inner -> walker27 inner))
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
          in (Optionals.cases (walker279 fun) (walker279 arg) (\_ -> walker279 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker279 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker279 body) Nothing (\inner -> walker279 inner))
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
          in (Optionals.cases (walker280 fun) (walker280 arg) (\_ -> walker280 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker280 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker280 body) Nothing (\inner -> walker280 inner))
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
          in (Optionals.cases (walker281 fun) (walker281 arg) (\_ -> walker281 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker281 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker281 body) Nothing (\inner -> walker281 inner))
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
          in (Optionals.cases (walker282 fun) (walker282 arg) (\_ -> walker282 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker282 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker282 body) Nothing (\inner -> walker282 inner))
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
          in (Optionals.cases (walker283 fun) (walker283 arg) (\_ -> walker283 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker283 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker283 body) Nothing (\inner -> walker283 inner))
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
          in (Optionals.cases (walker284 fun) (walker284 arg) (\_ -> walker284 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker284 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker284 body) Nothing (\inner -> walker284 inner))
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
          in (Optionals.cases (walker285 fun) (walker285 arg) (\_ -> walker285 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker285 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker285 body) Nothing (\inner -> walker285 inner))
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
          in (Optionals.cases (walker286 fun) (walker286 arg) (\_ -> walker286 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker286 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker286 body) Nothing (\inner -> walker286 inner))
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
          in (Optionals.cases (walker287 fun) (walker287 arg) (\_ -> walker287 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker287 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker287 body) Nothing (\inner -> walker287 inner))
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
          in (Optionals.cases (walker288 fun) (walker288 arg) (\_ -> walker288 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker288 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker288 body) Nothing (\inner -> walker288 inner))
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
          in (Optionals.cases (walker28 fun) (walker28 arg) (\_ -> walker28 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker28 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker28 body) Nothing (\inner -> walker28 inner))
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
          in (Optionals.cases (walker289 fun) (walker289 arg) (\_ -> walker289 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker289 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker289 body) Nothing (\inner -> walker289 inner))
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
          in (Optionals.cases (walker290 fun) (walker290 arg) (\_ -> walker290 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker290 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker290 body) Nothing (\inner -> walker290 inner))
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
          in (Optionals.cases (walker291 fun) (walker291 arg) (\_ -> walker291 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker291 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker291 body) Nothing (\inner -> walker291 inner))
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
          in (Optionals.cases (walker292 fun) (walker292 arg) (\_ -> walker292 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker292 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker292 body) Nothing (\inner -> walker292 inner))
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
          in (Optionals.cases (walker293 fun) (walker293 arg) (\_ -> walker293 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker293 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker293 body) Nothing (\inner -> walker293 inner))
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
          in (Optionals.cases (walker294 fun) (walker294 arg) (\_ -> walker294 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker294 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker294 body) Nothing (\inner -> walker294 inner))
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
          in (Optionals.cases (walker295 fun) (walker295 arg) (\_ -> walker295 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker295 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker295 body) Nothing (\inner -> walker295 inner))
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
          in (Optionals.cases (walker296 fun) (walker296 arg) (\_ -> walker296 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker296 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker296 body) Nothing (\inner -> walker296 inner))
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
          in (Optionals.cases (walker297 fun) (walker297 arg) (\_ -> walker297 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker297 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker297 body) Nothing (\inner -> walker297 inner))
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
          in (Optionals.cases (walker298 fun) (walker298 arg) (\_ -> walker298 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker298 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker298 body) Nothing (\inner -> walker298 inner))
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
          in (Optionals.cases (walker2 fun) (walker2 arg) (\_ -> walker2 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker2 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker2 body) Nothing (\inner -> walker2 inner))
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
          in (Optionals.cases (walker29 fun) (walker29 arg) (\_ -> walker29 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker29 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker29 body) Nothing (\inner -> walker29 inner))
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
          in (Optionals.cases (walker299 fun) (walker299 arg) (\_ -> walker299 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker299 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker299 body) Nothing (\inner -> walker299 inner))
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
          in (Optionals.cases (walker300 fun) (walker300 arg) (\_ -> walker300 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker300 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker300 body) Nothing (\inner -> walker300 inner))
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
          in (Optionals.cases (walker301 fun) (walker301 arg) (\_ -> walker301 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker301 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker301 body) Nothing (\inner -> walker301 inner))
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
          in (Optionals.cases (walker302 fun) (walker302 arg) (\_ -> walker302 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker302 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker302 body) Nothing (\inner -> walker302 inner))
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
          in (Optionals.cases (walker303 fun) (walker303 arg) (\_ -> walker303 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker303 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker303 body) Nothing (\inner -> walker303 inner))
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
          in (Optionals.cases (walker304 fun) (walker304 arg) (\_ -> walker304 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker304 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker304 body) Nothing (\inner -> walker304 inner))
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
          in (Optionals.cases (walker305 fun) (walker305 arg) (\_ -> walker305 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker305 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker305 body) Nothing (\inner -> walker305 inner))
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
          in (Optionals.cases (walker306 fun) (walker306 arg) (\_ -> walker306 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker306 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker306 body) Nothing (\inner -> walker306 inner))
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
          in (Optionals.cases (walker307 fun) (walker307 arg) (\_ -> walker307 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker307 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker307 body) Nothing (\inner -> walker307 inner))
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
          in (Optionals.cases (walker308 fun) (walker308 arg) (\_ -> walker308 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker308 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker308 body) Nothing (\inner -> walker308 inner))
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
          in (Optionals.cases (walker30 fun) (walker30 arg) (\_ -> walker30 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker30 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker30 body) Nothing (\inner -> walker30 inner))
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
          in (Optionals.cases (walker309 fun) (walker309 arg) (\_ -> walker309 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker309 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker309 body) Nothing (\inner -> walker309 inner))
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
          in (Optionals.cases (walker310 fun) (walker310 arg) (\_ -> walker310 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker310 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker310 body) Nothing (\inner -> walker310 inner))
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
          in (Optionals.cases (walker311 fun) (walker311 arg) (\_ -> walker311 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker311 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker311 body) Nothing (\inner -> walker311 inner))
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
          in (Optionals.cases (walker312 fun) (walker312 arg) (\_ -> walker312 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker312 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker312 body) Nothing (\inner -> walker312 inner))
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
          in (Optionals.cases (walker313 fun) (walker313 arg) (\_ -> walker313 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker313 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker313 body) Nothing (\inner -> walker313 inner))
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
          in (Optionals.cases (walker314 fun) (walker314 arg) (\_ -> walker314 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker314 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker314 body) Nothing (\inner -> walker314 inner))
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
          in (Optionals.cases (walker315 fun) (walker315 arg) (\_ -> walker315 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker315 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker315 body) Nothing (\inner -> walker315 inner))
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
          in (Optionals.cases (walker316 fun) (walker316 arg) (\_ -> walker316 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker316 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker316 body) Nothing (\inner -> walker316 inner))
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
          in (Optionals.cases (walker317 fun) (walker317 arg) (\_ -> walker317 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker317 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker317 body) Nothing (\inner -> walker317 inner))
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
          in (Optionals.cases (walker318 fun) (walker318 arg) (\_ -> walker318 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker318 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker318 body) Nothing (\inner -> walker318 inner))
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
          in (Optionals.cases (walker31 fun) (walker31 arg) (\_ -> walker31 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker31 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker31 body) Nothing (\inner -> walker31 inner))
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
          in (Optionals.cases (walker319 fun) (walker319 arg) (\_ -> walker319 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker319 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker319 body) Nothing (\inner -> walker319 inner))
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
          in (Optionals.cases (walker320 fun) (walker320 arg) (\_ -> walker320 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker320 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker320 body) Nothing (\inner -> walker320 inner))
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
          in (Optionals.cases (walker321 fun) (walker321 arg) (\_ -> walker321 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker321 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker321 body) Nothing (\inner -> walker321 inner))
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
          in (Optionals.cases (walker322 fun) (walker322 arg) (\_ -> walker322 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker322 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker322 body) Nothing (\inner -> walker322 inner))
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
          in (Optionals.cases (walker323 fun) (walker323 arg) (\_ -> walker323 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker323 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker323 body) Nothing (\inner -> walker323 inner))
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
          in (Optionals.cases (walker324 fun) (walker324 arg) (\_ -> walker324 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker324 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker324 body) Nothing (\inner -> walker324 inner))
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
          in (Optionals.cases (walker325 fun) (walker325 arg) (\_ -> walker325 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker325 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker325 body) Nothing (\inner -> walker325 inner))
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
          in (Optionals.cases (walker326 fun) (walker326 arg) (\_ -> walker326 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker326 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker326 body) Nothing (\inner -> walker326 inner))
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
          in (Optionals.cases (walker327 fun) (walker327 arg) (\_ -> walker327 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker327 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker327 body) Nothing (\inner -> walker327 inner))
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
          in (Optionals.cases (walker328 fun) (walker328 arg) (\_ -> walker328 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker328 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker328 body) Nothing (\inner -> walker328 inner))
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
          in (Optionals.cases (walker32 fun) (walker32 arg) (\_ -> walker32 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker32 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker32 body) Nothing (\inner -> walker32 inner))
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
          in (Optionals.cases (walker329 fun) (walker329 arg) (\_ -> walker329 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker329 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker329 body) Nothing (\inner -> walker329 inner))
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
          in (Optionals.cases (walker330 fun) (walker330 arg) (\_ -> walker330 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker330 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker330 body) Nothing (\inner -> walker330 inner))
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
          in (Optionals.cases (walker331 fun) (walker331 arg) (\_ -> walker331 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker331 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker331 body) Nothing (\inner -> walker331 inner))
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
          in (Optionals.cases (walker332 fun) (walker332 arg) (\_ -> walker332 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker332 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker332 body) Nothing (\inner -> walker332 inner))
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
          in (Optionals.cases (walker333 fun) (walker333 arg) (\_ -> walker333 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker333 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker333 body) Nothing (\inner -> walker333 inner))
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
          in (Optionals.cases (walker334 fun) (walker334 arg) (\_ -> walker334 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker334 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker334 body) Nothing (\inner -> walker334 inner))
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
          in (Optionals.cases (walker335 fun) (walker335 arg) (\_ -> walker335 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker335 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker335 body) Nothing (\inner -> walker335 inner))
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
          in (Optionals.cases (walker336 fun) (walker336 arg) (\_ -> walker336 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker336 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker336 body) Nothing (\inner -> walker336 inner))
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
          in (Optionals.cases (walker337 fun) (walker337 arg) (\_ -> walker337 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker337 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker337 body) Nothing (\inner -> walker337 inner))
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
          in (Optionals.cases (walker338 fun) (walker338 arg) (\_ -> walker338 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker338 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker338 body) Nothing (\inner -> walker338 inner))
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
          in (Optionals.cases (walker33 fun) (walker33 arg) (\_ -> walker33 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker33 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker33 body) Nothing (\inner -> walker33 inner))
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
          in (Optionals.cases (walker339 fun) (walker339 arg) (\_ -> walker339 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker339 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker339 body) Nothing (\inner -> walker339 inner))
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
          in (Optionals.cases (walker340 fun) (walker340 arg) (\_ -> walker340 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker340 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker340 body) Nothing (\inner -> walker340 inner))
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
          in (Optionals.cases (walker341 fun) (walker341 arg) (\_ -> walker341 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker341 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker341 body) Nothing (\inner -> walker341 inner))
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
          in (Optionals.cases (walker342 fun) (walker342 arg) (\_ -> walker342 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker342 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker342 body) Nothing (\inner -> walker342 inner))
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
          in (Optionals.cases (walker343 fun) (walker343 arg) (\_ -> walker343 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker343 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker343 body) Nothing (\inner -> walker343 inner))
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
          in (Optionals.cases (walker344 fun) (walker344 arg) (\_ -> walker344 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker344 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker344 body) Nothing (\inner -> walker344 inner))
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
          in (Optionals.cases (walker345 fun) (walker345 arg) (\_ -> walker345 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker345 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker345 body) Nothing (\inner -> walker345 inner))
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
          in (Optionals.cases (walker346 fun) (walker346 arg) (\_ -> walker346 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker346 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker346 body) Nothing (\inner -> walker346 inner))
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
          in (Optionals.cases (walker347 fun) (walker347 arg) (\_ -> walker347 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker347 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker347 body) Nothing (\inner -> walker347 inner))
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
          in (Optionals.cases (walker348 fun) (walker348 arg) (\_ -> walker348 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker348 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker348 body) Nothing (\inner -> walker348 inner))
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
          in (Optionals.cases (walker34 fun) (walker34 arg) (\_ -> walker34 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker34 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker34 body) Nothing (\inner -> walker34 inner))
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
          in (Optionals.cases (walker349 fun) (walker349 arg) (\_ -> walker349 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker349 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker349 body) Nothing (\inner -> walker349 inner))
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
          in (Optionals.cases (walker350 fun) (walker350 arg) (\_ -> walker350 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker350 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker350 body) Nothing (\inner -> walker350 inner))
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
          in (Optionals.cases (walker351 fun) (walker351 arg) (\_ -> walker351 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker351 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker351 body) Nothing (\inner -> walker351 inner))
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
          in (Optionals.cases (walker352 fun) (walker352 arg) (\_ -> walker352 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker352 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker352 body) Nothing (\inner -> walker352 inner))
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
          in (Optionals.cases (walker353 fun) (walker353 arg) (\_ -> walker353 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker353 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker353 body) Nothing (\inner -> walker353 inner))
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
          in (Optionals.cases (walker354 fun) (walker354 arg) (\_ -> walker354 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker354 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker354 body) Nothing (\inner -> walker354 inner))
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
          in (Optionals.cases (walker355 fun) (walker355 arg) (\_ -> walker355 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker355 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker355 body) Nothing (\inner -> walker355 inner))
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
          in (Optionals.cases (walker356 fun) (walker356 arg) (\_ -> walker356 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker356 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker356 body) Nothing (\inner -> walker356 inner))
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
          in (Optionals.cases (walker357 fun) (walker357 arg) (\_ -> walker357 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker357 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker357 body) Nothing (\inner -> walker357 inner))
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
          in (Optionals.cases (walker358 fun) (walker358 arg) (\_ -> walker358 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker358 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker358 body) Nothing (\inner -> walker358 inner))
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
          in (Optionals.cases (walker35 fun) (walker35 arg) (\_ -> walker35 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker35 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker35 body) Nothing (\inner -> walker35 inner))
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
          in (Optionals.cases (walker359 fun) (walker359 arg) (\_ -> walker359 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker359 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker359 body) Nothing (\inner -> walker359 inner))
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
          in (Optionals.cases (walker360 fun) (walker360 arg) (\_ -> walker360 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker360 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker360 body) Nothing (\inner -> walker360 inner))
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
          in (Optionals.cases (walker361 fun) (walker361 arg) (\_ -> walker361 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker361 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker361 body) Nothing (\inner -> walker361 inner))
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
          in (Optionals.cases (walker362 fun) (walker362 arg) (\_ -> walker362 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker362 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker362 body) Nothing (\inner -> walker362 inner))
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
          in (Optionals.cases (walker363 fun) (walker363 arg) (\_ -> walker363 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker363 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker363 body) Nothing (\inner -> walker363 inner))
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
          in (Optionals.cases (walker364 fun) (walker364 arg) (\_ -> walker364 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker364 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker364 body) Nothing (\inner -> walker364 inner))
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
          in (Optionals.cases (walker365 fun) (walker365 arg) (\_ -> walker365 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker365 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker365 body) Nothing (\inner -> walker365 inner))
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
          in (Optionals.cases (walker366 fun) (walker366 arg) (\_ -> walker366 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker366 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker366 body) Nothing (\inner -> walker366 inner))
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
          in (Optionals.cases (walker367 fun) (walker367 arg) (\_ -> walker367 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker367 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker367 body) Nothing (\inner -> walker367 inner))
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
          in (Optionals.cases (walker368 fun) (walker368 arg) (\_ -> walker368 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker368 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker368 body) Nothing (\inner -> walker368 inner))
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
          in (Optionals.cases (walker36 fun) (walker36 arg) (\_ -> walker36 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker36 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker36 body) Nothing (\inner -> walker36 inner))
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
          in (Optionals.cases (walker369 fun) (walker369 arg) (\_ -> walker369 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker369 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker369 body) Nothing (\inner -> walker369 inner))
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
          in (Optionals.cases (walker370 fun) (walker370 arg) (\_ -> walker370 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker370 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker370 body) Nothing (\inner -> walker370 inner))
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
          in (Optionals.cases (walker371 fun) (walker371 arg) (\_ -> walker371 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker371 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker371 body) Nothing (\inner -> walker371 inner))
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
          in (Optionals.cases (walker372 fun) (walker372 arg) (\_ -> walker372 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker372 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker372 body) Nothing (\inner -> walker372 inner))
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
          in (Optionals.cases (walker373 fun) (walker373 arg) (\_ -> walker373 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker373 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker373 body) Nothing (\inner -> walker373 inner))
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
          in (Optionals.cases (walker374 fun) (walker374 arg) (\_ -> walker374 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker374 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker374 body) Nothing (\inner -> walker374 inner))
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
          in (Optionals.cases (walker375 fun) (walker375 arg) (\_ -> walker375 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker375 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker375 body) Nothing (\inner -> walker375 inner))
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
          in (Optionals.cases (walker376 fun) (walker376 arg) (\_ -> walker376 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker376 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker376 body) Nothing (\inner -> walker376 inner))
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
          in (Optionals.cases (walker377 fun) (walker377 arg) (\_ -> walker377 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker377 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker377 body) Nothing (\inner -> walker377 inner))
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
          in (Optionals.cases (walker378 fun) (walker378 arg) (\_ -> walker378 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker378 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker378 body) Nothing (\inner -> walker378 inner))
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
          in (Optionals.cases (walker37 fun) (walker37 arg) (\_ -> walker37 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker37 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker37 body) Nothing (\inner -> walker37 inner))
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
          in (Optionals.cases (walker379 fun) (walker379 arg) (\_ -> walker379 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker379 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker379 body) Nothing (\inner -> walker379 inner))
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
          in (Optionals.cases (walker380 fun) (walker380 arg) (\_ -> walker380 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker380 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker380 body) Nothing (\inner -> walker380 inner))
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
          in (Optionals.cases (walker381 fun) (walker381 arg) (\_ -> walker381 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker381 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker381 body) Nothing (\inner -> walker381 inner))
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
          in (Optionals.cases (walker382 fun) (walker382 arg) (\_ -> walker382 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker382 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker382 body) Nothing (\inner -> walker382 inner))
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
          in (Optionals.cases (walker383 fun) (walker383 arg) (\_ -> walker383 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker383 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker383 body) Nothing (\inner -> walker383 inner))
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
          in (Optionals.cases (walker384 fun) (walker384 arg) (\_ -> walker384 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker384 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker384 body) Nothing (\inner -> walker384 inner))
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
          in (Optionals.cases (walker385 fun) (walker385 arg) (\_ -> walker385 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker385 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker385 body) Nothing (\inner -> walker385 inner))
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
          in (Optionals.cases (walker386 fun) (walker386 arg) (\_ -> walker386 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker386 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker386 body) Nothing (\inner -> walker386 inner))
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
          in (Optionals.cases (walker387 fun) (walker387 arg) (\_ -> walker387 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker387 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker387 body) Nothing (\inner -> walker387 inner))
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
          in (Optionals.cases (walker388 fun) (walker388 arg) (\_ -> walker388 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker388 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker388 body) Nothing (\inner -> walker388 inner))
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
          in (Optionals.cases (walker38 fun) (walker38 arg) (\_ -> walker38 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker38 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker38 body) Nothing (\inner -> walker38 inner))
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
          in (Optionals.cases (walker389 fun) (walker389 arg) (\_ -> walker389 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker389 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker389 body) Nothing (\inner -> walker389 inner))
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
          in (Optionals.cases (walker390 fun) (walker390 arg) (\_ -> walker390 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker390 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker390 body) Nothing (\inner -> walker390 inner))
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
          in (Optionals.cases (walker391 fun) (walker391 arg) (\_ -> walker391 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker391 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker391 body) Nothing (\inner -> walker391 inner))
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
          in (Optionals.cases (walker392 fun) (walker392 arg) (\_ -> walker392 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker392 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker392 body) Nothing (\inner -> walker392 inner))
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
          in (Optionals.cases (walker393 fun) (walker393 arg) (\_ -> walker393 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker393 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker393 body) Nothing (\inner -> walker393 inner))
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
          in (Optionals.cases (walker394 fun) (walker394 arg) (\_ -> walker394 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker394 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker394 body) Nothing (\inner -> walker394 inner))
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
          in (Optionals.cases (walker395 fun) (walker395 arg) (\_ -> walker395 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker395 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker395 body) Nothing (\inner -> walker395 inner))
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
          in (Optionals.cases (walker396 fun) (walker396 arg) (\_ -> walker396 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker396 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker396 body) Nothing (\inner -> walker396 inner))
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
          in (Optionals.cases (walker397 fun) (walker397 arg) (\_ -> walker397 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker397 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker397 body) Nothing (\inner -> walker397 inner))
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
          in (Optionals.cases (walker398 fun) (walker398 arg) (\_ -> walker398 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker398 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker398 body) Nothing (\inner -> walker398 inner))
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
          in (Optionals.cases (walker3 fun) (walker3 arg) (\_ -> walker3 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker3 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker3 body) Nothing (\inner -> walker3 inner))
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
          in (Optionals.cases (walker39 fun) (walker39 arg) (\_ -> walker39 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker39 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker39 body) Nothing (\inner -> walker39 inner))
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
          in (Optionals.cases (walker399 fun) (walker399 arg) (\_ -> walker399 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker399 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker399 body) Nothing (\inner -> walker399 inner))
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
          in (Optionals.cases (walker400 fun) (walker400 arg) (\_ -> walker400 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker400 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker400 body) Nothing (\inner -> walker400 inner))
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
          in (Optionals.cases (walker401 fun) (walker401 arg) (\_ -> walker401 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker401 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker401 body) Nothing (\inner -> walker401 inner))
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
          in (Optionals.cases (walker402 fun) (walker402 arg) (\_ -> walker402 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker402 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker402 body) Nothing (\inner -> walker402 inner))
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
          in (Optionals.cases (walker403 fun) (walker403 arg) (\_ -> walker403 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker403 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker403 body) Nothing (\inner -> walker403 inner))
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
          in (Optionals.cases (walker404 fun) (walker404 arg) (\_ -> walker404 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker404 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker404 body) Nothing (\inner -> walker404 inner))
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
          in (Optionals.cases (walker405 fun) (walker405 arg) (\_ -> walker405 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker405 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker405 body) Nothing (\inner -> walker405 inner))
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
          in (Optionals.cases (walker406 fun) (walker406 arg) (\_ -> walker406 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker406 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker406 body) Nothing (\inner -> walker406 inner))
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
          in (Optionals.cases (walker407 fun) (walker407 arg) (\_ -> walker407 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker407 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker407 body) Nothing (\inner -> walker407 inner))
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
          in (Optionals.cases (walker408 fun) (walker408 arg) (\_ -> walker408 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker408 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker408 body) Nothing (\inner -> walker408 inner))
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
          in (Optionals.cases (walker40 fun) (walker40 arg) (\_ -> walker40 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker40 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker40 body) Nothing (\inner -> walker40 inner))
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
          in (Optionals.cases (walker409 fun) (walker409 arg) (\_ -> walker409 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker409 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker409 body) Nothing (\inner -> walker409 inner))
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
          in (Optionals.cases (walker410 fun) (walker410 arg) (\_ -> walker410 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker410 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker410 body) Nothing (\inner -> walker410 inner))
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
          in (Optionals.cases (walker411 fun) (walker411 arg) (\_ -> walker411 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker411 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker411 body) Nothing (\inner -> walker411 inner))
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
          in (Optionals.cases (walker412 fun) (walker412 arg) (\_ -> walker412 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker412 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker412 body) Nothing (\inner -> walker412 inner))
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
          in (Optionals.cases (walker413 fun) (walker413 arg) (\_ -> walker413 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker413 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker413 body) Nothing (\inner -> walker413 inner))
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
          in (Optionals.cases (walker414 fun) (walker414 arg) (\_ -> walker414 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker414 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker414 body) Nothing (\inner -> walker414 inner))
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
          in (Optionals.cases (walker415 fun) (walker415 arg) (\_ -> walker415 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker415 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker415 body) Nothing (\inner -> walker415 inner))
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
          in (Optionals.cases (walker416 fun) (walker416 arg) (\_ -> walker416 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker416 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker416 body) Nothing (\inner -> walker416 inner))
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
          in (Optionals.cases (walker417 fun) (walker417 arg) (\_ -> walker417 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker417 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker417 body) Nothing (\inner -> walker417 inner))
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
          in (Optionals.cases (walker418 fun) (walker418 arg) (\_ -> walker418 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker418 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker418 body) Nothing (\inner -> walker418 inner))
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
          in (Optionals.cases (walker41 fun) (walker41 arg) (\_ -> walker41 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker41 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker41 body) Nothing (\inner -> walker41 inner))
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
          in (Optionals.cases (walker419 fun) (walker419 arg) (\_ -> walker419 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker419 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker419 body) Nothing (\inner -> walker419 inner))
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
          in (Optionals.cases (walker420 fun) (walker420 arg) (\_ -> walker420 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker420 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker420 body) Nothing (\inner -> walker420 inner))
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
          in (Optionals.cases (walker421 fun) (walker421 arg) (\_ -> walker421 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker421 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker421 body) Nothing (\inner -> walker421 inner))
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
          in (Optionals.cases (walker422 fun) (walker422 arg) (\_ -> walker422 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker422 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker422 body) Nothing (\inner -> walker422 inner))
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
          in (Optionals.cases (walker423 fun) (walker423 arg) (\_ -> walker423 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker423 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker423 body) Nothing (\inner -> walker423 inner))
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
          in (Optionals.cases (walker424 fun) (walker424 arg) (\_ -> walker424 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker424 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker424 body) Nothing (\inner -> walker424 inner))
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
          in (Optionals.cases (walker425 fun) (walker425 arg) (\_ -> walker425 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker425 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker425 body) Nothing (\inner -> walker425 inner))
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
          in (Optionals.cases (walker426 fun) (walker426 arg) (\_ -> walker426 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker426 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker426 body) Nothing (\inner -> walker426 inner))
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
          in (Optionals.cases (walker427 fun) (walker427 arg) (\_ -> walker427 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker427 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker427 body) Nothing (\inner -> walker427 inner))
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
          in (Optionals.cases (walker428 fun) (walker428 arg) (\_ -> walker428 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker428 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker428 body) Nothing (\inner -> walker428 inner))
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
          in (Optionals.cases (walker42 fun) (walker42 arg) (\_ -> walker42 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker42 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker42 body) Nothing (\inner -> walker42 inner))
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
          in (Optionals.cases (walker429 fun) (walker429 arg) (\_ -> walker429 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker429 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker429 body) Nothing (\inner -> walker429 inner))
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
          in (Optionals.cases (walker430 fun) (walker430 arg) (\_ -> walker430 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker430 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker430 body) Nothing (\inner -> walker430 inner))
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
          in (Optionals.cases (walker431 fun) (walker431 arg) (\_ -> walker431 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker431 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker431 body) Nothing (\inner -> walker431 inner))
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
          in (Optionals.cases (walker432 fun) (walker432 arg) (\_ -> walker432 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker432 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker432 body) Nothing (\inner -> walker432 inner))
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
          in (Optionals.cases (walker433 fun) (walker433 arg) (\_ -> walker433 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker433 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker433 body) Nothing (\inner -> walker433 inner))
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
          in (Optionals.cases (walker434 fun) (walker434 arg) (\_ -> walker434 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker434 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker434 body) Nothing (\inner -> walker434 inner))
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
          in (Optionals.cases (walker435 fun) (walker435 arg) (\_ -> walker435 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker435 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker435 body) Nothing (\inner -> walker435 inner))
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
          in (Optionals.cases (walker436 fun) (walker436 arg) (\_ -> walker436 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker436 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker436 body) Nothing (\inner -> walker436 inner))
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
          in (Optionals.cases (walker437 fun) (walker437 arg) (\_ -> walker437 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker437 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker437 body) Nothing (\inner -> walker437 inner))
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
          in (Optionals.cases (walker438 fun) (walker438 arg) (\_ -> walker438 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker438 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker438 body) Nothing (\inner -> walker438 inner))
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
          in (Optionals.cases (walker43 fun) (walker43 arg) (\_ -> walker43 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker43 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker43 body) Nothing (\inner -> walker43 inner))
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
          in (Optionals.cases (walker439 fun) (walker439 arg) (\_ -> walker439 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker439 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker439 body) Nothing (\inner -> walker439 inner))
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
          in (Optionals.cases (walker440 fun) (walker440 arg) (\_ -> walker440 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker440 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker440 body) Nothing (\inner -> walker440 inner))
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
          in (Optionals.cases (walker441 fun) (walker441 arg) (\_ -> walker441 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker441 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker441 body) Nothing (\inner -> walker441 inner))
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
          in (Optionals.cases (walker442 fun) (walker442 arg) (\_ -> walker442 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker442 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker442 body) Nothing (\inner -> walker442 inner))
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
          in (Optionals.cases (walker443 fun) (walker443 arg) (\_ -> walker443 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker443 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker443 body) Nothing (\inner -> walker443 inner))
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
          in (Optionals.cases (walker444 fun) (walker444 arg) (\_ -> walker444 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker444 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker444 body) Nothing (\inner -> walker444 inner))
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
          in (Optionals.cases (walker445 fun) (walker445 arg) (\_ -> walker445 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker445 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker445 body) Nothing (\inner -> walker445 inner))
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
          in (Optionals.cases (walker446 fun) (walker446 arg) (\_ -> walker446 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker446 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker446 body) Nothing (\inner -> walker446 inner))
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
          in (Optionals.cases (walker447 fun) (walker447 arg) (\_ -> walker447 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker447 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker447 body) Nothing (\inner -> walker447 inner))
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
          in (Optionals.cases (walker448 fun) (walker448 arg) (\_ -> walker448 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker448 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker448 body) Nothing (\inner -> walker448 inner))
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
          in (Optionals.cases (walker44 fun) (walker44 arg) (\_ -> walker44 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker44 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker44 body) Nothing (\inner -> walker44 inner))
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
          in (Optionals.cases (walker449 fun) (walker449 arg) (\_ -> walker449 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker449 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker449 body) Nothing (\inner -> walker449 inner))
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
          in (Optionals.cases (walker450 fun) (walker450 arg) (\_ -> walker450 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker450 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker450 body) Nothing (\inner -> walker450 inner))
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
          in (Optionals.cases (walker451 fun) (walker451 arg) (\_ -> walker451 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker451 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker451 body) Nothing (\inner -> walker451 inner))
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
          in (Optionals.cases (walker452 fun) (walker452 arg) (\_ -> walker452 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker452 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker452 body) Nothing (\inner -> walker452 inner))
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
          in (Optionals.cases (walker453 fun) (walker453 arg) (\_ -> walker453 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker453 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker453 body) Nothing (\inner -> walker453 inner))
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
          in (Optionals.cases (walker454 fun) (walker454 arg) (\_ -> walker454 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker454 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker454 body) Nothing (\inner -> walker454 inner))
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
          in (Optionals.cases (walker455 fun) (walker455 arg) (\_ -> walker455 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker455 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker455 body) Nothing (\inner -> walker455 inner))
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
          in (Optionals.cases (walker456 fun) (walker456 arg) (\_ -> walker456 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker456 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker456 body) Nothing (\inner -> walker456 inner))
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
          in (Optionals.cases (walker457 fun) (walker457 arg) (\_ -> walker457 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker457 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker457 body) Nothing (\inner -> walker457 inner))
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
          in (Optionals.cases (walker458 fun) (walker458 arg) (\_ -> walker458 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker458 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker458 body) Nothing (\inner -> walker458 inner))
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
          in (Optionals.cases (walker45 fun) (walker45 arg) (\_ -> walker45 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker45 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker45 body) Nothing (\inner -> walker45 inner))
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
          in (Optionals.cases (walker459 fun) (walker459 arg) (\_ -> walker459 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker459 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker459 body) Nothing (\inner -> walker459 inner))
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
          in (Optionals.cases (walker460 fun) (walker460 arg) (\_ -> walker460 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker460 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker460 body) Nothing (\inner -> walker460 inner))
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
          in (Optionals.cases (walker461 fun) (walker461 arg) (\_ -> walker461 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker461 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker461 body) Nothing (\inner -> walker461 inner))
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
          in (Optionals.cases (walker462 fun) (walker462 arg) (\_ -> walker462 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker462 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker462 body) Nothing (\inner -> walker462 inner))
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
          in (Optionals.cases (walker463 fun) (walker463 arg) (\_ -> walker463 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker463 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker463 body) Nothing (\inner -> walker463 inner))
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
          in (Optionals.cases (walker464 fun) (walker464 arg) (\_ -> walker464 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker464 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker464 body) Nothing (\inner -> walker464 inner))
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
          in (Optionals.cases (walker465 fun) (walker465 arg) (\_ -> walker465 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker465 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker465 body) Nothing (\inner -> walker465 inner))
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
          in (Optionals.cases (walker466 fun) (walker466 arg) (\_ -> walker466 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker466 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker466 body) Nothing (\inner -> walker466 inner))
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
          in (Optionals.cases (walker467 fun) (walker467 arg) (\_ -> walker467 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker467 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker467 body) Nothing (\inner -> walker467 inner))
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
          in (Optionals.cases (walker468 fun) (walker468 arg) (\_ -> walker468 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker468 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker468 body) Nothing (\inner -> walker468 inner))
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
          in (Optionals.cases (walker46 fun) (walker46 arg) (\_ -> walker46 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker46 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker46 body) Nothing (\inner -> walker46 inner))
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
          in (Optionals.cases (walker469 fun) (walker469 arg) (\_ -> walker469 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker469 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker469 body) Nothing (\inner -> walker469 inner))
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
          in (Optionals.cases (walker470 fun) (walker470 arg) (\_ -> walker470 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker470 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker470 body) Nothing (\inner -> walker470 inner))
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
          in (Optionals.cases (walker471 fun) (walker471 arg) (\_ -> walker471 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker471 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker471 body) Nothing (\inner -> walker471 inner))
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
          in (Optionals.cases (walker472 fun) (walker472 arg) (\_ -> walker472 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker472 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker472 body) Nothing (\inner -> walker472 inner))
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
          in (Optionals.cases (walker473 fun) (walker473 arg) (\_ -> walker473 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker473 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker473 body) Nothing (\inner -> walker473 inner))
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
          in (Optionals.cases (walker474 fun) (walker474 arg) (\_ -> walker474 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker474 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker474 body) Nothing (\inner -> walker474 inner))
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
          in (Optionals.cases (walker475 fun) (walker475 arg) (\_ -> walker475 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker475 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker475 body) Nothing (\inner -> walker475 inner))
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
          in (Optionals.cases (walker476 fun) (walker476 arg) (\_ -> walker476 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker476 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker476 body) Nothing (\inner -> walker476 inner))
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
          in (Optionals.cases (walker477 fun) (walker477 arg) (\_ -> walker477 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker477 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker477 body) Nothing (\inner -> walker477 inner))
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
          in (Optionals.cases (walker478 fun) (walker478 arg) (\_ -> walker478 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker478 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker478 body) Nothing (\inner -> walker478 inner))
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
          in (Optionals.cases (walker47 fun) (walker47 arg) (\_ -> walker47 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker47 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker47 body) Nothing (\inner -> walker47 inner))
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
          in (Optionals.cases (walker479 fun) (walker479 arg) (\_ -> walker479 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker479 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker479 body) Nothing (\inner -> walker479 inner))
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
          in (Optionals.cases (walker480 fun) (walker480 arg) (\_ -> walker480 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker480 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker480 body) Nothing (\inner -> walker480 inner))
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
          in (Optionals.cases (walker481 fun) (walker481 arg) (\_ -> walker481 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker481 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker481 body) Nothing (\inner -> walker481 inner))
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
          in (Optionals.cases (walker482 fun) (walker482 arg) (\_ -> walker482 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker482 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker482 body) Nothing (\inner -> walker482 inner))
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
          in (Optionals.cases (walker483 fun) (walker483 arg) (\_ -> walker483 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker483 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker483 body) Nothing (\inner -> walker483 inner))
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
          in (Optionals.cases (walker484 fun) (walker484 arg) (\_ -> walker484 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker484 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker484 body) Nothing (\inner -> walker484 inner))
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
          in (Optionals.cases (walker485 fun) (walker485 arg) (\_ -> walker485 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker485 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker485 body) Nothing (\inner -> walker485 inner))
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
          in (Optionals.cases (walker486 fun) (walker486 arg) (\_ -> walker486 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker486 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker486 body) Nothing (\inner -> walker486 inner))
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
          in (Optionals.cases (walker487 fun) (walker487 arg) (\_ -> walker487 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker487 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker487 body) Nothing (\inner -> walker487 inner))
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
          in (Optionals.cases (walker488 fun) (walker488 arg) (\_ -> walker488 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker488 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker488 body) Nothing (\inner -> walker488 inner))
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
          in (Optionals.cases (walker48 fun) (walker48 arg) (\_ -> walker48 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker48 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker48 body) Nothing (\inner -> walker48 inner))
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
          in (Optionals.cases (walker489 fun) (walker489 arg) (\_ -> walker489 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker489 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker489 body) Nothing (\inner -> walker489 inner))
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
          in (Optionals.cases (walker490 fun) (walker490 arg) (\_ -> walker490 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker490 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker490 body) Nothing (\inner -> walker490 inner))
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
          in (Optionals.cases (walker491 fun) (walker491 arg) (\_ -> walker491 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker491 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker491 body) Nothing (\inner -> walker491 inner))
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
          in (Optionals.cases (walker492 fun) (walker492 arg) (\_ -> walker492 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker492 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker492 body) Nothing (\inner -> walker492 inner))
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
          in (Optionals.cases (walker493 fun) (walker493 arg) (\_ -> walker493 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker493 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker493 body) Nothing (\inner -> walker493 inner))
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
          in (Optionals.cases (walker494 fun) (walker494 arg) (\_ -> walker494 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker494 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker494 body) Nothing (\inner -> walker494 inner))
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
          in (Optionals.cases (walker495 fun) (walker495 arg) (\_ -> walker495 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker495 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker495 body) Nothing (\inner -> walker495 inner))
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
          in (Optionals.cases (walker496 fun) (walker496 arg) (\_ -> walker496 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker496 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker496 body) Nothing (\inner -> walker496 inner))
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
          in (Optionals.cases (walker497 fun) (walker497 arg) (\_ -> walker497 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker497 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker497 body) Nothing (\inner -> walker497 inner))
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
          in (Optionals.cases (walker498 fun) (walker498 arg) (\_ -> walker498 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker498 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker498 body) Nothing (\inner -> walker498 inner))
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
          in (Optionals.cases (walker4 fun) (walker4 arg) (\_ -> walker4 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker4 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker4 body) Nothing (\inner -> walker4 inner))
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
          in (Optionals.cases (walker49 fun) (walker49 arg) (\_ -> walker49 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker49 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker49 body) Nothing (\inner -> walker49 inner))
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
          in (Optionals.cases (walker499 fun) (walker499 arg) (\_ -> walker499 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker499 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker499 body) Nothing (\inner -> walker499 inner))
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
          in (Optionals.cases (walker500 fun) (walker500 arg) (\_ -> walker500 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker500 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker500 body) Nothing (\inner -> walker500 inner))
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
          in (Optionals.cases (walker501 fun) (walker501 arg) (\_ -> walker501 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker501 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker501 body) Nothing (\inner -> walker501 inner))
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
          in (Optionals.cases (walker502 fun) (walker502 arg) (\_ -> walker502 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker502 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker502 body) Nothing (\inner -> walker502 inner))
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
          in (Optionals.cases (walker503 fun) (walker503 arg) (\_ -> walker503 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker503 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker503 body) Nothing (\inner -> walker503 inner))
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
          in (Optionals.cases (walker504 fun) (walker504 arg) (\_ -> walker504 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker504 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker504 body) Nothing (\inner -> walker504 inner))
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
          in (Optionals.cases (walker505 fun) (walker505 arg) (\_ -> walker505 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker505 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker505 body) Nothing (\inner -> walker505 inner))
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
          in (Optionals.cases (walker506 fun) (walker506 arg) (\_ -> walker506 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker506 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker506 body) Nothing (\inner -> walker506 inner))
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
          in (Optionals.cases (walker507 fun) (walker507 arg) (\_ -> walker507 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker507 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker507 body) Nothing (\inner -> walker507 inner))
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
          in (Optionals.cases (walker508 fun) (walker508 arg) (\_ -> walker508 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker508 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker508 body) Nothing (\inner -> walker508 inner))
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
          in (Optionals.cases (walker50 fun) (walker50 arg) (\_ -> walker50 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker50 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker50 body) Nothing (\inner -> walker50 inner))
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
          in (Optionals.cases (walker509 fun) (walker509 arg) (\_ -> walker509 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker509 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker509 body) Nothing (\inner -> walker509 inner))
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
          in (Optionals.cases (walker510 fun) (walker510 arg) (\_ -> walker510 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker510 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker510 body) Nothing (\inner -> walker510 inner))
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
          in (Optionals.cases (walker511 fun) (walker511 arg) (\_ -> walker511 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker511 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker511 body) Nothing (\inner -> walker511 inner))
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
          in (Optionals.cases (walker512 fun) (walker512 arg) (\_ -> walker512 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker512 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker512 body) Nothing (\inner -> walker512 inner))
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
          in (Optionals.cases (walker513 fun) (walker513 arg) (\_ -> walker513 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker513 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker513 body) Nothing (\inner -> walker513 inner))
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
          in (Optionals.cases (walker514 fun) (walker514 arg) (\_ -> walker514 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker514 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker514 body) Nothing (\inner -> walker514 inner))
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
          in (Optionals.cases (walker515 fun) (walker515 arg) (\_ -> walker515 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker515 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker515 body) Nothing (\inner -> walker515 inner))
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
          in (Optionals.cases (walker516 fun) (walker516 arg) (\_ -> walker516 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker516 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker516 body) Nothing (\inner -> walker516 inner))
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
          in (Optionals.cases (walker517 fun) (walker517 arg) (\_ -> walker517 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker517 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker517 body) Nothing (\inner -> walker517 inner))
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
          in (Optionals.cases (walker518 fun) (walker518 arg) (\_ -> walker518 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker518 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker518 body) Nothing (\inner -> walker518 inner))
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
          in (Optionals.cases (walker51 fun) (walker51 arg) (\_ -> walker51 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker51 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker51 body) Nothing (\inner -> walker51 inner))
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
          in (Optionals.cases (walker519 fun) (walker519 arg) (\_ -> walker519 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker519 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker519 body) Nothing (\inner -> walker519 inner))
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
          in (Optionals.cases (walker520 fun) (walker520 arg) (\_ -> walker520 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker520 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker520 body) Nothing (\inner -> walker520 inner))
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
          in (Optionals.cases (walker521 fun) (walker521 arg) (\_ -> walker521 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker521 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker521 body) Nothing (\inner -> walker521 inner))
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
          in (Optionals.cases (walker522 fun) (walker522 arg) (\_ -> walker522 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker522 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker522 body) Nothing (\inner -> walker522 inner))
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
          in (Optionals.cases (walker523 fun) (walker523 arg) (\_ -> walker523 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker523 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker523 body) Nothing (\inner -> walker523 inner))
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
          in (Optionals.cases (walker524 fun) (walker524 arg) (\_ -> walker524 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker524 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker524 body) Nothing (\inner -> walker524 inner))
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
          in (Optionals.cases (walker525 fun) (walker525 arg) (\_ -> walker525 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker525 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker525 body) Nothing (\inner -> walker525 inner))
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
          in (Optionals.cases (walker526 fun) (walker526 arg) (\_ -> walker526 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker526 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker526 body) Nothing (\inner -> walker526 inner))
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
          in (Optionals.cases (walker527 fun) (walker527 arg) (\_ -> walker527 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker527 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker527 body) Nothing (\inner -> walker527 inner))
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
          in (Optionals.cases (walker528 fun) (walker528 arg) (\_ -> walker528 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker528 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker528 body) Nothing (\inner -> walker528 inner))
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
          in (Optionals.cases (walker52 fun) (walker52 arg) (\_ -> walker52 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker52 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker52 body) Nothing (\inner -> walker52 inner))
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
          in (Optionals.cases (walker529 fun) (walker529 arg) (\_ -> walker529 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker529 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker529 body) Nothing (\inner -> walker529 inner))
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
          in (Optionals.cases (walker530 fun) (walker530 arg) (\_ -> walker530 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker530 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker530 body) Nothing (\inner -> walker530 inner))
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
          in (Optionals.cases (walker531 fun) (walker531 arg) (\_ -> walker531 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker531 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker531 body) Nothing (\inner -> walker531 inner))
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
          in (Optionals.cases (walker532 fun) (walker532 arg) (\_ -> walker532 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker532 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker532 body) Nothing (\inner -> walker532 inner))
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
          in (Optionals.cases (walker533 fun) (walker533 arg) (\_ -> walker533 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker533 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker533 body) Nothing (\inner -> walker533 inner))
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
          in (Optionals.cases (walker534 fun) (walker534 arg) (\_ -> walker534 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker534 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker534 body) Nothing (\inner -> walker534 inner))
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
          in (Optionals.cases (walker535 fun) (walker535 arg) (\_ -> walker535 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker535 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker535 body) Nothing (\inner -> walker535 inner))
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
          in (Optionals.cases (walker536 fun) (walker536 arg) (\_ -> walker536 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker536 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker536 body) Nothing (\inner -> walker536 inner))
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
          in (Optionals.cases (walker537 fun) (walker537 arg) (\_ -> walker537 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker537 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker537 body) Nothing (\inner -> walker537 inner))
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
          in (Optionals.cases (walker538 fun) (walker538 arg) (\_ -> walker538 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker538 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker538 body) Nothing (\inner -> walker538 inner))
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
          in (Optionals.cases (walker53 fun) (walker53 arg) (\_ -> walker53 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker53 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker53 body) Nothing (\inner -> walker53 inner))
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
          in (Optionals.cases (walker539 fun) (walker539 arg) (\_ -> walker539 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker539 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker539 body) Nothing (\inner -> walker539 inner))
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
          in (Optionals.cases (walker540 fun) (walker540 arg) (\_ -> walker540 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker540 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker540 body) Nothing (\inner -> walker540 inner))
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
          in (Optionals.cases (walker541 fun) (walker541 arg) (\_ -> walker541 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker541 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker541 body) Nothing (\inner -> walker541 inner))
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
          in (Optionals.cases (walker542 fun) (walker542 arg) (\_ -> walker542 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker542 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker542 body) Nothing (\inner -> walker542 inner))
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
          in (Optionals.cases (walker543 fun) (walker543 arg) (\_ -> walker543 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker543 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker543 body) Nothing (\inner -> walker543 inner))
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
          in (Optionals.cases (walker544 fun) (walker544 arg) (\_ -> walker544 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker544 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker544 body) Nothing (\inner -> walker544 inner))
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
          in (Optionals.cases (walker545 fun) (walker545 arg) (\_ -> walker545 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker545 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker545 body) Nothing (\inner -> walker545 inner))
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
          in (Optionals.cases (walker546 fun) (walker546 arg) (\_ -> walker546 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker546 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker546 body) Nothing (\inner -> walker546 inner))
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
          in (Optionals.cases (walker547 fun) (walker547 arg) (\_ -> walker547 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker547 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker547 body) Nothing (\inner -> walker547 inner))
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
          in (Optionals.cases (walker548 fun) (walker548 arg) (\_ -> walker548 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker548 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker548 body) Nothing (\inner -> walker548 inner))
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
          in (Optionals.cases (walker54 fun) (walker54 arg) (\_ -> walker54 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker54 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker54 body) Nothing (\inner -> walker54 inner))
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
          in (Optionals.cases (walker549 fun) (walker549 arg) (\_ -> walker549 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker549 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker549 body) Nothing (\inner -> walker549 inner))
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
          in (Optionals.cases (walker550 fun) (walker550 arg) (\_ -> walker550 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker550 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker550 body) Nothing (\inner -> walker550 inner))
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
          in (Optionals.cases (walker551 fun) (walker551 arg) (\_ -> walker551 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker551 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker551 body) Nothing (\inner -> walker551 inner))
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
          in (Optionals.cases (walker552 fun) (walker552 arg) (\_ -> walker552 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker552 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker552 body) Nothing (\inner -> walker552 inner))
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
          in (Optionals.cases (walker553 fun) (walker553 arg) (\_ -> walker553 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker553 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker553 body) Nothing (\inner -> walker553 inner))
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
          in (Optionals.cases (walker554 fun) (walker554 arg) (\_ -> walker554 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker554 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker554 body) Nothing (\inner -> walker554 inner))
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
          in (Optionals.cases (walker555 fun) (walker555 arg) (\_ -> walker555 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker555 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker555 body) Nothing (\inner -> walker555 inner))
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
          in (Optionals.cases (walker556 fun) (walker556 arg) (\_ -> walker556 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker556 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker556 body) Nothing (\inner -> walker556 inner))
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
          in (Optionals.cases (walker557 fun) (walker557 arg) (\_ -> walker557 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker557 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker557 body) Nothing (\inner -> walker557 inner))
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
          in (Optionals.cases (walker558 fun) (walker558 arg) (\_ -> walker558 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker558 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker558 body) Nothing (\inner -> walker558 inner))
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
          in (Optionals.cases (walker55 fun) (walker55 arg) (\_ -> walker55 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker55 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker55 body) Nothing (\inner -> walker55 inner))
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
          in (Optionals.cases (walker559 fun) (walker559 arg) (\_ -> walker559 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker559 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker559 body) Nothing (\inner -> walker559 inner))
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
          in (Optionals.cases (walker560 fun) (walker560 arg) (\_ -> walker560 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker560 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker560 body) Nothing (\inner -> walker560 inner))
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
          in (Optionals.cases (walker561 fun) (walker561 arg) (\_ -> walker561 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker561 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker561 body) Nothing (\inner -> walker561 inner))
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
          in (Optionals.cases (walker562 fun) (walker562 arg) (\_ -> walker562 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker562 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker562 body) Nothing (\inner -> walker562 inner))
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
          in (Optionals.cases (walker563 fun) (walker563 arg) (\_ -> walker563 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker563 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker563 body) Nothing (\inner -> walker563 inner))
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
          in (Optionals.cases (walker564 fun) (walker564 arg) (\_ -> walker564 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker564 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker564 body) Nothing (\inner -> walker564 inner))
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
          in (Optionals.cases (walker565 fun) (walker565 arg) (\_ -> walker565 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker565 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker565 body) Nothing (\inner -> walker565 inner))
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
          in (Optionals.cases (walker566 fun) (walker566 arg) (\_ -> walker566 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker566 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker566 body) Nothing (\inner -> walker566 inner))
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
          in (Optionals.cases (walker567 fun) (walker567 arg) (\_ -> walker567 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker567 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker567 body) Nothing (\inner -> walker567 inner))
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
          in (Optionals.cases (walker568 fun) (walker568 arg) (\_ -> walker568 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker568 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker568 body) Nothing (\inner -> walker568 inner))
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
          in (Optionals.cases (walker56 fun) (walker56 arg) (\_ -> walker56 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker56 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker56 body) Nothing (\inner -> walker56 inner))
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
          in (Optionals.cases (walker569 fun) (walker569 arg) (\_ -> walker569 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker569 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker569 body) Nothing (\inner -> walker569 inner))
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
          in (Optionals.cases (walker570 fun) (walker570 arg) (\_ -> walker570 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker570 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker570 body) Nothing (\inner -> walker570 inner))
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
          in (Optionals.cases (walker571 fun) (walker571 arg) (\_ -> walker571 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker571 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker571 body) Nothing (\inner -> walker571 inner))
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
          in (Optionals.cases (walker572 fun) (walker572 arg) (\_ -> walker572 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker572 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker572 body) Nothing (\inner -> walker572 inner))
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
          in (Optionals.cases (walker573 fun) (walker573 arg) (\_ -> walker573 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker573 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker573 body) Nothing (\inner -> walker573 inner))
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
          in (Optionals.cases (walker574 fun) (walker574 arg) (\_ -> walker574 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker574 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker574 body) Nothing (\inner -> walker574 inner))
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
          in (Optionals.cases (walker575 fun) (walker575 arg) (\_ -> walker575 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker575 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker575 body) Nothing (\inner -> walker575 inner))
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
          in (Optionals.cases (walker576 fun) (walker576 arg) (\_ -> walker576 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker576 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker576 body) Nothing (\inner -> walker576 inner))
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
          in (Optionals.cases (walker577 fun) (walker577 arg) (\_ -> walker577 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker577 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker577 body) Nothing (\inner -> walker577 inner))
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
          in (Optionals.cases (walker578 fun) (walker578 arg) (\_ -> walker578 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker578 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker578 body) Nothing (\inner -> walker578 inner))
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
          in (Optionals.cases (walker57 fun) (walker57 arg) (\_ -> walker57 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker57 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker57 body) Nothing (\inner -> walker57 inner))
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
          in (Optionals.cases (walker579 fun) (walker579 arg) (\_ -> walker579 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker579 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker579 body) Nothing (\inner -> walker579 inner))
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
          in (Optionals.cases (walker580 fun) (walker580 arg) (\_ -> walker580 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker580 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker580 body) Nothing (\inner -> walker580 inner))
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
          in (Optionals.cases (walker581 fun) (walker581 arg) (\_ -> walker581 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker581 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker581 body) Nothing (\inner -> walker581 inner))
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
          in (Optionals.cases (walker582 fun) (walker582 arg) (\_ -> walker582 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker582 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker582 body) Nothing (\inner -> walker582 inner))
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
          in (Optionals.cases (walker583 fun) (walker583 arg) (\_ -> walker583 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker583 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker583 body) Nothing (\inner -> walker583 inner))
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
          in (Optionals.cases (walker584 fun) (walker584 arg) (\_ -> walker584 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker584 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker584 body) Nothing (\inner -> walker584 inner))
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
          in (Optionals.cases (walker585 fun) (walker585 arg) (\_ -> walker585 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker585 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker585 body) Nothing (\inner -> walker585 inner))
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
          in (Optionals.cases (walker586 fun) (walker586 arg) (\_ -> walker586 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker586 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker586 body) Nothing (\inner -> walker586 inner))
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
          in (Optionals.cases (walker587 fun) (walker587 arg) (\_ -> walker587 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker587 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker587 body) Nothing (\inner -> walker587 inner))
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
          in (Optionals.cases (walker588 fun) (walker588 arg) (\_ -> walker588 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker588 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker588 body) Nothing (\inner -> walker588 inner))
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
          in (Optionals.cases (walker58 fun) (walker58 arg) (\_ -> walker58 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker58 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker58 body) Nothing (\inner -> walker58 inner))
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
          in (Optionals.cases (walker589 fun) (walker589 arg) (\_ -> walker589 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker589 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker589 body) Nothing (\inner -> walker589 inner))
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
          in (Optionals.cases (walker590 fun) (walker590 arg) (\_ -> walker590 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker590 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker590 body) Nothing (\inner -> walker590 inner))
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
          in (Optionals.cases (walker591 fun) (walker591 arg) (\_ -> walker591 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker591 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker591 body) Nothing (\inner -> walker591 inner))
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
          in (Optionals.cases (walker592 fun) (walker592 arg) (\_ -> walker592 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker592 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker592 body) Nothing (\inner -> walker592 inner))
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
          in (Optionals.cases (walker593 fun) (walker593 arg) (\_ -> walker593 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker593 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker593 body) Nothing (\inner -> walker593 inner))
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
          in (Optionals.cases (walker594 fun) (walker594 arg) (\_ -> walker594 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker594 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker594 body) Nothing (\inner -> walker594 inner))
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
          in (Optionals.cases (walker595 fun) (walker595 arg) (\_ -> walker595 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker595 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker595 body) Nothing (\inner -> walker595 inner))
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
          in (Optionals.cases (walker596 fun) (walker596 arg) (\_ -> walker596 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker596 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker596 body) Nothing (\inner -> walker596 inner))
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
          in (Optionals.cases (walker597 fun) (walker597 arg) (\_ -> walker597 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker597 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker597 body) Nothing (\inner -> walker597 inner))
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
          in (Optionals.cases (walker598 fun) (walker598 arg) (\_ -> walker598 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker598 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker598 body) Nothing (\inner -> walker598 inner))
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
          in (Optionals.cases (walker5 fun) (walker5 arg) (\_ -> walker5 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker5 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker5 body) Nothing (\inner -> walker5 inner))
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
          in (Optionals.cases (walker59 fun) (walker59 arg) (\_ -> walker59 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker59 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker59 body) Nothing (\inner -> walker59 inner))
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
          in (Optionals.cases (walker599 fun) (walker599 arg) (\_ -> walker599 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker599 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker599 body) Nothing (\inner -> walker599 inner))
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
          in (Optionals.cases (walker600 fun) (walker600 arg) (\_ -> walker600 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker600 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker600 body) Nothing (\inner -> walker600 inner))
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
          in (Optionals.cases (walker601 fun) (walker601 arg) (\_ -> walker601 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker601 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker601 body) Nothing (\inner -> walker601 inner))
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
          in (Optionals.cases (walker602 fun) (walker602 arg) (\_ -> walker602 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker602 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker602 body) Nothing (\inner -> walker602 inner))
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
          in (Optionals.cases (walker603 fun) (walker603 arg) (\_ -> walker603 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker603 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker603 body) Nothing (\inner -> walker603 inner))
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
          in (Optionals.cases (walker604 fun) (walker604 arg) (\_ -> walker604 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker604 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker604 body) Nothing (\inner -> walker604 inner))
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
          in (Optionals.cases (walker605 fun) (walker605 arg) (\_ -> walker605 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker605 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker605 body) Nothing (\inner -> walker605 inner))
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
          in (Optionals.cases (walker606 fun) (walker606 arg) (\_ -> walker606 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker606 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker606 body) Nothing (\inner -> walker606 inner))
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
          in (Optionals.cases (walker607 fun) (walker607 arg) (\_ -> walker607 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker607 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker607 body) Nothing (\inner -> walker607 inner))
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
          in (Optionals.cases (walker608 fun) (walker608 arg) (\_ -> walker608 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker608 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker608 body) Nothing (\inner -> walker608 inner))
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
          in (Optionals.cases (walker60 fun) (walker60 arg) (\_ -> walker60 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker60 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker60 body) Nothing (\inner -> walker60 inner))
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
          in (Optionals.cases (walker609 fun) (walker609 arg) (\_ -> walker609 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker609 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker609 body) Nothing (\inner -> walker609 inner))
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
          in (Optionals.cases (walker610 fun) (walker610 arg) (\_ -> walker610 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker610 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker610 body) Nothing (\inner -> walker610 inner))
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
          in (Optionals.cases (walker611 fun) (walker611 arg) (\_ -> walker611 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker611 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker611 body) Nothing (\inner -> walker611 inner))
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
          in (Optionals.cases (walker612 fun) (walker612 arg) (\_ -> walker612 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker612 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker612 body) Nothing (\inner -> walker612 inner))
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
          in (Optionals.cases (walker613 fun) (walker613 arg) (\_ -> walker613 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker613 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker613 body) Nothing (\inner -> walker613 inner))
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
          in (Optionals.cases (walker614 fun) (walker614 arg) (\_ -> walker614 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker614 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker614 body) Nothing (\inner -> walker614 inner))
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
          in (Optionals.cases (walker615 fun) (walker615 arg) (\_ -> walker615 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker615 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker615 body) Nothing (\inner -> walker615 inner))
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
          in (Optionals.cases (walker616 fun) (walker616 arg) (\_ -> walker616 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker616 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker616 body) Nothing (\inner -> walker616 inner))
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
          in (Optionals.cases (walker617 fun) (walker617 arg) (\_ -> walker617 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker617 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker617 body) Nothing (\inner -> walker617 inner))
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
          in (Optionals.cases (walker618 fun) (walker618 arg) (\_ -> walker618 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker618 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker618 body) Nothing (\inner -> walker618 inner))
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
          in (Optionals.cases (walker61 fun) (walker61 arg) (\_ -> walker61 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker61 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker61 body) Nothing (\inner -> walker61 inner))
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
          in (Optionals.cases (walker619 fun) (walker619 arg) (\_ -> walker619 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker619 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker619 body) Nothing (\inner -> walker619 inner))
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
          in (Optionals.cases (walker620 fun) (walker620 arg) (\_ -> walker620 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker620 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker620 body) Nothing (\inner -> walker620 inner))
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
          in (Optionals.cases (walker621 fun) (walker621 arg) (\_ -> walker621 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker621 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker621 body) Nothing (\inner -> walker621 inner))
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
          in (Optionals.cases (walker622 fun) (walker622 arg) (\_ -> walker622 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker622 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker622 body) Nothing (\inner -> walker622 inner))
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
          in (Optionals.cases (walker623 fun) (walker623 arg) (\_ -> walker623 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker623 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker623 body) Nothing (\inner -> walker623 inner))
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
          in (Optionals.cases (walker624 fun) (walker624 arg) (\_ -> walker624 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker624 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker624 body) Nothing (\inner -> walker624 inner))
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
          in (Optionals.cases (walker625 fun) (walker625 arg) (\_ -> walker625 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker625 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker625 body) Nothing (\inner -> walker625 inner))
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
          in (Optionals.cases (walker626 fun) (walker626 arg) (\_ -> walker626 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker626 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker626 body) Nothing (\inner -> walker626 inner))
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
          in (Optionals.cases (walker627 fun) (walker627 arg) (\_ -> walker627 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker627 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker627 body) Nothing (\inner -> walker627 inner))
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
          in (Optionals.cases (walker628 fun) (walker628 arg) (\_ -> walker628 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker628 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker628 body) Nothing (\inner -> walker628 inner))
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
          in (Optionals.cases (walker62 fun) (walker62 arg) (\_ -> walker62 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker62 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker62 body) Nothing (\inner -> walker62 inner))
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
          in (Optionals.cases (walker629 fun) (walker629 arg) (\_ -> walker629 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker629 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker629 body) Nothing (\inner -> walker629 inner))
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
          in (Optionals.cases (walker630 fun) (walker630 arg) (\_ -> walker630 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker630 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker630 body) Nothing (\inner -> walker630 inner))
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
          in (Optionals.cases (walker631 fun) (walker631 arg) (\_ -> walker631 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker631 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker631 body) Nothing (\inner -> walker631 inner))
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
          in (Optionals.cases (walker632 fun) (walker632 arg) (\_ -> walker632 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker632 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker632 body) Nothing (\inner -> walker632 inner))
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
          in (Optionals.cases (walker633 fun) (walker633 arg) (\_ -> walker633 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker633 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker633 body) Nothing (\inner -> walker633 inner))
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
          in (Optionals.cases (walker634 fun) (walker634 arg) (\_ -> walker634 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker634 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker634 body) Nothing (\inner -> walker634 inner))
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
          in (Optionals.cases (walker635 fun) (walker635 arg) (\_ -> walker635 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker635 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker635 body) Nothing (\inner -> walker635 inner))
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
          in (Optionals.cases (walker636 fun) (walker636 arg) (\_ -> walker636 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker636 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker636 body) Nothing (\inner -> walker636 inner))
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
          in (Optionals.cases (walker637 fun) (walker637 arg) (\_ -> walker637 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker637 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker637 body) Nothing (\inner -> walker637 inner))
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
          in (Optionals.cases (walker638 fun) (walker638 arg) (\_ -> walker638 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker638 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker638 body) Nothing (\inner -> walker638 inner))
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
          in (Optionals.cases (walker63 fun) (walker63 arg) (\_ -> walker63 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker63 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker63 body) Nothing (\inner -> walker63 inner))
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
          in (Optionals.cases (walker639 fun) (walker639 arg) (\_ -> walker639 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker639 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker639 body) Nothing (\inner -> walker639 inner))
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
          in (Optionals.cases (walker640 fun) (walker640 arg) (\_ -> walker640 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker640 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker640 body) Nothing (\inner -> walker640 inner))
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
          in (Optionals.cases (walker641 fun) (walker641 arg) (\_ -> walker641 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker641 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker641 body) Nothing (\inner -> walker641 inner))
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
          in (Optionals.cases (walker642 fun) (walker642 arg) (\_ -> walker642 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker642 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker642 body) Nothing (\inner -> walker642 inner))
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
          in (Optionals.cases (walker643 fun) (walker643 arg) (\_ -> walker643 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker643 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker643 body) Nothing (\inner -> walker643 inner))
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
          in (Optionals.cases (walker644 fun) (walker644 arg) (\_ -> walker644 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker644 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker644 body) Nothing (\inner -> walker644 inner))
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
          in (Optionals.cases (walker645 fun) (walker645 arg) (\_ -> walker645 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker645 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker645 body) Nothing (\inner -> walker645 inner))
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
          in (Optionals.cases (walker646 fun) (walker646 arg) (\_ -> walker646 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker646 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker646 body) Nothing (\inner -> walker646 inner))
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
          in (Optionals.cases (walker647 fun) (walker647 arg) (\_ -> walker647 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker647 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker647 body) Nothing (\inner -> walker647 inner))
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
          in (Optionals.cases (walker648 fun) (walker648 arg) (\_ -> walker648 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker648 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker648 body) Nothing (\inner -> walker648 inner))
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
          in (Optionals.cases (walker64 fun) (walker64 arg) (\_ -> walker64 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker64 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker64 body) Nothing (\inner -> walker64 inner))
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
          in (Optionals.cases (walker649 fun) (walker649 arg) (\_ -> walker649 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker649 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker649 body) Nothing (\inner -> walker649 inner))
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
          in (Optionals.cases (walker650 fun) (walker650 arg) (\_ -> walker650 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker650 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker650 body) Nothing (\inner -> walker650 inner))
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
          in (Optionals.cases (walker651 fun) (walker651 arg) (\_ -> walker651 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker651 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker651 body) Nothing (\inner -> walker651 inner))
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
          in (Optionals.cases (walker652 fun) (walker652 arg) (\_ -> walker652 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker652 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker652 body) Nothing (\inner -> walker652 inner))
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
          in (Optionals.cases (walker653 fun) (walker653 arg) (\_ -> walker653 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker653 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker653 body) Nothing (\inner -> walker653 inner))
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
          in (Optionals.cases (walker654 fun) (walker654 arg) (\_ -> walker654 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker654 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker654 body) Nothing (\inner -> walker654 inner))
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
          in (Optionals.cases (walker655 fun) (walker655 arg) (\_ -> walker655 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker655 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker655 body) Nothing (\inner -> walker655 inner))
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
          in (Optionals.cases (walker656 fun) (walker656 arg) (\_ -> walker656 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker656 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker656 body) Nothing (\inner -> walker656 inner))
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
          in (Optionals.cases (walker657 fun) (walker657 arg) (\_ -> walker657 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker657 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker657 body) Nothing (\inner -> walker657 inner))
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
          in (Optionals.cases (walker658 fun) (walker658 arg) (\_ -> walker658 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker658 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker658 body) Nothing (\inner -> walker658 inner))
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
          in (Optionals.cases (walker65 fun) (walker65 arg) (\_ -> walker65 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker65 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker65 body) Nothing (\inner -> walker65 inner))
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
          in (Optionals.cases (walker659 fun) (walker659 arg) (\_ -> walker659 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker659 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker659 body) Nothing (\inner -> walker659 inner))
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
          in (Optionals.cases (walker660 fun) (walker660 arg) (\_ -> walker660 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker660 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker660 body) Nothing (\inner -> walker660 inner))
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
          in (Optionals.cases (walker661 fun) (walker661 arg) (\_ -> walker661 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker661 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker661 body) Nothing (\inner -> walker661 inner))
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
          in (Optionals.cases (walker662 fun) (walker662 arg) (\_ -> walker662 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker662 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker662 body) Nothing (\inner -> walker662 inner))
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
          in (Optionals.cases (walker663 fun) (walker663 arg) (\_ -> walker663 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker663 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker663 body) Nothing (\inner -> walker663 inner))
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
          in (Optionals.cases (walker664 fun) (walker664 arg) (\_ -> walker664 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker664 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker664 body) Nothing (\inner -> walker664 inner))
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
          in (Optionals.cases (walker665 fun) (walker665 arg) (\_ -> walker665 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker665 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker665 body) Nothing (\inner -> walker665 inner))
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
          in (Optionals.cases (walker666 fun) (walker666 arg) (\_ -> walker666 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker666 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker666 body) Nothing (\inner -> walker666 inner))
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
          in (Optionals.cases (walker667 fun) (walker667 arg) (\_ -> walker667 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker667 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker667 body) Nothing (\inner -> walker667 inner))
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
          in (Optionals.cases (walker668 fun) (walker668 arg) (\_ -> walker668 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker668 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker668 body) Nothing (\inner -> walker668 inner))
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
          in (Optionals.cases (walker66 fun) (walker66 arg) (\_ -> walker66 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker66 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker66 body) Nothing (\inner -> walker66 inner))
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
          in (Optionals.cases (walker669 fun) (walker669 arg) (\_ -> walker669 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker669 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker669 body) Nothing (\inner -> walker669 inner))
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
          in (Optionals.cases (walker670 fun) (walker670 arg) (\_ -> walker670 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker670 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker670 body) Nothing (\inner -> walker670 inner))
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
          in (Optionals.cases (walker671 fun) (walker671 arg) (\_ -> walker671 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker671 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker671 body) Nothing (\inner -> walker671 inner))
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
          in (Optionals.cases (walker672 fun) (walker672 arg) (\_ -> walker672 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker672 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker672 body) Nothing (\inner -> walker672 inner))
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
          in (Optionals.cases (walker673 fun) (walker673 arg) (\_ -> walker673 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker673 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker673 body) Nothing (\inner -> walker673 inner))
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
          in (Optionals.cases (walker674 fun) (walker674 arg) (\_ -> walker674 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker674 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker674 body) Nothing (\inner -> walker674 inner))
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
          in (Optionals.cases (walker675 fun) (walker675 arg) (\_ -> walker675 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker675 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker675 body) Nothing (\inner -> walker675 inner))
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
          in (Optionals.cases (walker676 fun) (walker676 arg) (\_ -> walker676 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker676 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker676 body) Nothing (\inner -> walker676 inner))
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
          in (Optionals.cases (walker677 fun) (walker677 arg) (\_ -> walker677 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker677 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker677 body) Nothing (\inner -> walker677 inner))
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
          in (Optionals.cases (walker678 fun) (walker678 arg) (\_ -> walker678 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker678 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker678 body) Nothing (\inner -> walker678 inner))
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
          in (Optionals.cases (walker67 fun) (walker67 arg) (\_ -> walker67 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker67 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker67 body) Nothing (\inner -> walker67 inner))
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
          in (Optionals.cases (walker679 fun) (walker679 arg) (\_ -> walker679 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker679 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker679 body) Nothing (\inner -> walker679 inner))
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
          in (Optionals.cases (walker680 fun) (walker680 arg) (\_ -> walker680 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker680 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker680 body) Nothing (\inner -> walker680 inner))
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
          in (Optionals.cases (walker681 fun) (walker681 arg) (\_ -> walker681 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker681 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker681 body) Nothing (\inner -> walker681 inner))
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
          in (Optionals.cases (walker682 fun) (walker682 arg) (\_ -> walker682 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker682 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker682 body) Nothing (\inner -> walker682 inner))
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
          in (Optionals.cases (walker683 fun) (walker683 arg) (\_ -> walker683 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker683 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker683 body) Nothing (\inner -> walker683 inner))
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
          in (Optionals.cases (walker684 fun) (walker684 arg) (\_ -> walker684 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker684 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker684 body) Nothing (\inner -> walker684 inner))
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
          in (Optionals.cases (walker685 fun) (walker685 arg) (\_ -> walker685 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker685 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker685 body) Nothing (\inner -> walker685 inner))
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
          in (Optionals.cases (walker686 fun) (walker686 arg) (\_ -> walker686 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker686 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker686 body) Nothing (\inner -> walker686 inner))
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
          in (Optionals.cases (walker687 fun) (walker687 arg) (\_ -> walker687 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker687 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker687 body) Nothing (\inner -> walker687 inner))
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
          in (Optionals.cases (walker688 fun) (walker688 arg) (\_ -> walker688 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker688 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker688 body) Nothing (\inner -> walker688 inner))
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
          in (Optionals.cases (walker68 fun) (walker68 arg) (\_ -> walker68 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker68 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker68 body) Nothing (\inner -> walker68 inner))
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
          in (Optionals.cases (walker689 fun) (walker689 arg) (\_ -> walker689 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker689 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker689 body) Nothing (\inner -> walker689 inner))
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
          in (Optionals.cases (walker690 fun) (walker690 arg) (\_ -> walker690 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker690 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker690 body) Nothing (\inner -> walker690 inner))
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
          in (Optionals.cases (walker691 fun) (walker691 arg) (\_ -> walker691 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker691 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker691 body) Nothing (\inner -> walker691 inner))
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
          in (Optionals.cases (walker692 fun) (walker692 arg) (\_ -> walker692 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker692 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker692 body) Nothing (\inner -> walker692 inner))
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
          in (Optionals.cases (walker693 fun) (walker693 arg) (\_ -> walker693 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker693 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker693 body) Nothing (\inner -> walker693 inner))
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
          in (Optionals.cases (walker694 fun) (walker694 arg) (\_ -> walker694 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker694 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker694 body) Nothing (\inner -> walker694 inner))
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
          in (Optionals.cases (walker695 fun) (walker695 arg) (\_ -> walker695 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker695 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker695 body) Nothing (\inner -> walker695 inner))
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
          in (Optionals.cases (walker696 fun) (walker696 arg) (\_ -> walker696 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker696 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker696 body) Nothing (\inner -> walker696 inner))
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
          in (Optionals.cases (walker697 fun) (walker697 arg) (\_ -> walker697 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker697 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker697 body) Nothing (\inner -> walker697 inner))
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
          in (Optionals.cases (walker698 fun) (walker698 arg) (\_ -> walker698 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker698 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker698 body) Nothing (\inner -> walker698 inner))
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
          in (Optionals.cases (walker6 fun) (walker6 arg) (\_ -> walker6 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker6 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker6 body) Nothing (\inner -> walker6 inner))
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
          in (Optionals.cases (walker69 fun) (walker69 arg) (\_ -> walker69 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker69 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker69 body) Nothing (\inner -> walker69 inner))
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
          in (Optionals.cases (walker699 fun) (walker699 arg) (\_ -> walker699 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker699 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker699 body) Nothing (\inner -> walker699 inner))
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
          in (Optionals.cases (walker700 fun) (walker700 arg) (\_ -> walker700 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker700 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker700 body) Nothing (\inner -> walker700 inner))
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
          in (Optionals.cases (walker701 fun) (walker701 arg) (\_ -> walker701 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker701 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker701 body) Nothing (\inner -> walker701 inner))
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
          in (Optionals.cases (walker702 fun) (walker702 arg) (\_ -> walker702 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker702 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker702 body) Nothing (\inner -> walker702 inner))
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
          in (Optionals.cases (walker703 fun) (walker703 arg) (\_ -> walker703 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker703 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker703 body) Nothing (\inner -> walker703 inner))
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
          in (Optionals.cases (walker704 fun) (walker704 arg) (\_ -> walker704 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker704 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker704 body) Nothing (\inner -> walker704 inner))
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
          in (Optionals.cases (walker705 fun) (walker705 arg) (\_ -> walker705 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker705 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker705 body) Nothing (\inner -> walker705 inner))
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
          in (Optionals.cases (walker706 fun) (walker706 arg) (\_ -> walker706 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker706 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker706 body) Nothing (\inner -> walker706 inner))
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
          in (Optionals.cases (walker707 fun) (walker707 arg) (\_ -> walker707 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker707 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker707 body) Nothing (\inner -> walker707 inner))
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
          in (Optionals.cases (walker708 fun) (walker708 arg) (\_ -> walker708 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker708 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker708 body) Nothing (\inner -> walker708 inner))
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
          in (Optionals.cases (walker70 fun) (walker70 arg) (\_ -> walker70 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker70 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker70 body) Nothing (\inner -> walker70 inner))
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
          in (Optionals.cases (walker709 fun) (walker709 arg) (\_ -> walker709 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker709 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker709 body) Nothing (\inner -> walker709 inner))
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
          in (Optionals.cases (walker710 fun) (walker710 arg) (\_ -> walker710 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker710 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker710 body) Nothing (\inner -> walker710 inner))
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
          in (Optionals.cases (walker711 fun) (walker711 arg) (\_ -> walker711 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker711 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker711 body) Nothing (\inner -> walker711 inner))
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
          in (Optionals.cases (walker712 fun) (walker712 arg) (\_ -> walker712 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker712 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker712 body) Nothing (\inner -> walker712 inner))
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
          in (Optionals.cases (walker713 fun) (walker713 arg) (\_ -> walker713 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker713 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker713 body) Nothing (\inner -> walker713 inner))
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
          in (Optionals.cases (walker714 fun) (walker714 arg) (\_ -> walker714 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker714 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker714 body) Nothing (\inner -> walker714 inner))
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
          in (Optionals.cases (walker715 fun) (walker715 arg) (\_ -> walker715 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker715 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker715 body) Nothing (\inner -> walker715 inner))
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
          in (Optionals.cases (walker716 fun) (walker716 arg) (\_ -> walker716 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker716 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker716 body) Nothing (\inner -> walker716 inner))
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
          in (Optionals.cases (walker717 fun) (walker717 arg) (\_ -> walker717 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker717 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker717 body) Nothing (\inner -> walker717 inner))
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
          in (Optionals.cases (walker718 fun) (walker718 arg) (\_ -> walker718 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker718 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker718 body) Nothing (\inner -> walker718 inner))
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
          in (Optionals.cases (walker71 fun) (walker71 arg) (\_ -> walker71 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker71 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker71 body) Nothing (\inner -> walker71 inner))
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
          in (Optionals.cases (walker719 fun) (walker719 arg) (\_ -> walker719 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker719 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker719 body) Nothing (\inner -> walker719 inner))
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
          in (Optionals.cases (walker720 fun) (walker720 arg) (\_ -> walker720 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker720 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker720 body) Nothing (\inner -> walker720 inner))
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
          in (Optionals.cases (walker721 fun) (walker721 arg) (\_ -> walker721 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker721 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker721 body) Nothing (\inner -> walker721 inner))
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
          in (Optionals.cases (walker722 fun) (walker722 arg) (\_ -> walker722 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker722 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker722 body) Nothing (\inner -> walker722 inner))
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
          in (Optionals.cases (walker723 fun) (walker723 arg) (\_ -> walker723 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker723 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker723 body) Nothing (\inner -> walker723 inner))
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
          in (Optionals.cases (walker724 fun) (walker724 arg) (\_ -> walker724 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker724 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker724 body) Nothing (\inner -> walker724 inner))
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
          in (Optionals.cases (walker725 fun) (walker725 arg) (\_ -> walker725 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker725 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker725 body) Nothing (\inner -> walker725 inner))
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
          in (Optionals.cases (walker726 fun) (walker726 arg) (\_ -> walker726 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker726 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker726 body) Nothing (\inner -> walker726 inner))
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
          in (Optionals.cases (walker727 fun) (walker727 arg) (\_ -> walker727 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker727 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker727 body) Nothing (\inner -> walker727 inner))
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
          in (Optionals.cases (walker728 fun) (walker728 arg) (\_ -> walker728 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker728 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker728 body) Nothing (\inner -> walker728 inner))
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
          in (Optionals.cases (walker72 fun) (walker72 arg) (\_ -> walker72 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker72 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker72 body) Nothing (\inner -> walker72 inner))
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
          in (Optionals.cases (walker729 fun) (walker729 arg) (\_ -> walker729 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker729 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker729 body) Nothing (\inner -> walker729 inner))
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
          in (Optionals.cases (walker730 fun) (walker730 arg) (\_ -> walker730 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker730 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker730 body) Nothing (\inner -> walker730 inner))
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
          in (Optionals.cases (walker731 fun) (walker731 arg) (\_ -> walker731 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker731 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker731 body) Nothing (\inner -> walker731 inner))
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
          in (Optionals.cases (walker732 fun) (walker732 arg) (\_ -> walker732 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker732 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker732 body) Nothing (\inner -> walker732 inner))
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
          in (Optionals.cases (walker733 fun) (walker733 arg) (\_ -> walker733 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker733 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker733 body) Nothing (\inner -> walker733 inner))
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
          in (Optionals.cases (walker734 fun) (walker734 arg) (\_ -> walker734 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker734 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker734 body) Nothing (\inner -> walker734 inner))
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
          in (Optionals.cases (walker735 fun) (walker735 arg) (\_ -> walker735 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker735 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker735 body) Nothing (\inner -> walker735 inner))
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
          in (Optionals.cases (walker736 fun) (walker736 arg) (\_ -> walker736 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker736 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker736 body) Nothing (\inner -> walker736 inner))
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
          in (Optionals.cases (walker737 fun) (walker737 arg) (\_ -> walker737 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker737 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker737 body) Nothing (\inner -> walker737 inner))
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
          in (Optionals.cases (walker738 fun) (walker738 arg) (\_ -> walker738 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker738 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker738 body) Nothing (\inner -> walker738 inner))
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
          in (Optionals.cases (walker73 fun) (walker73 arg) (\_ -> walker73 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker73 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker73 body) Nothing (\inner -> walker73 inner))
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
          in (Optionals.cases (walker739 fun) (walker739 arg) (\_ -> walker739 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker739 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker739 body) Nothing (\inner -> walker739 inner))
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
          in (Optionals.cases (walker740 fun) (walker740 arg) (\_ -> walker740 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker740 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker740 body) Nothing (\inner -> walker740 inner))
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
          in (Optionals.cases (walker741 fun) (walker741 arg) (\_ -> walker741 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker741 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker741 body) Nothing (\inner -> walker741 inner))
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
          in (Optionals.cases (walker742 fun) (walker742 arg) (\_ -> walker742 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker742 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker742 body) Nothing (\inner -> walker742 inner))
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
          in (Optionals.cases (walker743 fun) (walker743 arg) (\_ -> walker743 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker743 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker743 body) Nothing (\inner -> walker743 inner))
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
          in (Optionals.cases (walker744 fun) (walker744 arg) (\_ -> walker744 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker744 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker744 body) Nothing (\inner -> walker744 inner))
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
          in (Optionals.cases (walker745 fun) (walker745 arg) (\_ -> walker745 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker745 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker745 body) Nothing (\inner -> walker745 inner))
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
          in (Optionals.cases (walker746 fun) (walker746 arg) (\_ -> walker746 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker746 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker746 body) Nothing (\inner -> walker746 inner))
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
          in (Optionals.cases (walker747 fun) (walker747 arg) (\_ -> walker747 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker747 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker747 body) Nothing (\inner -> walker747 inner))
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
          in (Optionals.cases (walker748 fun) (walker748 arg) (\_ -> walker748 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker748 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker748 body) Nothing (\inner -> walker748 inner))
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
          in (Optionals.cases (walker74 fun) (walker74 arg) (\_ -> walker74 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker74 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker74 body) Nothing (\inner -> walker74 inner))
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
          in (Optionals.cases (walker749 fun) (walker749 arg) (\_ -> walker749 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker749 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker749 body) Nothing (\inner -> walker749 inner))
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
          in (Optionals.cases (walker750 fun) (walker750 arg) (\_ -> walker750 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker750 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker750 body) Nothing (\inner -> walker750 inner))
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
          in (Optionals.cases (walker751 fun) (walker751 arg) (\_ -> walker751 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker751 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker751 body) Nothing (\inner -> walker751 inner))
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
          in (Optionals.cases (walker752 fun) (walker752 arg) (\_ -> walker752 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker752 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker752 body) Nothing (\inner -> walker752 inner))
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
          in (Optionals.cases (walker753 fun) (walker753 arg) (\_ -> walker753 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker753 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker753 body) Nothing (\inner -> walker753 inner))
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
          in (Optionals.cases (walker754 fun) (walker754 arg) (\_ -> walker754 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker754 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker754 body) Nothing (\inner -> walker754 inner))
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
          in (Optionals.cases (walker755 fun) (walker755 arg) (\_ -> walker755 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker755 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker755 body) Nothing (\inner -> walker755 inner))
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
          in (Optionals.cases (walker756 fun) (walker756 arg) (\_ -> walker756 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker756 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker756 body) Nothing (\inner -> walker756 inner))
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
          in (Optionals.cases (walker757 fun) (walker757 arg) (\_ -> walker757 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker757 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker757 body) Nothing (\inner -> walker757 inner))
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
          in (Optionals.cases (walker758 fun) (walker758 arg) (\_ -> walker758 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker758 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker758 body) Nothing (\inner -> walker758 inner))
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
          in (Optionals.cases (walker75 fun) (walker75 arg) (\_ -> walker75 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker75 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker75 body) Nothing (\inner -> walker75 inner))
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
          in (Optionals.cases (walker759 fun) (walker759 arg) (\_ -> walker759 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker759 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker759 body) Nothing (\inner -> walker759 inner))
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
          in (Optionals.cases (walker760 fun) (walker760 arg) (\_ -> walker760 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker760 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker760 body) Nothing (\inner -> walker760 inner))
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
          in (Optionals.cases (walker761 fun) (walker761 arg) (\_ -> walker761 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker761 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker761 body) Nothing (\inner -> walker761 inner))
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
          in (Optionals.cases (walker762 fun) (walker762 arg) (\_ -> walker762 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker762 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker762 body) Nothing (\inner -> walker762 inner))
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
          in (Optionals.cases (walker763 fun) (walker763 arg) (\_ -> walker763 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker763 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker763 body) Nothing (\inner -> walker763 inner))
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
          in (Optionals.cases (walker764 fun) (walker764 arg) (\_ -> walker764 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker764 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker764 body) Nothing (\inner -> walker764 inner))
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
          in (Optionals.cases (walker765 fun) (walker765 arg) (\_ -> walker765 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker765 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker765 body) Nothing (\inner -> walker765 inner))
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
          in (Optionals.cases (walker766 fun) (walker766 arg) (\_ -> walker766 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker766 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker766 body) Nothing (\inner -> walker766 inner))
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
          in (Optionals.cases (walker767 fun) (walker767 arg) (\_ -> walker767 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker767 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker767 body) Nothing (\inner -> walker767 inner))
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
          in (Optionals.cases (walker768 fun) (walker768 arg) (\_ -> walker768 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker768 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker768 body) Nothing (\inner -> walker768 inner))
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
          in (Optionals.cases (walker76 fun) (walker76 arg) (\_ -> walker76 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker76 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker76 body) Nothing (\inner -> walker76 inner))
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
          in (Optionals.cases (walker769 fun) (walker769 arg) (\_ -> walker769 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker769 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker769 body) Nothing (\inner -> walker769 inner))
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
          in (Optionals.cases (walker770 fun) (walker770 arg) (\_ -> walker770 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker770 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker770 body) Nothing (\inner -> walker770 inner))
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
          in (Optionals.cases (walker771 fun) (walker771 arg) (\_ -> walker771 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker771 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker771 body) Nothing (\inner -> walker771 inner))
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
          in (Optionals.cases (walker772 fun) (walker772 arg) (\_ -> walker772 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker772 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker772 body) Nothing (\inner -> walker772 inner))
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
          in (Optionals.cases (walker773 fun) (walker773 arg) (\_ -> walker773 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker773 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker773 body) Nothing (\inner -> walker773 inner))
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
          in (Optionals.cases (walker774 fun) (walker774 arg) (\_ -> walker774 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker774 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker774 body) Nothing (\inner -> walker774 inner))
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
          in (Optionals.cases (walker775 fun) (walker775 arg) (\_ -> walker775 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker775 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker775 body) Nothing (\inner -> walker775 inner))
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
          in (Optionals.cases (walker776 fun) (walker776 arg) (\_ -> walker776 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker776 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker776 body) Nothing (\inner -> walker776 inner))
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
          in (Optionals.cases (walker777 fun) (walker777 arg) (\_ -> walker777 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker777 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker777 body) Nothing (\inner -> walker777 inner))
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
          in (Optionals.cases (walker778 fun) (walker778 arg) (\_ -> walker778 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker778 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker778 body) Nothing (\inner -> walker778 inner))
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
          in (Optionals.cases (walker77 fun) (walker77 arg) (\_ -> walker77 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker77 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker77 body) Nothing (\inner -> walker77 inner))
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
          in (Optionals.cases (walker779 fun) (walker779 arg) (\_ -> walker779 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker779 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker779 body) Nothing (\inner -> walker779 inner))
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
          in (Optionals.cases (walker780 fun) (walker780 arg) (\_ -> walker780 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker780 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker780 body) Nothing (\inner -> walker780 inner))
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
          in (Optionals.cases (walker781 fun) (walker781 arg) (\_ -> walker781 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker781 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker781 body) Nothing (\inner -> walker781 inner))
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
          in (Optionals.cases (walker782 fun) (walker782 arg) (\_ -> walker782 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker782 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker782 body) Nothing (\inner -> walker782 inner))
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
          in (Optionals.cases (walker783 fun) (walker783 arg) (\_ -> walker783 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker783 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker783 body) Nothing (\inner -> walker783 inner))
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
          in (Optionals.cases (walker784 fun) (walker784 arg) (\_ -> walker784 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker784 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker784 body) Nothing (\inner -> walker784 inner))
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
          in (Optionals.cases (walker785 fun) (walker785 arg) (\_ -> walker785 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker785 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker785 body) Nothing (\inner -> walker785 inner))
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
          in (Optionals.cases (walker786 fun) (walker786 arg) (\_ -> walker786 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker786 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker786 body) Nothing (\inner -> walker786 inner))
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
          in (Optionals.cases (walker787 fun) (walker787 arg) (\_ -> walker787 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker787 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker787 body) Nothing (\inner -> walker787 inner))
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
          in (Optionals.cases (walker788 fun) (walker788 arg) (\_ -> walker788 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker788 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker788 body) Nothing (\inner -> walker788 inner))
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
          in (Optionals.cases (walker78 fun) (walker78 arg) (\_ -> walker78 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker78 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker78 body) Nothing (\inner -> walker78 inner))
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
          in (Optionals.cases (walker789 fun) (walker789 arg) (\_ -> walker789 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker789 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker789 body) Nothing (\inner -> walker789 inner))
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
          in (Optionals.cases (walker790 fun) (walker790 arg) (\_ -> walker790 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker790 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker790 body) Nothing (\inner -> walker790 inner))
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
          in (Optionals.cases (walker791 fun) (walker791 arg) (\_ -> walker791 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker791 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker791 body) Nothing (\inner -> walker791 inner))
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
          in (Optionals.cases (walker792 fun) (walker792 arg) (\_ -> walker792 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker792 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker792 body) Nothing (\inner -> walker792 inner))
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
          in (Optionals.cases (walker793 fun) (walker793 arg) (\_ -> walker793 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker793 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker793 body) Nothing (\inner -> walker793 inner))
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
          in (Optionals.cases (walker794 fun) (walker794 arg) (\_ -> walker794 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker794 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker794 body) Nothing (\inner -> walker794 inner))
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
          in (Optionals.cases (walker795 fun) (walker795 arg) (\_ -> walker795 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker795 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker795 body) Nothing (\inner -> walker795 inner))
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
          in (Optionals.cases (walker796 fun) (walker796 arg) (\_ -> walker796 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker796 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker796 body) Nothing (\inner -> walker796 inner))
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
          in (Optionals.cases (walker797 fun) (walker797 arg) (\_ -> walker797 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker797 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker797 body) Nothing (\inner -> walker797 inner))
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
          in (Optionals.cases (walker798 fun) (walker798 arg) (\_ -> walker798 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker798 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker798 body) Nothing (\inner -> walker798 inner))
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
          in (Optionals.cases (walker7 fun) (walker7 arg) (\_ -> walker7 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker7 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker7 body) Nothing (\inner -> walker7 inner))
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
          in (Optionals.cases (walker79 fun) (walker79 arg) (\_ -> walker79 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker79 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker79 body) Nothing (\inner -> walker79 inner))
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
          in (Optionals.cases (walker80 fun) (walker80 arg) (\_ -> walker80 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker80 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker80 body) Nothing (\inner -> walker80 inner))
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
          in (Optionals.cases (walker81 fun) (walker81 arg) (\_ -> walker81 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker81 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker81 body) Nothing (\inner -> walker81 inner))
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
          in (Optionals.cases (walker82 fun) (walker82 arg) (\_ -> walker82 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker82 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker82 body) Nothing (\inner -> walker82 inner))
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
          in (Optionals.cases (walker83 fun) (walker83 arg) (\_ -> walker83 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker83 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker83 body) Nothing (\inner -> walker83 inner))
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
          in (Optionals.cases (walker84 fun) (walker84 arg) (\_ -> walker84 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker84 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker84 body) Nothing (\inner -> walker84 inner))
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
          in (Optionals.cases (walker85 fun) (walker85 arg) (\_ -> walker85 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker85 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker85 body) Nothing (\inner -> walker85 inner))
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
          in (Optionals.cases (walker86 fun) (walker86 arg) (\_ -> walker86 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker86 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker86 body) Nothing (\inner -> walker86 inner))
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
          in (Optionals.cases (walker87 fun) (walker87 arg) (\_ -> walker87 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker87 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker87 body) Nothing (\inner -> walker87 inner))
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
          in (Optionals.cases (walker88 fun) (walker88 arg) (\_ -> walker88 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker88 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker88 body) Nothing (\inner -> walker88 inner))
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
          in (Optionals.cases (walker8 fun) (walker8 arg) (\_ -> walker8 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker8 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker8 body) Nothing (\inner -> walker8 inner))
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
          in (Optionals.cases (walker89 fun) (walker89 arg) (\_ -> walker89 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker89 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker89 body) Nothing (\inner -> walker89 inner))
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
          in (Optionals.cases (walker90 fun) (walker90 arg) (\_ -> walker90 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker90 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker90 body) Nothing (\inner -> walker90 inner))
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
          in (Optionals.cases (walker91 fun) (walker91 arg) (\_ -> walker91 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker91 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker91 body) Nothing (\inner -> walker91 inner))
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
          in (Optionals.cases (walker92 fun) (walker92 arg) (\_ -> walker92 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker92 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker92 body) Nothing (\inner -> walker92 inner))
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
          in (Optionals.cases (walker93 fun) (walker93 arg) (\_ -> walker93 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker93 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker93 body) Nothing (\inner -> walker93 inner))
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
          in (Optionals.cases (walker94 fun) (walker94 arg) (\_ -> walker94 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker94 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker94 body) Nothing (\inner -> walker94 inner))
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
          in (Optionals.cases (walker95 fun) (walker95 arg) (\_ -> walker95 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker95 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker95 body) Nothing (\inner -> walker95 inner))
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
          in (Optionals.cases (walker96 fun) (walker96 arg) (\_ -> walker96 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker96 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker96 body) Nothing (\inner -> walker96 inner))
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
          in (Optionals.cases (walker97 fun) (walker97 arg) (\_ -> walker97 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker97 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker97 body) Nothing (\inner -> walker97 inner))
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
          in (Optionals.cases (walker98 fun) (walker98 arg) (\_ -> walker98 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (walker98 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (walker98 body) Nothing (\inner -> walker98 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
