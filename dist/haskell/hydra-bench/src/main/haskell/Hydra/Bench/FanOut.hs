-- Note: this is an automatically generated file. Do not edit.
-- | Fan-out inference benchmark. Each fanWalker_K branches to three smaller fanWalkers via _Term cases — closer to real codegen DAG shape than LinearChain.

module Hydra.Bench.FanOut where
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
-- | Fan-out walker level 0; branches to fanWalker0, fanWalker0, fanWalker0.
fanWalker0 :: t0 -> Maybe t0
fanWalker0 t = Just t
-- | Fan-out walker level 1; branches to fanWalker0, fanWalker0, fanWalker0.
fanWalker1 :: Core.Term -> Maybe Core.Term
fanWalker1 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker0 fun) (fanWalker0 arg) (\_ -> fanWalker0 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker0 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker0 body) Nothing (\inner -> fanWalker0 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 10; branches to fanWalker9, fanWalker5, fanWalker7.
fanWalker10 :: Core.Term -> Maybe Core.Term
fanWalker10 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker9 fun) (fanWalker9 arg) (\_ -> fanWalker9 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker5 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker7 body) Nothing (\inner -> fanWalker7 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 100; branches to fanWalker99, fanWalker50, fanWalker67.
fanWalker100 :: Core.Term -> Maybe Core.Term
fanWalker100 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker99 fun) (fanWalker99 arg) (\_ -> fanWalker99 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker50 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker67 body) Nothing (\inner -> fanWalker67 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 101; branches to fanWalker100, fanWalker51, fanWalker68.
fanWalker101 :: Core.Term -> Maybe Core.Term
fanWalker101 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker100 fun) (fanWalker100 arg) (\_ -> fanWalker100 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker51 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker68 body) Nothing (\inner -> fanWalker68 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 102; branches to fanWalker101, fanWalker51, fanWalker68.
fanWalker102 :: Core.Term -> Maybe Core.Term
fanWalker102 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker101 fun) (fanWalker101 arg) (\_ -> fanWalker101 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker51 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker68 body) Nothing (\inner -> fanWalker68 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 103; branches to fanWalker102, fanWalker52, fanWalker69.
fanWalker103 :: Core.Term -> Maybe Core.Term
fanWalker103 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker102 fun) (fanWalker102 arg) (\_ -> fanWalker102 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker52 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker69 body) Nothing (\inner -> fanWalker69 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 104; branches to fanWalker103, fanWalker52, fanWalker70.
fanWalker104 :: Core.Term -> Maybe Core.Term
fanWalker104 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker103 fun) (fanWalker103 arg) (\_ -> fanWalker103 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker52 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker70 body) Nothing (\inner -> fanWalker70 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 105; branches to fanWalker104, fanWalker53, fanWalker70.
fanWalker105 :: Core.Term -> Maybe Core.Term
fanWalker105 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker104 fun) (fanWalker104 arg) (\_ -> fanWalker104 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker53 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker70 body) Nothing (\inner -> fanWalker70 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 106; branches to fanWalker105, fanWalker53, fanWalker71.
fanWalker106 :: Core.Term -> Maybe Core.Term
fanWalker106 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker105 fun) (fanWalker105 arg) (\_ -> fanWalker105 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker53 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker71 body) Nothing (\inner -> fanWalker71 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 107; branches to fanWalker106, fanWalker54, fanWalker72.
fanWalker107 :: Core.Term -> Maybe Core.Term
fanWalker107 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker106 fun) (fanWalker106 arg) (\_ -> fanWalker106 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker54 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker72 body) Nothing (\inner -> fanWalker72 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 108; branches to fanWalker107, fanWalker54, fanWalker72.
fanWalker108 :: Core.Term -> Maybe Core.Term
fanWalker108 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker107 fun) (fanWalker107 arg) (\_ -> fanWalker107 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker54 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker72 body) Nothing (\inner -> fanWalker72 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 109; branches to fanWalker108, fanWalker55, fanWalker73.
fanWalker109 :: Core.Term -> Maybe Core.Term
fanWalker109 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker108 fun) (fanWalker108 arg) (\_ -> fanWalker108 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker55 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker73 body) Nothing (\inner -> fanWalker73 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 11; branches to fanWalker10, fanWalker6, fanWalker8.
fanWalker11 :: Core.Term -> Maybe Core.Term
fanWalker11 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker10 fun) (fanWalker10 arg) (\_ -> fanWalker10 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker6 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker8 body) Nothing (\inner -> fanWalker8 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 110; branches to fanWalker109, fanWalker55, fanWalker74.
fanWalker110 :: Core.Term -> Maybe Core.Term
fanWalker110 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker109 fun) (fanWalker109 arg) (\_ -> fanWalker109 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker55 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker74 body) Nothing (\inner -> fanWalker74 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 111; branches to fanWalker110, fanWalker56, fanWalker74.
fanWalker111 :: Core.Term -> Maybe Core.Term
fanWalker111 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker110 fun) (fanWalker110 arg) (\_ -> fanWalker110 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker56 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker74 body) Nothing (\inner -> fanWalker74 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 112; branches to fanWalker111, fanWalker56, fanWalker75.
fanWalker112 :: Core.Term -> Maybe Core.Term
fanWalker112 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker111 fun) (fanWalker111 arg) (\_ -> fanWalker111 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker56 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker75 body) Nothing (\inner -> fanWalker75 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 113; branches to fanWalker112, fanWalker57, fanWalker76.
fanWalker113 :: Core.Term -> Maybe Core.Term
fanWalker113 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker112 fun) (fanWalker112 arg) (\_ -> fanWalker112 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker57 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker76 body) Nothing (\inner -> fanWalker76 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 114; branches to fanWalker113, fanWalker57, fanWalker76.
fanWalker114 :: Core.Term -> Maybe Core.Term
fanWalker114 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker113 fun) (fanWalker113 arg) (\_ -> fanWalker113 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker57 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker76 body) Nothing (\inner -> fanWalker76 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 115; branches to fanWalker114, fanWalker58, fanWalker77.
fanWalker115 :: Core.Term -> Maybe Core.Term
fanWalker115 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker114 fun) (fanWalker114 arg) (\_ -> fanWalker114 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker58 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker77 body) Nothing (\inner -> fanWalker77 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 116; branches to fanWalker115, fanWalker58, fanWalker78.
fanWalker116 :: Core.Term -> Maybe Core.Term
fanWalker116 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker115 fun) (fanWalker115 arg) (\_ -> fanWalker115 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker58 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker78 body) Nothing (\inner -> fanWalker78 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 117; branches to fanWalker116, fanWalker59, fanWalker78.
fanWalker117 :: Core.Term -> Maybe Core.Term
fanWalker117 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker116 fun) (fanWalker116 arg) (\_ -> fanWalker116 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker59 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker78 body) Nothing (\inner -> fanWalker78 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 118; branches to fanWalker117, fanWalker59, fanWalker79.
fanWalker118 :: Core.Term -> Maybe Core.Term
fanWalker118 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker117 fun) (fanWalker117 arg) (\_ -> fanWalker117 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker59 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker79 body) Nothing (\inner -> fanWalker79 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 119; branches to fanWalker118, fanWalker60, fanWalker80.
fanWalker119 :: Core.Term -> Maybe Core.Term
fanWalker119 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker118 fun) (fanWalker118 arg) (\_ -> fanWalker118 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker60 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker80 body) Nothing (\inner -> fanWalker80 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 12; branches to fanWalker11, fanWalker6, fanWalker8.
fanWalker12 :: Core.Term -> Maybe Core.Term
fanWalker12 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker11 fun) (fanWalker11 arg) (\_ -> fanWalker11 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker6 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker8 body) Nothing (\inner -> fanWalker8 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 120; branches to fanWalker119, fanWalker60, fanWalker80.
fanWalker120 :: Core.Term -> Maybe Core.Term
fanWalker120 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker119 fun) (fanWalker119 arg) (\_ -> fanWalker119 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker60 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker80 body) Nothing (\inner -> fanWalker80 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 121; branches to fanWalker120, fanWalker61, fanWalker81.
fanWalker121 :: Core.Term -> Maybe Core.Term
fanWalker121 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker120 fun) (fanWalker120 arg) (\_ -> fanWalker120 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker61 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker81 body) Nothing (\inner -> fanWalker81 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 122; branches to fanWalker121, fanWalker61, fanWalker82.
fanWalker122 :: Core.Term -> Maybe Core.Term
fanWalker122 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker121 fun) (fanWalker121 arg) (\_ -> fanWalker121 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker61 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker82 body) Nothing (\inner -> fanWalker82 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 123; branches to fanWalker122, fanWalker62, fanWalker82.
fanWalker123 :: Core.Term -> Maybe Core.Term
fanWalker123 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker122 fun) (fanWalker122 arg) (\_ -> fanWalker122 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker62 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker82 body) Nothing (\inner -> fanWalker82 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 124; branches to fanWalker123, fanWalker62, fanWalker83.
fanWalker124 :: Core.Term -> Maybe Core.Term
fanWalker124 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker123 fun) (fanWalker123 arg) (\_ -> fanWalker123 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker62 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker83 body) Nothing (\inner -> fanWalker83 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 125; branches to fanWalker124, fanWalker63, fanWalker84.
fanWalker125 :: Core.Term -> Maybe Core.Term
fanWalker125 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker124 fun) (fanWalker124 arg) (\_ -> fanWalker124 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker63 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker84 body) Nothing (\inner -> fanWalker84 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 126; branches to fanWalker125, fanWalker63, fanWalker84.
fanWalker126 :: Core.Term -> Maybe Core.Term
fanWalker126 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker125 fun) (fanWalker125 arg) (\_ -> fanWalker125 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker63 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker84 body) Nothing (\inner -> fanWalker84 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 127; branches to fanWalker126, fanWalker64, fanWalker85.
fanWalker127 :: Core.Term -> Maybe Core.Term
fanWalker127 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker126 fun) (fanWalker126 arg) (\_ -> fanWalker126 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker64 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker85 body) Nothing (\inner -> fanWalker85 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 128; branches to fanWalker127, fanWalker64, fanWalker86.
fanWalker128 :: Core.Term -> Maybe Core.Term
fanWalker128 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker127 fun) (fanWalker127 arg) (\_ -> fanWalker127 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker64 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker86 body) Nothing (\inner -> fanWalker86 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 129; branches to fanWalker128, fanWalker65, fanWalker86.
fanWalker129 :: Core.Term -> Maybe Core.Term
fanWalker129 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker128 fun) (fanWalker128 arg) (\_ -> fanWalker128 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker65 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker86 body) Nothing (\inner -> fanWalker86 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 13; branches to fanWalker12, fanWalker7, fanWalker9.
fanWalker13 :: Core.Term -> Maybe Core.Term
fanWalker13 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker12 fun) (fanWalker12 arg) (\_ -> fanWalker12 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker7 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker9 body) Nothing (\inner -> fanWalker9 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 130; branches to fanWalker129, fanWalker65, fanWalker87.
fanWalker130 :: Core.Term -> Maybe Core.Term
fanWalker130 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker129 fun) (fanWalker129 arg) (\_ -> fanWalker129 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker65 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker87 body) Nothing (\inner -> fanWalker87 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 131; branches to fanWalker130, fanWalker66, fanWalker88.
fanWalker131 :: Core.Term -> Maybe Core.Term
fanWalker131 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker130 fun) (fanWalker130 arg) (\_ -> fanWalker130 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker66 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker88 body) Nothing (\inner -> fanWalker88 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 132; branches to fanWalker131, fanWalker66, fanWalker88.
fanWalker132 :: Core.Term -> Maybe Core.Term
fanWalker132 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker131 fun) (fanWalker131 arg) (\_ -> fanWalker131 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker66 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker88 body) Nothing (\inner -> fanWalker88 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 133; branches to fanWalker132, fanWalker67, fanWalker89.
fanWalker133 :: Core.Term -> Maybe Core.Term
fanWalker133 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker132 fun) (fanWalker132 arg) (\_ -> fanWalker132 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker67 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker89 body) Nothing (\inner -> fanWalker89 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 134; branches to fanWalker133, fanWalker67, fanWalker90.
fanWalker134 :: Core.Term -> Maybe Core.Term
fanWalker134 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker133 fun) (fanWalker133 arg) (\_ -> fanWalker133 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker67 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker90 body) Nothing (\inner -> fanWalker90 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 135; branches to fanWalker134, fanWalker68, fanWalker90.
fanWalker135 :: Core.Term -> Maybe Core.Term
fanWalker135 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker134 fun) (fanWalker134 arg) (\_ -> fanWalker134 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker68 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker90 body) Nothing (\inner -> fanWalker90 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 136; branches to fanWalker135, fanWalker68, fanWalker91.
fanWalker136 :: Core.Term -> Maybe Core.Term
fanWalker136 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker135 fun) (fanWalker135 arg) (\_ -> fanWalker135 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker68 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker91 body) Nothing (\inner -> fanWalker91 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 137; branches to fanWalker136, fanWalker69, fanWalker92.
fanWalker137 :: Core.Term -> Maybe Core.Term
fanWalker137 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker136 fun) (fanWalker136 arg) (\_ -> fanWalker136 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker69 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker92 body) Nothing (\inner -> fanWalker92 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 138; branches to fanWalker137, fanWalker69, fanWalker92.
fanWalker138 :: Core.Term -> Maybe Core.Term
fanWalker138 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker137 fun) (fanWalker137 arg) (\_ -> fanWalker137 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker69 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker92 body) Nothing (\inner -> fanWalker92 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 139; branches to fanWalker138, fanWalker70, fanWalker93.
fanWalker139 :: Core.Term -> Maybe Core.Term
fanWalker139 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker138 fun) (fanWalker138 arg) (\_ -> fanWalker138 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker70 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker93 body) Nothing (\inner -> fanWalker93 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 14; branches to fanWalker13, fanWalker7, fanWalker10.
fanWalker14 :: Core.Term -> Maybe Core.Term
fanWalker14 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker13 fun) (fanWalker13 arg) (\_ -> fanWalker13 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker7 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker10 body) Nothing (\inner -> fanWalker10 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 140; branches to fanWalker139, fanWalker70, fanWalker94.
fanWalker140 :: Core.Term -> Maybe Core.Term
fanWalker140 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker139 fun) (fanWalker139 arg) (\_ -> fanWalker139 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker70 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker94 body) Nothing (\inner -> fanWalker94 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 141; branches to fanWalker140, fanWalker71, fanWalker94.
fanWalker141 :: Core.Term -> Maybe Core.Term
fanWalker141 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker140 fun) (fanWalker140 arg) (\_ -> fanWalker140 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker71 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker94 body) Nothing (\inner -> fanWalker94 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 142; branches to fanWalker141, fanWalker71, fanWalker95.
fanWalker142 :: Core.Term -> Maybe Core.Term
fanWalker142 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker141 fun) (fanWalker141 arg) (\_ -> fanWalker141 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker71 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker95 body) Nothing (\inner -> fanWalker95 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 143; branches to fanWalker142, fanWalker72, fanWalker96.
fanWalker143 :: Core.Term -> Maybe Core.Term
fanWalker143 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker142 fun) (fanWalker142 arg) (\_ -> fanWalker142 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker72 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker96 body) Nothing (\inner -> fanWalker96 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 144; branches to fanWalker143, fanWalker72, fanWalker96.
fanWalker144 :: Core.Term -> Maybe Core.Term
fanWalker144 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker143 fun) (fanWalker143 arg) (\_ -> fanWalker143 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker72 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker96 body) Nothing (\inner -> fanWalker96 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 145; branches to fanWalker144, fanWalker73, fanWalker97.
fanWalker145 :: Core.Term -> Maybe Core.Term
fanWalker145 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker144 fun) (fanWalker144 arg) (\_ -> fanWalker144 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker73 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker97 body) Nothing (\inner -> fanWalker97 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 146; branches to fanWalker145, fanWalker73, fanWalker98.
fanWalker146 :: Core.Term -> Maybe Core.Term
fanWalker146 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker145 fun) (fanWalker145 arg) (\_ -> fanWalker145 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker73 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker98 body) Nothing (\inner -> fanWalker98 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 147; branches to fanWalker146, fanWalker74, fanWalker98.
fanWalker147 :: Core.Term -> Maybe Core.Term
fanWalker147 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker146 fun) (fanWalker146 arg) (\_ -> fanWalker146 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker74 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker98 body) Nothing (\inner -> fanWalker98 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 148; branches to fanWalker147, fanWalker74, fanWalker99.
fanWalker148 :: Core.Term -> Maybe Core.Term
fanWalker148 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker147 fun) (fanWalker147 arg) (\_ -> fanWalker147 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker74 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker99 body) Nothing (\inner -> fanWalker99 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 149; branches to fanWalker148, fanWalker75, fanWalker100.
fanWalker149 :: Core.Term -> Maybe Core.Term
fanWalker149 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker148 fun) (fanWalker148 arg) (\_ -> fanWalker148 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker75 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker100 body) Nothing (\inner -> fanWalker100 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 15; branches to fanWalker14, fanWalker8, fanWalker10.
fanWalker15 :: Core.Term -> Maybe Core.Term
fanWalker15 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker14 fun) (fanWalker14 arg) (\_ -> fanWalker14 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker8 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker10 body) Nothing (\inner -> fanWalker10 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 150; branches to fanWalker149, fanWalker75, fanWalker100.
fanWalker150 :: Core.Term -> Maybe Core.Term
fanWalker150 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker149 fun) (fanWalker149 arg) (\_ -> fanWalker149 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker75 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker100 body) Nothing (\inner -> fanWalker100 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 151; branches to fanWalker150, fanWalker76, fanWalker101.
fanWalker151 :: Core.Term -> Maybe Core.Term
fanWalker151 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker150 fun) (fanWalker150 arg) (\_ -> fanWalker150 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker76 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker101 body) Nothing (\inner -> fanWalker101 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 152; branches to fanWalker151, fanWalker76, fanWalker102.
fanWalker152 :: Core.Term -> Maybe Core.Term
fanWalker152 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker151 fun) (fanWalker151 arg) (\_ -> fanWalker151 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker76 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker102 body) Nothing (\inner -> fanWalker102 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 153; branches to fanWalker152, fanWalker77, fanWalker102.
fanWalker153 :: Core.Term -> Maybe Core.Term
fanWalker153 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker152 fun) (fanWalker152 arg) (\_ -> fanWalker152 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker77 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker102 body) Nothing (\inner -> fanWalker102 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 154; branches to fanWalker153, fanWalker77, fanWalker103.
fanWalker154 :: Core.Term -> Maybe Core.Term
fanWalker154 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker153 fun) (fanWalker153 arg) (\_ -> fanWalker153 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker77 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker103 body) Nothing (\inner -> fanWalker103 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 155; branches to fanWalker154, fanWalker78, fanWalker104.
fanWalker155 :: Core.Term -> Maybe Core.Term
fanWalker155 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker154 fun) (fanWalker154 arg) (\_ -> fanWalker154 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker78 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker104 body) Nothing (\inner -> fanWalker104 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 156; branches to fanWalker155, fanWalker78, fanWalker104.
fanWalker156 :: Core.Term -> Maybe Core.Term
fanWalker156 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker155 fun) (fanWalker155 arg) (\_ -> fanWalker155 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker78 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker104 body) Nothing (\inner -> fanWalker104 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 157; branches to fanWalker156, fanWalker79, fanWalker105.
fanWalker157 :: Core.Term -> Maybe Core.Term
fanWalker157 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker156 fun) (fanWalker156 arg) (\_ -> fanWalker156 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker79 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker105 body) Nothing (\inner -> fanWalker105 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 158; branches to fanWalker157, fanWalker79, fanWalker106.
fanWalker158 :: Core.Term -> Maybe Core.Term
fanWalker158 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker157 fun) (fanWalker157 arg) (\_ -> fanWalker157 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker79 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker106 body) Nothing (\inner -> fanWalker106 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 159; branches to fanWalker158, fanWalker80, fanWalker106.
fanWalker159 :: Core.Term -> Maybe Core.Term
fanWalker159 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker158 fun) (fanWalker158 arg) (\_ -> fanWalker158 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker80 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker106 body) Nothing (\inner -> fanWalker106 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 16; branches to fanWalker15, fanWalker8, fanWalker11.
fanWalker16 :: Core.Term -> Maybe Core.Term
fanWalker16 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker15 fun) (fanWalker15 arg) (\_ -> fanWalker15 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker8 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker11 body) Nothing (\inner -> fanWalker11 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 160; branches to fanWalker159, fanWalker80, fanWalker107.
fanWalker160 :: Core.Term -> Maybe Core.Term
fanWalker160 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker159 fun) (fanWalker159 arg) (\_ -> fanWalker159 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker80 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker107 body) Nothing (\inner -> fanWalker107 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 161; branches to fanWalker160, fanWalker81, fanWalker108.
fanWalker161 :: Core.Term -> Maybe Core.Term
fanWalker161 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker160 fun) (fanWalker160 arg) (\_ -> fanWalker160 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker81 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker108 body) Nothing (\inner -> fanWalker108 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 162; branches to fanWalker161, fanWalker81, fanWalker108.
fanWalker162 :: Core.Term -> Maybe Core.Term
fanWalker162 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker161 fun) (fanWalker161 arg) (\_ -> fanWalker161 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker81 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker108 body) Nothing (\inner -> fanWalker108 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 163; branches to fanWalker162, fanWalker82, fanWalker109.
fanWalker163 :: Core.Term -> Maybe Core.Term
fanWalker163 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker162 fun) (fanWalker162 arg) (\_ -> fanWalker162 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker82 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker109 body) Nothing (\inner -> fanWalker109 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 164; branches to fanWalker163, fanWalker82, fanWalker110.
fanWalker164 :: Core.Term -> Maybe Core.Term
fanWalker164 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker163 fun) (fanWalker163 arg) (\_ -> fanWalker163 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker82 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker110 body) Nothing (\inner -> fanWalker110 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 165; branches to fanWalker164, fanWalker83, fanWalker110.
fanWalker165 :: Core.Term -> Maybe Core.Term
fanWalker165 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker164 fun) (fanWalker164 arg) (\_ -> fanWalker164 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker83 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker110 body) Nothing (\inner -> fanWalker110 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 166; branches to fanWalker165, fanWalker83, fanWalker111.
fanWalker166 :: Core.Term -> Maybe Core.Term
fanWalker166 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker165 fun) (fanWalker165 arg) (\_ -> fanWalker165 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker83 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker111 body) Nothing (\inner -> fanWalker111 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 167; branches to fanWalker166, fanWalker84, fanWalker112.
fanWalker167 :: Core.Term -> Maybe Core.Term
fanWalker167 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker166 fun) (fanWalker166 arg) (\_ -> fanWalker166 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker84 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker112 body) Nothing (\inner -> fanWalker112 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 168; branches to fanWalker167, fanWalker84, fanWalker112.
fanWalker168 :: Core.Term -> Maybe Core.Term
fanWalker168 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker167 fun) (fanWalker167 arg) (\_ -> fanWalker167 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker84 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker112 body) Nothing (\inner -> fanWalker112 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 169; branches to fanWalker168, fanWalker85, fanWalker113.
fanWalker169 :: Core.Term -> Maybe Core.Term
fanWalker169 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker168 fun) (fanWalker168 arg) (\_ -> fanWalker168 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker85 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker113 body) Nothing (\inner -> fanWalker113 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 17; branches to fanWalker16, fanWalker9, fanWalker12.
fanWalker17 :: Core.Term -> Maybe Core.Term
fanWalker17 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker16 fun) (fanWalker16 arg) (\_ -> fanWalker16 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker9 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker12 body) Nothing (\inner -> fanWalker12 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 170; branches to fanWalker169, fanWalker85, fanWalker114.
fanWalker170 :: Core.Term -> Maybe Core.Term
fanWalker170 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker169 fun) (fanWalker169 arg) (\_ -> fanWalker169 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker85 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker114 body) Nothing (\inner -> fanWalker114 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 171; branches to fanWalker170, fanWalker86, fanWalker114.
fanWalker171 :: Core.Term -> Maybe Core.Term
fanWalker171 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker170 fun) (fanWalker170 arg) (\_ -> fanWalker170 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker86 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker114 body) Nothing (\inner -> fanWalker114 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 172; branches to fanWalker171, fanWalker86, fanWalker115.
fanWalker172 :: Core.Term -> Maybe Core.Term
fanWalker172 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker171 fun) (fanWalker171 arg) (\_ -> fanWalker171 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker86 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker115 body) Nothing (\inner -> fanWalker115 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 173; branches to fanWalker172, fanWalker87, fanWalker116.
fanWalker173 :: Core.Term -> Maybe Core.Term
fanWalker173 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker172 fun) (fanWalker172 arg) (\_ -> fanWalker172 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker87 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker116 body) Nothing (\inner -> fanWalker116 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 174; branches to fanWalker173, fanWalker87, fanWalker116.
fanWalker174 :: Core.Term -> Maybe Core.Term
fanWalker174 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker173 fun) (fanWalker173 arg) (\_ -> fanWalker173 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker87 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker116 body) Nothing (\inner -> fanWalker116 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 175; branches to fanWalker174, fanWalker88, fanWalker117.
fanWalker175 :: Core.Term -> Maybe Core.Term
fanWalker175 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker174 fun) (fanWalker174 arg) (\_ -> fanWalker174 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker88 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker117 body) Nothing (\inner -> fanWalker117 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 176; branches to fanWalker175, fanWalker88, fanWalker118.
fanWalker176 :: Core.Term -> Maybe Core.Term
fanWalker176 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker175 fun) (fanWalker175 arg) (\_ -> fanWalker175 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker88 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker118 body) Nothing (\inner -> fanWalker118 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 177; branches to fanWalker176, fanWalker89, fanWalker118.
fanWalker177 :: Core.Term -> Maybe Core.Term
fanWalker177 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker176 fun) (fanWalker176 arg) (\_ -> fanWalker176 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker89 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker118 body) Nothing (\inner -> fanWalker118 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 178; branches to fanWalker177, fanWalker89, fanWalker119.
fanWalker178 :: Core.Term -> Maybe Core.Term
fanWalker178 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker177 fun) (fanWalker177 arg) (\_ -> fanWalker177 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker89 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker119 body) Nothing (\inner -> fanWalker119 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 179; branches to fanWalker178, fanWalker90, fanWalker120.
fanWalker179 :: Core.Term -> Maybe Core.Term
fanWalker179 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker178 fun) (fanWalker178 arg) (\_ -> fanWalker178 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker90 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker120 body) Nothing (\inner -> fanWalker120 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 18; branches to fanWalker17, fanWalker9, fanWalker12.
fanWalker18 :: Core.Term -> Maybe Core.Term
fanWalker18 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker17 fun) (fanWalker17 arg) (\_ -> fanWalker17 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker9 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker12 body) Nothing (\inner -> fanWalker12 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 180; branches to fanWalker179, fanWalker90, fanWalker120.
fanWalker180 :: Core.Term -> Maybe Core.Term
fanWalker180 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker179 fun) (fanWalker179 arg) (\_ -> fanWalker179 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker90 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker120 body) Nothing (\inner -> fanWalker120 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 181; branches to fanWalker180, fanWalker91, fanWalker121.
fanWalker181 :: Core.Term -> Maybe Core.Term
fanWalker181 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker180 fun) (fanWalker180 arg) (\_ -> fanWalker180 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker91 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker121 body) Nothing (\inner -> fanWalker121 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 182; branches to fanWalker181, fanWalker91, fanWalker122.
fanWalker182 :: Core.Term -> Maybe Core.Term
fanWalker182 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker181 fun) (fanWalker181 arg) (\_ -> fanWalker181 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker91 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker122 body) Nothing (\inner -> fanWalker122 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 183; branches to fanWalker182, fanWalker92, fanWalker122.
fanWalker183 :: Core.Term -> Maybe Core.Term
fanWalker183 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker182 fun) (fanWalker182 arg) (\_ -> fanWalker182 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker92 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker122 body) Nothing (\inner -> fanWalker122 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 184; branches to fanWalker183, fanWalker92, fanWalker123.
fanWalker184 :: Core.Term -> Maybe Core.Term
fanWalker184 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker183 fun) (fanWalker183 arg) (\_ -> fanWalker183 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker92 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker123 body) Nothing (\inner -> fanWalker123 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 185; branches to fanWalker184, fanWalker93, fanWalker124.
fanWalker185 :: Core.Term -> Maybe Core.Term
fanWalker185 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker184 fun) (fanWalker184 arg) (\_ -> fanWalker184 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker93 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker124 body) Nothing (\inner -> fanWalker124 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 186; branches to fanWalker185, fanWalker93, fanWalker124.
fanWalker186 :: Core.Term -> Maybe Core.Term
fanWalker186 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker185 fun) (fanWalker185 arg) (\_ -> fanWalker185 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker93 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker124 body) Nothing (\inner -> fanWalker124 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 187; branches to fanWalker186, fanWalker94, fanWalker125.
fanWalker187 :: Core.Term -> Maybe Core.Term
fanWalker187 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker186 fun) (fanWalker186 arg) (\_ -> fanWalker186 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker94 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker125 body) Nothing (\inner -> fanWalker125 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 188; branches to fanWalker187, fanWalker94, fanWalker126.
fanWalker188 :: Core.Term -> Maybe Core.Term
fanWalker188 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker187 fun) (fanWalker187 arg) (\_ -> fanWalker187 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker94 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker126 body) Nothing (\inner -> fanWalker126 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 189; branches to fanWalker188, fanWalker95, fanWalker126.
fanWalker189 :: Core.Term -> Maybe Core.Term
fanWalker189 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker188 fun) (fanWalker188 arg) (\_ -> fanWalker188 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker95 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker126 body) Nothing (\inner -> fanWalker126 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 19; branches to fanWalker18, fanWalker10, fanWalker13.
fanWalker19 :: Core.Term -> Maybe Core.Term
fanWalker19 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker18 fun) (fanWalker18 arg) (\_ -> fanWalker18 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker10 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker13 body) Nothing (\inner -> fanWalker13 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 190; branches to fanWalker189, fanWalker95, fanWalker127.
fanWalker190 :: Core.Term -> Maybe Core.Term
fanWalker190 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker189 fun) (fanWalker189 arg) (\_ -> fanWalker189 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker95 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker127 body) Nothing (\inner -> fanWalker127 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 191; branches to fanWalker190, fanWalker96, fanWalker128.
fanWalker191 :: Core.Term -> Maybe Core.Term
fanWalker191 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker190 fun) (fanWalker190 arg) (\_ -> fanWalker190 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker96 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker128 body) Nothing (\inner -> fanWalker128 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 192; branches to fanWalker191, fanWalker96, fanWalker128.
fanWalker192 :: Core.Term -> Maybe Core.Term
fanWalker192 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker191 fun) (fanWalker191 arg) (\_ -> fanWalker191 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker96 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker128 body) Nothing (\inner -> fanWalker128 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 193; branches to fanWalker192, fanWalker97, fanWalker129.
fanWalker193 :: Core.Term -> Maybe Core.Term
fanWalker193 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker192 fun) (fanWalker192 arg) (\_ -> fanWalker192 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker97 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker129 body) Nothing (\inner -> fanWalker129 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 194; branches to fanWalker193, fanWalker97, fanWalker130.
fanWalker194 :: Core.Term -> Maybe Core.Term
fanWalker194 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker193 fun) (fanWalker193 arg) (\_ -> fanWalker193 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker97 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker130 body) Nothing (\inner -> fanWalker130 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 195; branches to fanWalker194, fanWalker98, fanWalker130.
fanWalker195 :: Core.Term -> Maybe Core.Term
fanWalker195 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker194 fun) (fanWalker194 arg) (\_ -> fanWalker194 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker98 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker130 body) Nothing (\inner -> fanWalker130 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 196; branches to fanWalker195, fanWalker98, fanWalker131.
fanWalker196 :: Core.Term -> Maybe Core.Term
fanWalker196 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker195 fun) (fanWalker195 arg) (\_ -> fanWalker195 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker98 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker131 body) Nothing (\inner -> fanWalker131 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 197; branches to fanWalker196, fanWalker99, fanWalker132.
fanWalker197 :: Core.Term -> Maybe Core.Term
fanWalker197 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker196 fun) (fanWalker196 arg) (\_ -> fanWalker196 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker99 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker132 body) Nothing (\inner -> fanWalker132 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 198; branches to fanWalker197, fanWalker99, fanWalker132.
fanWalker198 :: Core.Term -> Maybe Core.Term
fanWalker198 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker197 fun) (fanWalker197 arg) (\_ -> fanWalker197 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker99 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker132 body) Nothing (\inner -> fanWalker132 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 199; branches to fanWalker198, fanWalker100, fanWalker133.
fanWalker199 :: Core.Term -> Maybe Core.Term
fanWalker199 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker198 fun) (fanWalker198 arg) (\_ -> fanWalker198 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker100 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker133 body) Nothing (\inner -> fanWalker133 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 2; branches to fanWalker1, fanWalker1, fanWalker1.
fanWalker2 :: Core.Term -> Maybe Core.Term
fanWalker2 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker1 fun) (fanWalker1 arg) (\_ -> fanWalker1 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker1 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker1 body) Nothing (\inner -> fanWalker1 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 20; branches to fanWalker19, fanWalker10, fanWalker14.
fanWalker20 :: Core.Term -> Maybe Core.Term
fanWalker20 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker19 fun) (fanWalker19 arg) (\_ -> fanWalker19 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker10 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker14 body) Nothing (\inner -> fanWalker14 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 200; branches to fanWalker199, fanWalker100, fanWalker134.
fanWalker200 :: Core.Term -> Maybe Core.Term
fanWalker200 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker199 fun) (fanWalker199 arg) (\_ -> fanWalker199 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker100 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker134 body) Nothing (\inner -> fanWalker134 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 201; branches to fanWalker200, fanWalker101, fanWalker134.
fanWalker201 :: Core.Term -> Maybe Core.Term
fanWalker201 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker200 fun) (fanWalker200 arg) (\_ -> fanWalker200 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker101 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker134 body) Nothing (\inner -> fanWalker134 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 202; branches to fanWalker201, fanWalker101, fanWalker135.
fanWalker202 :: Core.Term -> Maybe Core.Term
fanWalker202 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker201 fun) (fanWalker201 arg) (\_ -> fanWalker201 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker101 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker135 body) Nothing (\inner -> fanWalker135 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 203; branches to fanWalker202, fanWalker102, fanWalker136.
fanWalker203 :: Core.Term -> Maybe Core.Term
fanWalker203 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker202 fun) (fanWalker202 arg) (\_ -> fanWalker202 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker102 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker136 body) Nothing (\inner -> fanWalker136 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 204; branches to fanWalker203, fanWalker102, fanWalker136.
fanWalker204 :: Core.Term -> Maybe Core.Term
fanWalker204 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker203 fun) (fanWalker203 arg) (\_ -> fanWalker203 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker102 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker136 body) Nothing (\inner -> fanWalker136 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 205; branches to fanWalker204, fanWalker103, fanWalker137.
fanWalker205 :: Core.Term -> Maybe Core.Term
fanWalker205 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker204 fun) (fanWalker204 arg) (\_ -> fanWalker204 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker103 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker137 body) Nothing (\inner -> fanWalker137 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 206; branches to fanWalker205, fanWalker103, fanWalker138.
fanWalker206 :: Core.Term -> Maybe Core.Term
fanWalker206 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker205 fun) (fanWalker205 arg) (\_ -> fanWalker205 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker103 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker138 body) Nothing (\inner -> fanWalker138 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 207; branches to fanWalker206, fanWalker104, fanWalker138.
fanWalker207 :: Core.Term -> Maybe Core.Term
fanWalker207 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker206 fun) (fanWalker206 arg) (\_ -> fanWalker206 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker104 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker138 body) Nothing (\inner -> fanWalker138 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 208; branches to fanWalker207, fanWalker104, fanWalker139.
fanWalker208 :: Core.Term -> Maybe Core.Term
fanWalker208 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker207 fun) (fanWalker207 arg) (\_ -> fanWalker207 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker104 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker139 body) Nothing (\inner -> fanWalker139 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 209; branches to fanWalker208, fanWalker105, fanWalker140.
fanWalker209 :: Core.Term -> Maybe Core.Term
fanWalker209 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker208 fun) (fanWalker208 arg) (\_ -> fanWalker208 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker105 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker140 body) Nothing (\inner -> fanWalker140 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 21; branches to fanWalker20, fanWalker11, fanWalker14.
fanWalker21 :: Core.Term -> Maybe Core.Term
fanWalker21 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker20 fun) (fanWalker20 arg) (\_ -> fanWalker20 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker11 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker14 body) Nothing (\inner -> fanWalker14 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 210; branches to fanWalker209, fanWalker105, fanWalker140.
fanWalker210 :: Core.Term -> Maybe Core.Term
fanWalker210 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker209 fun) (fanWalker209 arg) (\_ -> fanWalker209 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker105 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker140 body) Nothing (\inner -> fanWalker140 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 211; branches to fanWalker210, fanWalker106, fanWalker141.
fanWalker211 :: Core.Term -> Maybe Core.Term
fanWalker211 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker210 fun) (fanWalker210 arg) (\_ -> fanWalker210 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker106 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker141 body) Nothing (\inner -> fanWalker141 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 212; branches to fanWalker211, fanWalker106, fanWalker142.
fanWalker212 :: Core.Term -> Maybe Core.Term
fanWalker212 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker211 fun) (fanWalker211 arg) (\_ -> fanWalker211 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker106 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker142 body) Nothing (\inner -> fanWalker142 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 213; branches to fanWalker212, fanWalker107, fanWalker142.
fanWalker213 :: Core.Term -> Maybe Core.Term
fanWalker213 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker212 fun) (fanWalker212 arg) (\_ -> fanWalker212 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker107 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker142 body) Nothing (\inner -> fanWalker142 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 214; branches to fanWalker213, fanWalker107, fanWalker143.
fanWalker214 :: Core.Term -> Maybe Core.Term
fanWalker214 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker213 fun) (fanWalker213 arg) (\_ -> fanWalker213 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker107 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker143 body) Nothing (\inner -> fanWalker143 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 215; branches to fanWalker214, fanWalker108, fanWalker144.
fanWalker215 :: Core.Term -> Maybe Core.Term
fanWalker215 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker214 fun) (fanWalker214 arg) (\_ -> fanWalker214 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker108 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker144 body) Nothing (\inner -> fanWalker144 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 216; branches to fanWalker215, fanWalker108, fanWalker144.
fanWalker216 :: Core.Term -> Maybe Core.Term
fanWalker216 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker215 fun) (fanWalker215 arg) (\_ -> fanWalker215 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker108 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker144 body) Nothing (\inner -> fanWalker144 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 217; branches to fanWalker216, fanWalker109, fanWalker145.
fanWalker217 :: Core.Term -> Maybe Core.Term
fanWalker217 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker216 fun) (fanWalker216 arg) (\_ -> fanWalker216 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker109 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker145 body) Nothing (\inner -> fanWalker145 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 218; branches to fanWalker217, fanWalker109, fanWalker146.
fanWalker218 :: Core.Term -> Maybe Core.Term
fanWalker218 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker217 fun) (fanWalker217 arg) (\_ -> fanWalker217 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker109 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker146 body) Nothing (\inner -> fanWalker146 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 219; branches to fanWalker218, fanWalker110, fanWalker146.
fanWalker219 :: Core.Term -> Maybe Core.Term
fanWalker219 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker218 fun) (fanWalker218 arg) (\_ -> fanWalker218 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker110 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker146 body) Nothing (\inner -> fanWalker146 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 22; branches to fanWalker21, fanWalker11, fanWalker15.
fanWalker22 :: Core.Term -> Maybe Core.Term
fanWalker22 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker21 fun) (fanWalker21 arg) (\_ -> fanWalker21 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker11 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker15 body) Nothing (\inner -> fanWalker15 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 220; branches to fanWalker219, fanWalker110, fanWalker147.
fanWalker220 :: Core.Term -> Maybe Core.Term
fanWalker220 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker219 fun) (fanWalker219 arg) (\_ -> fanWalker219 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker110 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker147 body) Nothing (\inner -> fanWalker147 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 221; branches to fanWalker220, fanWalker111, fanWalker148.
fanWalker221 :: Core.Term -> Maybe Core.Term
fanWalker221 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker220 fun) (fanWalker220 arg) (\_ -> fanWalker220 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker111 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker148 body) Nothing (\inner -> fanWalker148 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 222; branches to fanWalker221, fanWalker111, fanWalker148.
fanWalker222 :: Core.Term -> Maybe Core.Term
fanWalker222 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker221 fun) (fanWalker221 arg) (\_ -> fanWalker221 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker111 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker148 body) Nothing (\inner -> fanWalker148 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 223; branches to fanWalker222, fanWalker112, fanWalker149.
fanWalker223 :: Core.Term -> Maybe Core.Term
fanWalker223 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker222 fun) (fanWalker222 arg) (\_ -> fanWalker222 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker112 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker149 body) Nothing (\inner -> fanWalker149 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 224; branches to fanWalker223, fanWalker112, fanWalker150.
fanWalker224 :: Core.Term -> Maybe Core.Term
fanWalker224 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker223 fun) (fanWalker223 arg) (\_ -> fanWalker223 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker112 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker150 body) Nothing (\inner -> fanWalker150 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 225; branches to fanWalker224, fanWalker113, fanWalker150.
fanWalker225 :: Core.Term -> Maybe Core.Term
fanWalker225 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker224 fun) (fanWalker224 arg) (\_ -> fanWalker224 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker113 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker150 body) Nothing (\inner -> fanWalker150 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 226; branches to fanWalker225, fanWalker113, fanWalker151.
fanWalker226 :: Core.Term -> Maybe Core.Term
fanWalker226 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker225 fun) (fanWalker225 arg) (\_ -> fanWalker225 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker113 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker151 body) Nothing (\inner -> fanWalker151 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 227; branches to fanWalker226, fanWalker114, fanWalker152.
fanWalker227 :: Core.Term -> Maybe Core.Term
fanWalker227 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker226 fun) (fanWalker226 arg) (\_ -> fanWalker226 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker114 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker152 body) Nothing (\inner -> fanWalker152 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 228; branches to fanWalker227, fanWalker114, fanWalker152.
fanWalker228 :: Core.Term -> Maybe Core.Term
fanWalker228 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker227 fun) (fanWalker227 arg) (\_ -> fanWalker227 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker114 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker152 body) Nothing (\inner -> fanWalker152 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 229; branches to fanWalker228, fanWalker115, fanWalker153.
fanWalker229 :: Core.Term -> Maybe Core.Term
fanWalker229 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker228 fun) (fanWalker228 arg) (\_ -> fanWalker228 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker115 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker153 body) Nothing (\inner -> fanWalker153 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 23; branches to fanWalker22, fanWalker12, fanWalker16.
fanWalker23 :: Core.Term -> Maybe Core.Term
fanWalker23 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker22 fun) (fanWalker22 arg) (\_ -> fanWalker22 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker12 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker16 body) Nothing (\inner -> fanWalker16 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 230; branches to fanWalker229, fanWalker115, fanWalker154.
fanWalker230 :: Core.Term -> Maybe Core.Term
fanWalker230 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker229 fun) (fanWalker229 arg) (\_ -> fanWalker229 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker115 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker154 body) Nothing (\inner -> fanWalker154 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 231; branches to fanWalker230, fanWalker116, fanWalker154.
fanWalker231 :: Core.Term -> Maybe Core.Term
fanWalker231 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker230 fun) (fanWalker230 arg) (\_ -> fanWalker230 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker116 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker154 body) Nothing (\inner -> fanWalker154 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 232; branches to fanWalker231, fanWalker116, fanWalker155.
fanWalker232 :: Core.Term -> Maybe Core.Term
fanWalker232 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker231 fun) (fanWalker231 arg) (\_ -> fanWalker231 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker116 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker155 body) Nothing (\inner -> fanWalker155 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 233; branches to fanWalker232, fanWalker117, fanWalker156.
fanWalker233 :: Core.Term -> Maybe Core.Term
fanWalker233 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker232 fun) (fanWalker232 arg) (\_ -> fanWalker232 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker117 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker156 body) Nothing (\inner -> fanWalker156 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 234; branches to fanWalker233, fanWalker117, fanWalker156.
fanWalker234 :: Core.Term -> Maybe Core.Term
fanWalker234 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker233 fun) (fanWalker233 arg) (\_ -> fanWalker233 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker117 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker156 body) Nothing (\inner -> fanWalker156 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 235; branches to fanWalker234, fanWalker118, fanWalker157.
fanWalker235 :: Core.Term -> Maybe Core.Term
fanWalker235 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker234 fun) (fanWalker234 arg) (\_ -> fanWalker234 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker118 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker157 body) Nothing (\inner -> fanWalker157 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 236; branches to fanWalker235, fanWalker118, fanWalker158.
fanWalker236 :: Core.Term -> Maybe Core.Term
fanWalker236 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker235 fun) (fanWalker235 arg) (\_ -> fanWalker235 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker118 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker158 body) Nothing (\inner -> fanWalker158 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 237; branches to fanWalker236, fanWalker119, fanWalker158.
fanWalker237 :: Core.Term -> Maybe Core.Term
fanWalker237 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker236 fun) (fanWalker236 arg) (\_ -> fanWalker236 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker119 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker158 body) Nothing (\inner -> fanWalker158 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 238; branches to fanWalker237, fanWalker119, fanWalker159.
fanWalker238 :: Core.Term -> Maybe Core.Term
fanWalker238 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker237 fun) (fanWalker237 arg) (\_ -> fanWalker237 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker119 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker159 body) Nothing (\inner -> fanWalker159 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 239; branches to fanWalker238, fanWalker120, fanWalker160.
fanWalker239 :: Core.Term -> Maybe Core.Term
fanWalker239 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker238 fun) (fanWalker238 arg) (\_ -> fanWalker238 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker120 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker160 body) Nothing (\inner -> fanWalker160 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 24; branches to fanWalker23, fanWalker12, fanWalker16.
fanWalker24 :: Core.Term -> Maybe Core.Term
fanWalker24 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker23 fun) (fanWalker23 arg) (\_ -> fanWalker23 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker12 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker16 body) Nothing (\inner -> fanWalker16 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 240; branches to fanWalker239, fanWalker120, fanWalker160.
fanWalker240 :: Core.Term -> Maybe Core.Term
fanWalker240 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker239 fun) (fanWalker239 arg) (\_ -> fanWalker239 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker120 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker160 body) Nothing (\inner -> fanWalker160 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 241; branches to fanWalker240, fanWalker121, fanWalker161.
fanWalker241 :: Core.Term -> Maybe Core.Term
fanWalker241 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker240 fun) (fanWalker240 arg) (\_ -> fanWalker240 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker121 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker161 body) Nothing (\inner -> fanWalker161 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 242; branches to fanWalker241, fanWalker121, fanWalker162.
fanWalker242 :: Core.Term -> Maybe Core.Term
fanWalker242 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker241 fun) (fanWalker241 arg) (\_ -> fanWalker241 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker121 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker162 body) Nothing (\inner -> fanWalker162 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 243; branches to fanWalker242, fanWalker122, fanWalker162.
fanWalker243 :: Core.Term -> Maybe Core.Term
fanWalker243 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker242 fun) (fanWalker242 arg) (\_ -> fanWalker242 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker122 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker162 body) Nothing (\inner -> fanWalker162 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 244; branches to fanWalker243, fanWalker122, fanWalker163.
fanWalker244 :: Core.Term -> Maybe Core.Term
fanWalker244 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker243 fun) (fanWalker243 arg) (\_ -> fanWalker243 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker122 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker163 body) Nothing (\inner -> fanWalker163 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 245; branches to fanWalker244, fanWalker123, fanWalker164.
fanWalker245 :: Core.Term -> Maybe Core.Term
fanWalker245 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker244 fun) (fanWalker244 arg) (\_ -> fanWalker244 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker123 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker164 body) Nothing (\inner -> fanWalker164 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 246; branches to fanWalker245, fanWalker123, fanWalker164.
fanWalker246 :: Core.Term -> Maybe Core.Term
fanWalker246 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker245 fun) (fanWalker245 arg) (\_ -> fanWalker245 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker123 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker164 body) Nothing (\inner -> fanWalker164 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 247; branches to fanWalker246, fanWalker124, fanWalker165.
fanWalker247 :: Core.Term -> Maybe Core.Term
fanWalker247 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker246 fun) (fanWalker246 arg) (\_ -> fanWalker246 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker124 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker165 body) Nothing (\inner -> fanWalker165 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 248; branches to fanWalker247, fanWalker124, fanWalker166.
fanWalker248 :: Core.Term -> Maybe Core.Term
fanWalker248 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker247 fun) (fanWalker247 arg) (\_ -> fanWalker247 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker124 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker166 body) Nothing (\inner -> fanWalker166 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 249; branches to fanWalker248, fanWalker125, fanWalker166.
fanWalker249 :: Core.Term -> Maybe Core.Term
fanWalker249 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker248 fun) (fanWalker248 arg) (\_ -> fanWalker248 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker125 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker166 body) Nothing (\inner -> fanWalker166 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 25; branches to fanWalker24, fanWalker13, fanWalker17.
fanWalker25 :: Core.Term -> Maybe Core.Term
fanWalker25 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker24 fun) (fanWalker24 arg) (\_ -> fanWalker24 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker13 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker17 body) Nothing (\inner -> fanWalker17 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 250; branches to fanWalker249, fanWalker125, fanWalker167.
fanWalker250 :: Core.Term -> Maybe Core.Term
fanWalker250 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker249 fun) (fanWalker249 arg) (\_ -> fanWalker249 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker125 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker167 body) Nothing (\inner -> fanWalker167 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 251; branches to fanWalker250, fanWalker126, fanWalker168.
fanWalker251 :: Core.Term -> Maybe Core.Term
fanWalker251 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker250 fun) (fanWalker250 arg) (\_ -> fanWalker250 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker126 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker168 body) Nothing (\inner -> fanWalker168 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 252; branches to fanWalker251, fanWalker126, fanWalker168.
fanWalker252 :: Core.Term -> Maybe Core.Term
fanWalker252 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker251 fun) (fanWalker251 arg) (\_ -> fanWalker251 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker126 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker168 body) Nothing (\inner -> fanWalker168 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 253; branches to fanWalker252, fanWalker127, fanWalker169.
fanWalker253 :: Core.Term -> Maybe Core.Term
fanWalker253 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker252 fun) (fanWalker252 arg) (\_ -> fanWalker252 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker127 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker169 body) Nothing (\inner -> fanWalker169 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 254; branches to fanWalker253, fanWalker127, fanWalker170.
fanWalker254 :: Core.Term -> Maybe Core.Term
fanWalker254 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker253 fun) (fanWalker253 arg) (\_ -> fanWalker253 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker127 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker170 body) Nothing (\inner -> fanWalker170 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 255; branches to fanWalker254, fanWalker128, fanWalker170.
fanWalker255 :: Core.Term -> Maybe Core.Term
fanWalker255 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker254 fun) (fanWalker254 arg) (\_ -> fanWalker254 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker128 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker170 body) Nothing (\inner -> fanWalker170 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 256; branches to fanWalker255, fanWalker128, fanWalker171.
fanWalker256 :: Core.Term -> Maybe Core.Term
fanWalker256 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker255 fun) (fanWalker255 arg) (\_ -> fanWalker255 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker128 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker171 body) Nothing (\inner -> fanWalker171 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 257; branches to fanWalker256, fanWalker129, fanWalker172.
fanWalker257 :: Core.Term -> Maybe Core.Term
fanWalker257 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker256 fun) (fanWalker256 arg) (\_ -> fanWalker256 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker129 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker172 body) Nothing (\inner -> fanWalker172 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 258; branches to fanWalker257, fanWalker129, fanWalker172.
fanWalker258 :: Core.Term -> Maybe Core.Term
fanWalker258 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker257 fun) (fanWalker257 arg) (\_ -> fanWalker257 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker129 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker172 body) Nothing (\inner -> fanWalker172 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 259; branches to fanWalker258, fanWalker130, fanWalker173.
fanWalker259 :: Core.Term -> Maybe Core.Term
fanWalker259 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker258 fun) (fanWalker258 arg) (\_ -> fanWalker258 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker130 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker173 body) Nothing (\inner -> fanWalker173 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 26; branches to fanWalker25, fanWalker13, fanWalker18.
fanWalker26 :: Core.Term -> Maybe Core.Term
fanWalker26 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker25 fun) (fanWalker25 arg) (\_ -> fanWalker25 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker13 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker18 body) Nothing (\inner -> fanWalker18 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 260; branches to fanWalker259, fanWalker130, fanWalker174.
fanWalker260 :: Core.Term -> Maybe Core.Term
fanWalker260 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker259 fun) (fanWalker259 arg) (\_ -> fanWalker259 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker130 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker174 body) Nothing (\inner -> fanWalker174 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 261; branches to fanWalker260, fanWalker131, fanWalker174.
fanWalker261 :: Core.Term -> Maybe Core.Term
fanWalker261 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker260 fun) (fanWalker260 arg) (\_ -> fanWalker260 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker131 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker174 body) Nothing (\inner -> fanWalker174 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 262; branches to fanWalker261, fanWalker131, fanWalker175.
fanWalker262 :: Core.Term -> Maybe Core.Term
fanWalker262 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker261 fun) (fanWalker261 arg) (\_ -> fanWalker261 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker131 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker175 body) Nothing (\inner -> fanWalker175 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 263; branches to fanWalker262, fanWalker132, fanWalker176.
fanWalker263 :: Core.Term -> Maybe Core.Term
fanWalker263 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker262 fun) (fanWalker262 arg) (\_ -> fanWalker262 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker132 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker176 body) Nothing (\inner -> fanWalker176 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 264; branches to fanWalker263, fanWalker132, fanWalker176.
fanWalker264 :: Core.Term -> Maybe Core.Term
fanWalker264 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker263 fun) (fanWalker263 arg) (\_ -> fanWalker263 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker132 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker176 body) Nothing (\inner -> fanWalker176 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 265; branches to fanWalker264, fanWalker133, fanWalker177.
fanWalker265 :: Core.Term -> Maybe Core.Term
fanWalker265 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker264 fun) (fanWalker264 arg) (\_ -> fanWalker264 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker133 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker177 body) Nothing (\inner -> fanWalker177 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 266; branches to fanWalker265, fanWalker133, fanWalker178.
fanWalker266 :: Core.Term -> Maybe Core.Term
fanWalker266 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker265 fun) (fanWalker265 arg) (\_ -> fanWalker265 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker133 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker178 body) Nothing (\inner -> fanWalker178 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 267; branches to fanWalker266, fanWalker134, fanWalker178.
fanWalker267 :: Core.Term -> Maybe Core.Term
fanWalker267 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker266 fun) (fanWalker266 arg) (\_ -> fanWalker266 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker134 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker178 body) Nothing (\inner -> fanWalker178 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 268; branches to fanWalker267, fanWalker134, fanWalker179.
fanWalker268 :: Core.Term -> Maybe Core.Term
fanWalker268 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker267 fun) (fanWalker267 arg) (\_ -> fanWalker267 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker134 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker179 body) Nothing (\inner -> fanWalker179 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 269; branches to fanWalker268, fanWalker135, fanWalker180.
fanWalker269 :: Core.Term -> Maybe Core.Term
fanWalker269 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker268 fun) (fanWalker268 arg) (\_ -> fanWalker268 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker135 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker180 body) Nothing (\inner -> fanWalker180 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 27; branches to fanWalker26, fanWalker14, fanWalker18.
fanWalker27 :: Core.Term -> Maybe Core.Term
fanWalker27 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker26 fun) (fanWalker26 arg) (\_ -> fanWalker26 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker14 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker18 body) Nothing (\inner -> fanWalker18 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 270; branches to fanWalker269, fanWalker135, fanWalker180.
fanWalker270 :: Core.Term -> Maybe Core.Term
fanWalker270 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker269 fun) (fanWalker269 arg) (\_ -> fanWalker269 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker135 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker180 body) Nothing (\inner -> fanWalker180 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 271; branches to fanWalker270, fanWalker136, fanWalker181.
fanWalker271 :: Core.Term -> Maybe Core.Term
fanWalker271 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker270 fun) (fanWalker270 arg) (\_ -> fanWalker270 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker136 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker181 body) Nothing (\inner -> fanWalker181 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 272; branches to fanWalker271, fanWalker136, fanWalker182.
fanWalker272 :: Core.Term -> Maybe Core.Term
fanWalker272 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker271 fun) (fanWalker271 arg) (\_ -> fanWalker271 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker136 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker182 body) Nothing (\inner -> fanWalker182 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 273; branches to fanWalker272, fanWalker137, fanWalker182.
fanWalker273 :: Core.Term -> Maybe Core.Term
fanWalker273 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker272 fun) (fanWalker272 arg) (\_ -> fanWalker272 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker137 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker182 body) Nothing (\inner -> fanWalker182 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 274; branches to fanWalker273, fanWalker137, fanWalker183.
fanWalker274 :: Core.Term -> Maybe Core.Term
fanWalker274 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker273 fun) (fanWalker273 arg) (\_ -> fanWalker273 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker137 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker183 body) Nothing (\inner -> fanWalker183 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 275; branches to fanWalker274, fanWalker138, fanWalker184.
fanWalker275 :: Core.Term -> Maybe Core.Term
fanWalker275 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker274 fun) (fanWalker274 arg) (\_ -> fanWalker274 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker138 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker184 body) Nothing (\inner -> fanWalker184 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 276; branches to fanWalker275, fanWalker138, fanWalker184.
fanWalker276 :: Core.Term -> Maybe Core.Term
fanWalker276 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker275 fun) (fanWalker275 arg) (\_ -> fanWalker275 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker138 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker184 body) Nothing (\inner -> fanWalker184 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 277; branches to fanWalker276, fanWalker139, fanWalker185.
fanWalker277 :: Core.Term -> Maybe Core.Term
fanWalker277 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker276 fun) (fanWalker276 arg) (\_ -> fanWalker276 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker139 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker185 body) Nothing (\inner -> fanWalker185 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 278; branches to fanWalker277, fanWalker139, fanWalker186.
fanWalker278 :: Core.Term -> Maybe Core.Term
fanWalker278 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker277 fun) (fanWalker277 arg) (\_ -> fanWalker277 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker139 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker186 body) Nothing (\inner -> fanWalker186 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 279; branches to fanWalker278, fanWalker140, fanWalker186.
fanWalker279 :: Core.Term -> Maybe Core.Term
fanWalker279 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker278 fun) (fanWalker278 arg) (\_ -> fanWalker278 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker140 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker186 body) Nothing (\inner -> fanWalker186 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 28; branches to fanWalker27, fanWalker14, fanWalker19.
fanWalker28 :: Core.Term -> Maybe Core.Term
fanWalker28 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker27 fun) (fanWalker27 arg) (\_ -> fanWalker27 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker14 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker19 body) Nothing (\inner -> fanWalker19 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 280; branches to fanWalker279, fanWalker140, fanWalker187.
fanWalker280 :: Core.Term -> Maybe Core.Term
fanWalker280 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker279 fun) (fanWalker279 arg) (\_ -> fanWalker279 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker140 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker187 body) Nothing (\inner -> fanWalker187 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 281; branches to fanWalker280, fanWalker141, fanWalker188.
fanWalker281 :: Core.Term -> Maybe Core.Term
fanWalker281 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker280 fun) (fanWalker280 arg) (\_ -> fanWalker280 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker141 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker188 body) Nothing (\inner -> fanWalker188 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 282; branches to fanWalker281, fanWalker141, fanWalker188.
fanWalker282 :: Core.Term -> Maybe Core.Term
fanWalker282 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker281 fun) (fanWalker281 arg) (\_ -> fanWalker281 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker141 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker188 body) Nothing (\inner -> fanWalker188 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 283; branches to fanWalker282, fanWalker142, fanWalker189.
fanWalker283 :: Core.Term -> Maybe Core.Term
fanWalker283 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker282 fun) (fanWalker282 arg) (\_ -> fanWalker282 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker142 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker189 body) Nothing (\inner -> fanWalker189 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 284; branches to fanWalker283, fanWalker142, fanWalker190.
fanWalker284 :: Core.Term -> Maybe Core.Term
fanWalker284 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker283 fun) (fanWalker283 arg) (\_ -> fanWalker283 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker142 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker190 body) Nothing (\inner -> fanWalker190 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 285; branches to fanWalker284, fanWalker143, fanWalker190.
fanWalker285 :: Core.Term -> Maybe Core.Term
fanWalker285 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker284 fun) (fanWalker284 arg) (\_ -> fanWalker284 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker143 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker190 body) Nothing (\inner -> fanWalker190 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 286; branches to fanWalker285, fanWalker143, fanWalker191.
fanWalker286 :: Core.Term -> Maybe Core.Term
fanWalker286 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker285 fun) (fanWalker285 arg) (\_ -> fanWalker285 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker143 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker191 body) Nothing (\inner -> fanWalker191 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 287; branches to fanWalker286, fanWalker144, fanWalker192.
fanWalker287 :: Core.Term -> Maybe Core.Term
fanWalker287 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker286 fun) (fanWalker286 arg) (\_ -> fanWalker286 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker144 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker192 body) Nothing (\inner -> fanWalker192 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 288; branches to fanWalker287, fanWalker144, fanWalker192.
fanWalker288 :: Core.Term -> Maybe Core.Term
fanWalker288 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker287 fun) (fanWalker287 arg) (\_ -> fanWalker287 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker144 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker192 body) Nothing (\inner -> fanWalker192 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 289; branches to fanWalker288, fanWalker145, fanWalker193.
fanWalker289 :: Core.Term -> Maybe Core.Term
fanWalker289 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker288 fun) (fanWalker288 arg) (\_ -> fanWalker288 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker145 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker193 body) Nothing (\inner -> fanWalker193 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 29; branches to fanWalker28, fanWalker15, fanWalker20.
fanWalker29 :: Core.Term -> Maybe Core.Term
fanWalker29 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker28 fun) (fanWalker28 arg) (\_ -> fanWalker28 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker15 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker20 body) Nothing (\inner -> fanWalker20 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 290; branches to fanWalker289, fanWalker145, fanWalker194.
fanWalker290 :: Core.Term -> Maybe Core.Term
fanWalker290 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker289 fun) (fanWalker289 arg) (\_ -> fanWalker289 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker145 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker194 body) Nothing (\inner -> fanWalker194 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 291; branches to fanWalker290, fanWalker146, fanWalker194.
fanWalker291 :: Core.Term -> Maybe Core.Term
fanWalker291 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker290 fun) (fanWalker290 arg) (\_ -> fanWalker290 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker146 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker194 body) Nothing (\inner -> fanWalker194 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 292; branches to fanWalker291, fanWalker146, fanWalker195.
fanWalker292 :: Core.Term -> Maybe Core.Term
fanWalker292 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker291 fun) (fanWalker291 arg) (\_ -> fanWalker291 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker146 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker195 body) Nothing (\inner -> fanWalker195 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 293; branches to fanWalker292, fanWalker147, fanWalker196.
fanWalker293 :: Core.Term -> Maybe Core.Term
fanWalker293 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker292 fun) (fanWalker292 arg) (\_ -> fanWalker292 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker147 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker196 body) Nothing (\inner -> fanWalker196 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 294; branches to fanWalker293, fanWalker147, fanWalker196.
fanWalker294 :: Core.Term -> Maybe Core.Term
fanWalker294 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker293 fun) (fanWalker293 arg) (\_ -> fanWalker293 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker147 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker196 body) Nothing (\inner -> fanWalker196 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 295; branches to fanWalker294, fanWalker148, fanWalker197.
fanWalker295 :: Core.Term -> Maybe Core.Term
fanWalker295 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker294 fun) (fanWalker294 arg) (\_ -> fanWalker294 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker148 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker197 body) Nothing (\inner -> fanWalker197 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 296; branches to fanWalker295, fanWalker148, fanWalker198.
fanWalker296 :: Core.Term -> Maybe Core.Term
fanWalker296 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker295 fun) (fanWalker295 arg) (\_ -> fanWalker295 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker148 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker198 body) Nothing (\inner -> fanWalker198 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 297; branches to fanWalker296, fanWalker149, fanWalker198.
fanWalker297 :: Core.Term -> Maybe Core.Term
fanWalker297 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker296 fun) (fanWalker296 arg) (\_ -> fanWalker296 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker149 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker198 body) Nothing (\inner -> fanWalker198 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 298; branches to fanWalker297, fanWalker149, fanWalker199.
fanWalker298 :: Core.Term -> Maybe Core.Term
fanWalker298 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker297 fun) (fanWalker297 arg) (\_ -> fanWalker297 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker149 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker199 body) Nothing (\inner -> fanWalker199 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 299; branches to fanWalker298, fanWalker150, fanWalker200.
fanWalker299 :: Core.Term -> Maybe Core.Term
fanWalker299 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker298 fun) (fanWalker298 arg) (\_ -> fanWalker298 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker150 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker200 body) Nothing (\inner -> fanWalker200 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 3; branches to fanWalker2, fanWalker2, fanWalker2.
fanWalker3 :: Core.Term -> Maybe Core.Term
fanWalker3 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker2 fun) (fanWalker2 arg) (\_ -> fanWalker2 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker2 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker2 body) Nothing (\inner -> fanWalker2 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 30; branches to fanWalker29, fanWalker15, fanWalker20.
fanWalker30 :: Core.Term -> Maybe Core.Term
fanWalker30 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker29 fun) (fanWalker29 arg) (\_ -> fanWalker29 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker15 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker20 body) Nothing (\inner -> fanWalker20 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 300; branches to fanWalker299, fanWalker150, fanWalker200.
fanWalker300 :: Core.Term -> Maybe Core.Term
fanWalker300 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker299 fun) (fanWalker299 arg) (\_ -> fanWalker299 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker150 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker200 body) Nothing (\inner -> fanWalker200 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 301; branches to fanWalker300, fanWalker151, fanWalker201.
fanWalker301 :: Core.Term -> Maybe Core.Term
fanWalker301 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker300 fun) (fanWalker300 arg) (\_ -> fanWalker300 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker151 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker201 body) Nothing (\inner -> fanWalker201 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 302; branches to fanWalker301, fanWalker151, fanWalker202.
fanWalker302 :: Core.Term -> Maybe Core.Term
fanWalker302 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker301 fun) (fanWalker301 arg) (\_ -> fanWalker301 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker151 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker202 body) Nothing (\inner -> fanWalker202 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 303; branches to fanWalker302, fanWalker152, fanWalker202.
fanWalker303 :: Core.Term -> Maybe Core.Term
fanWalker303 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker302 fun) (fanWalker302 arg) (\_ -> fanWalker302 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker152 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker202 body) Nothing (\inner -> fanWalker202 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 304; branches to fanWalker303, fanWalker152, fanWalker203.
fanWalker304 :: Core.Term -> Maybe Core.Term
fanWalker304 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker303 fun) (fanWalker303 arg) (\_ -> fanWalker303 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker152 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker203 body) Nothing (\inner -> fanWalker203 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 305; branches to fanWalker304, fanWalker153, fanWalker204.
fanWalker305 :: Core.Term -> Maybe Core.Term
fanWalker305 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker304 fun) (fanWalker304 arg) (\_ -> fanWalker304 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker153 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker204 body) Nothing (\inner -> fanWalker204 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 306; branches to fanWalker305, fanWalker153, fanWalker204.
fanWalker306 :: Core.Term -> Maybe Core.Term
fanWalker306 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker305 fun) (fanWalker305 arg) (\_ -> fanWalker305 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker153 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker204 body) Nothing (\inner -> fanWalker204 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 307; branches to fanWalker306, fanWalker154, fanWalker205.
fanWalker307 :: Core.Term -> Maybe Core.Term
fanWalker307 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker306 fun) (fanWalker306 arg) (\_ -> fanWalker306 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker154 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker205 body) Nothing (\inner -> fanWalker205 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 308; branches to fanWalker307, fanWalker154, fanWalker206.
fanWalker308 :: Core.Term -> Maybe Core.Term
fanWalker308 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker307 fun) (fanWalker307 arg) (\_ -> fanWalker307 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker154 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker206 body) Nothing (\inner -> fanWalker206 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 309; branches to fanWalker308, fanWalker155, fanWalker206.
fanWalker309 :: Core.Term -> Maybe Core.Term
fanWalker309 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker308 fun) (fanWalker308 arg) (\_ -> fanWalker308 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker155 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker206 body) Nothing (\inner -> fanWalker206 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 31; branches to fanWalker30, fanWalker16, fanWalker21.
fanWalker31 :: Core.Term -> Maybe Core.Term
fanWalker31 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker30 fun) (fanWalker30 arg) (\_ -> fanWalker30 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker16 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker21 body) Nothing (\inner -> fanWalker21 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 310; branches to fanWalker309, fanWalker155, fanWalker207.
fanWalker310 :: Core.Term -> Maybe Core.Term
fanWalker310 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker309 fun) (fanWalker309 arg) (\_ -> fanWalker309 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker155 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker207 body) Nothing (\inner -> fanWalker207 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 311; branches to fanWalker310, fanWalker156, fanWalker208.
fanWalker311 :: Core.Term -> Maybe Core.Term
fanWalker311 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker310 fun) (fanWalker310 arg) (\_ -> fanWalker310 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker156 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker208 body) Nothing (\inner -> fanWalker208 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 312; branches to fanWalker311, fanWalker156, fanWalker208.
fanWalker312 :: Core.Term -> Maybe Core.Term
fanWalker312 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker311 fun) (fanWalker311 arg) (\_ -> fanWalker311 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker156 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker208 body) Nothing (\inner -> fanWalker208 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 313; branches to fanWalker312, fanWalker157, fanWalker209.
fanWalker313 :: Core.Term -> Maybe Core.Term
fanWalker313 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker312 fun) (fanWalker312 arg) (\_ -> fanWalker312 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker157 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker209 body) Nothing (\inner -> fanWalker209 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 314; branches to fanWalker313, fanWalker157, fanWalker210.
fanWalker314 :: Core.Term -> Maybe Core.Term
fanWalker314 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker313 fun) (fanWalker313 arg) (\_ -> fanWalker313 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker157 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker210 body) Nothing (\inner -> fanWalker210 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 315; branches to fanWalker314, fanWalker158, fanWalker210.
fanWalker315 :: Core.Term -> Maybe Core.Term
fanWalker315 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker314 fun) (fanWalker314 arg) (\_ -> fanWalker314 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker158 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker210 body) Nothing (\inner -> fanWalker210 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 316; branches to fanWalker315, fanWalker158, fanWalker211.
fanWalker316 :: Core.Term -> Maybe Core.Term
fanWalker316 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker315 fun) (fanWalker315 arg) (\_ -> fanWalker315 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker158 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker211 body) Nothing (\inner -> fanWalker211 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 317; branches to fanWalker316, fanWalker159, fanWalker212.
fanWalker317 :: Core.Term -> Maybe Core.Term
fanWalker317 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker316 fun) (fanWalker316 arg) (\_ -> fanWalker316 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker159 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker212 body) Nothing (\inner -> fanWalker212 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 318; branches to fanWalker317, fanWalker159, fanWalker212.
fanWalker318 :: Core.Term -> Maybe Core.Term
fanWalker318 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker317 fun) (fanWalker317 arg) (\_ -> fanWalker317 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker159 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker212 body) Nothing (\inner -> fanWalker212 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 319; branches to fanWalker318, fanWalker160, fanWalker213.
fanWalker319 :: Core.Term -> Maybe Core.Term
fanWalker319 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker318 fun) (fanWalker318 arg) (\_ -> fanWalker318 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker160 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker213 body) Nothing (\inner -> fanWalker213 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 32; branches to fanWalker31, fanWalker16, fanWalker22.
fanWalker32 :: Core.Term -> Maybe Core.Term
fanWalker32 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker31 fun) (fanWalker31 arg) (\_ -> fanWalker31 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker16 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker22 body) Nothing (\inner -> fanWalker22 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 320; branches to fanWalker319, fanWalker160, fanWalker214.
fanWalker320 :: Core.Term -> Maybe Core.Term
fanWalker320 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker319 fun) (fanWalker319 arg) (\_ -> fanWalker319 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker160 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker214 body) Nothing (\inner -> fanWalker214 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 321; branches to fanWalker320, fanWalker161, fanWalker214.
fanWalker321 :: Core.Term -> Maybe Core.Term
fanWalker321 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker320 fun) (fanWalker320 arg) (\_ -> fanWalker320 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker161 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker214 body) Nothing (\inner -> fanWalker214 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 322; branches to fanWalker321, fanWalker161, fanWalker215.
fanWalker322 :: Core.Term -> Maybe Core.Term
fanWalker322 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker321 fun) (fanWalker321 arg) (\_ -> fanWalker321 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker161 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker215 body) Nothing (\inner -> fanWalker215 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 323; branches to fanWalker322, fanWalker162, fanWalker216.
fanWalker323 :: Core.Term -> Maybe Core.Term
fanWalker323 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker322 fun) (fanWalker322 arg) (\_ -> fanWalker322 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker162 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker216 body) Nothing (\inner -> fanWalker216 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 324; branches to fanWalker323, fanWalker162, fanWalker216.
fanWalker324 :: Core.Term -> Maybe Core.Term
fanWalker324 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker323 fun) (fanWalker323 arg) (\_ -> fanWalker323 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker162 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker216 body) Nothing (\inner -> fanWalker216 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 325; branches to fanWalker324, fanWalker163, fanWalker217.
fanWalker325 :: Core.Term -> Maybe Core.Term
fanWalker325 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker324 fun) (fanWalker324 arg) (\_ -> fanWalker324 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker163 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker217 body) Nothing (\inner -> fanWalker217 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 326; branches to fanWalker325, fanWalker163, fanWalker218.
fanWalker326 :: Core.Term -> Maybe Core.Term
fanWalker326 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker325 fun) (fanWalker325 arg) (\_ -> fanWalker325 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker163 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker218 body) Nothing (\inner -> fanWalker218 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 327; branches to fanWalker326, fanWalker164, fanWalker218.
fanWalker327 :: Core.Term -> Maybe Core.Term
fanWalker327 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker326 fun) (fanWalker326 arg) (\_ -> fanWalker326 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker164 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker218 body) Nothing (\inner -> fanWalker218 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 328; branches to fanWalker327, fanWalker164, fanWalker219.
fanWalker328 :: Core.Term -> Maybe Core.Term
fanWalker328 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker327 fun) (fanWalker327 arg) (\_ -> fanWalker327 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker164 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker219 body) Nothing (\inner -> fanWalker219 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 329; branches to fanWalker328, fanWalker165, fanWalker220.
fanWalker329 :: Core.Term -> Maybe Core.Term
fanWalker329 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker328 fun) (fanWalker328 arg) (\_ -> fanWalker328 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker165 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker220 body) Nothing (\inner -> fanWalker220 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 33; branches to fanWalker32, fanWalker17, fanWalker22.
fanWalker33 :: Core.Term -> Maybe Core.Term
fanWalker33 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker32 fun) (fanWalker32 arg) (\_ -> fanWalker32 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker17 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker22 body) Nothing (\inner -> fanWalker22 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 330; branches to fanWalker329, fanWalker165, fanWalker220.
fanWalker330 :: Core.Term -> Maybe Core.Term
fanWalker330 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker329 fun) (fanWalker329 arg) (\_ -> fanWalker329 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker165 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker220 body) Nothing (\inner -> fanWalker220 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 331; branches to fanWalker330, fanWalker166, fanWalker221.
fanWalker331 :: Core.Term -> Maybe Core.Term
fanWalker331 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker330 fun) (fanWalker330 arg) (\_ -> fanWalker330 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker166 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker221 body) Nothing (\inner -> fanWalker221 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 332; branches to fanWalker331, fanWalker166, fanWalker222.
fanWalker332 :: Core.Term -> Maybe Core.Term
fanWalker332 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker331 fun) (fanWalker331 arg) (\_ -> fanWalker331 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker166 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker222 body) Nothing (\inner -> fanWalker222 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 333; branches to fanWalker332, fanWalker167, fanWalker222.
fanWalker333 :: Core.Term -> Maybe Core.Term
fanWalker333 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker332 fun) (fanWalker332 arg) (\_ -> fanWalker332 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker167 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker222 body) Nothing (\inner -> fanWalker222 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 334; branches to fanWalker333, fanWalker167, fanWalker223.
fanWalker334 :: Core.Term -> Maybe Core.Term
fanWalker334 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker333 fun) (fanWalker333 arg) (\_ -> fanWalker333 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker167 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker223 body) Nothing (\inner -> fanWalker223 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 335; branches to fanWalker334, fanWalker168, fanWalker224.
fanWalker335 :: Core.Term -> Maybe Core.Term
fanWalker335 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker334 fun) (fanWalker334 arg) (\_ -> fanWalker334 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker168 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker224 body) Nothing (\inner -> fanWalker224 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 336; branches to fanWalker335, fanWalker168, fanWalker224.
fanWalker336 :: Core.Term -> Maybe Core.Term
fanWalker336 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker335 fun) (fanWalker335 arg) (\_ -> fanWalker335 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker168 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker224 body) Nothing (\inner -> fanWalker224 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 337; branches to fanWalker336, fanWalker169, fanWalker225.
fanWalker337 :: Core.Term -> Maybe Core.Term
fanWalker337 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker336 fun) (fanWalker336 arg) (\_ -> fanWalker336 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker169 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker225 body) Nothing (\inner -> fanWalker225 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 338; branches to fanWalker337, fanWalker169, fanWalker226.
fanWalker338 :: Core.Term -> Maybe Core.Term
fanWalker338 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker337 fun) (fanWalker337 arg) (\_ -> fanWalker337 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker169 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker226 body) Nothing (\inner -> fanWalker226 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 339; branches to fanWalker338, fanWalker170, fanWalker226.
fanWalker339 :: Core.Term -> Maybe Core.Term
fanWalker339 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker338 fun) (fanWalker338 arg) (\_ -> fanWalker338 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker170 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker226 body) Nothing (\inner -> fanWalker226 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 34; branches to fanWalker33, fanWalker17, fanWalker23.
fanWalker34 :: Core.Term -> Maybe Core.Term
fanWalker34 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker33 fun) (fanWalker33 arg) (\_ -> fanWalker33 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker17 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker23 body) Nothing (\inner -> fanWalker23 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 340; branches to fanWalker339, fanWalker170, fanWalker227.
fanWalker340 :: Core.Term -> Maybe Core.Term
fanWalker340 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker339 fun) (fanWalker339 arg) (\_ -> fanWalker339 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker170 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker227 body) Nothing (\inner -> fanWalker227 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 341; branches to fanWalker340, fanWalker171, fanWalker228.
fanWalker341 :: Core.Term -> Maybe Core.Term
fanWalker341 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker340 fun) (fanWalker340 arg) (\_ -> fanWalker340 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker171 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker228 body) Nothing (\inner -> fanWalker228 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 342; branches to fanWalker341, fanWalker171, fanWalker228.
fanWalker342 :: Core.Term -> Maybe Core.Term
fanWalker342 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker341 fun) (fanWalker341 arg) (\_ -> fanWalker341 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker171 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker228 body) Nothing (\inner -> fanWalker228 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 343; branches to fanWalker342, fanWalker172, fanWalker229.
fanWalker343 :: Core.Term -> Maybe Core.Term
fanWalker343 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker342 fun) (fanWalker342 arg) (\_ -> fanWalker342 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker172 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker229 body) Nothing (\inner -> fanWalker229 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 344; branches to fanWalker343, fanWalker172, fanWalker230.
fanWalker344 :: Core.Term -> Maybe Core.Term
fanWalker344 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker343 fun) (fanWalker343 arg) (\_ -> fanWalker343 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker172 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker230 body) Nothing (\inner -> fanWalker230 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 345; branches to fanWalker344, fanWalker173, fanWalker230.
fanWalker345 :: Core.Term -> Maybe Core.Term
fanWalker345 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker344 fun) (fanWalker344 arg) (\_ -> fanWalker344 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker173 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker230 body) Nothing (\inner -> fanWalker230 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 346; branches to fanWalker345, fanWalker173, fanWalker231.
fanWalker346 :: Core.Term -> Maybe Core.Term
fanWalker346 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker345 fun) (fanWalker345 arg) (\_ -> fanWalker345 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker173 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker231 body) Nothing (\inner -> fanWalker231 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 347; branches to fanWalker346, fanWalker174, fanWalker232.
fanWalker347 :: Core.Term -> Maybe Core.Term
fanWalker347 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker346 fun) (fanWalker346 arg) (\_ -> fanWalker346 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker174 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker232 body) Nothing (\inner -> fanWalker232 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 348; branches to fanWalker347, fanWalker174, fanWalker232.
fanWalker348 :: Core.Term -> Maybe Core.Term
fanWalker348 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker347 fun) (fanWalker347 arg) (\_ -> fanWalker347 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker174 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker232 body) Nothing (\inner -> fanWalker232 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 349; branches to fanWalker348, fanWalker175, fanWalker233.
fanWalker349 :: Core.Term -> Maybe Core.Term
fanWalker349 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker348 fun) (fanWalker348 arg) (\_ -> fanWalker348 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker175 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker233 body) Nothing (\inner -> fanWalker233 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 35; branches to fanWalker34, fanWalker18, fanWalker24.
fanWalker35 :: Core.Term -> Maybe Core.Term
fanWalker35 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker34 fun) (fanWalker34 arg) (\_ -> fanWalker34 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker18 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker24 body) Nothing (\inner -> fanWalker24 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 350; branches to fanWalker349, fanWalker175, fanWalker234.
fanWalker350 :: Core.Term -> Maybe Core.Term
fanWalker350 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker349 fun) (fanWalker349 arg) (\_ -> fanWalker349 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker175 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker234 body) Nothing (\inner -> fanWalker234 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 351; branches to fanWalker350, fanWalker176, fanWalker234.
fanWalker351 :: Core.Term -> Maybe Core.Term
fanWalker351 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker350 fun) (fanWalker350 arg) (\_ -> fanWalker350 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker176 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker234 body) Nothing (\inner -> fanWalker234 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 352; branches to fanWalker351, fanWalker176, fanWalker235.
fanWalker352 :: Core.Term -> Maybe Core.Term
fanWalker352 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker351 fun) (fanWalker351 arg) (\_ -> fanWalker351 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker176 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker235 body) Nothing (\inner -> fanWalker235 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 353; branches to fanWalker352, fanWalker177, fanWalker236.
fanWalker353 :: Core.Term -> Maybe Core.Term
fanWalker353 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker352 fun) (fanWalker352 arg) (\_ -> fanWalker352 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker177 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker236 body) Nothing (\inner -> fanWalker236 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 354; branches to fanWalker353, fanWalker177, fanWalker236.
fanWalker354 :: Core.Term -> Maybe Core.Term
fanWalker354 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker353 fun) (fanWalker353 arg) (\_ -> fanWalker353 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker177 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker236 body) Nothing (\inner -> fanWalker236 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 355; branches to fanWalker354, fanWalker178, fanWalker237.
fanWalker355 :: Core.Term -> Maybe Core.Term
fanWalker355 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker354 fun) (fanWalker354 arg) (\_ -> fanWalker354 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker178 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker237 body) Nothing (\inner -> fanWalker237 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 356; branches to fanWalker355, fanWalker178, fanWalker238.
fanWalker356 :: Core.Term -> Maybe Core.Term
fanWalker356 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker355 fun) (fanWalker355 arg) (\_ -> fanWalker355 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker178 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker238 body) Nothing (\inner -> fanWalker238 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 357; branches to fanWalker356, fanWalker179, fanWalker238.
fanWalker357 :: Core.Term -> Maybe Core.Term
fanWalker357 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker356 fun) (fanWalker356 arg) (\_ -> fanWalker356 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker179 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker238 body) Nothing (\inner -> fanWalker238 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 358; branches to fanWalker357, fanWalker179, fanWalker239.
fanWalker358 :: Core.Term -> Maybe Core.Term
fanWalker358 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker357 fun) (fanWalker357 arg) (\_ -> fanWalker357 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker179 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker239 body) Nothing (\inner -> fanWalker239 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 359; branches to fanWalker358, fanWalker180, fanWalker240.
fanWalker359 :: Core.Term -> Maybe Core.Term
fanWalker359 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker358 fun) (fanWalker358 arg) (\_ -> fanWalker358 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker180 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker240 body) Nothing (\inner -> fanWalker240 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 36; branches to fanWalker35, fanWalker18, fanWalker24.
fanWalker36 :: Core.Term -> Maybe Core.Term
fanWalker36 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker35 fun) (fanWalker35 arg) (\_ -> fanWalker35 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker18 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker24 body) Nothing (\inner -> fanWalker24 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 360; branches to fanWalker359, fanWalker180, fanWalker240.
fanWalker360 :: Core.Term -> Maybe Core.Term
fanWalker360 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker359 fun) (fanWalker359 arg) (\_ -> fanWalker359 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker180 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker240 body) Nothing (\inner -> fanWalker240 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 361; branches to fanWalker360, fanWalker181, fanWalker241.
fanWalker361 :: Core.Term -> Maybe Core.Term
fanWalker361 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker360 fun) (fanWalker360 arg) (\_ -> fanWalker360 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker181 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker241 body) Nothing (\inner -> fanWalker241 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 362; branches to fanWalker361, fanWalker181, fanWalker242.
fanWalker362 :: Core.Term -> Maybe Core.Term
fanWalker362 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker361 fun) (fanWalker361 arg) (\_ -> fanWalker361 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker181 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker242 body) Nothing (\inner -> fanWalker242 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 363; branches to fanWalker362, fanWalker182, fanWalker242.
fanWalker363 :: Core.Term -> Maybe Core.Term
fanWalker363 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker362 fun) (fanWalker362 arg) (\_ -> fanWalker362 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker182 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker242 body) Nothing (\inner -> fanWalker242 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 364; branches to fanWalker363, fanWalker182, fanWalker243.
fanWalker364 :: Core.Term -> Maybe Core.Term
fanWalker364 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker363 fun) (fanWalker363 arg) (\_ -> fanWalker363 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker182 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker243 body) Nothing (\inner -> fanWalker243 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 365; branches to fanWalker364, fanWalker183, fanWalker244.
fanWalker365 :: Core.Term -> Maybe Core.Term
fanWalker365 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker364 fun) (fanWalker364 arg) (\_ -> fanWalker364 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker183 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker244 body) Nothing (\inner -> fanWalker244 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 366; branches to fanWalker365, fanWalker183, fanWalker244.
fanWalker366 :: Core.Term -> Maybe Core.Term
fanWalker366 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker365 fun) (fanWalker365 arg) (\_ -> fanWalker365 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker183 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker244 body) Nothing (\inner -> fanWalker244 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 367; branches to fanWalker366, fanWalker184, fanWalker245.
fanWalker367 :: Core.Term -> Maybe Core.Term
fanWalker367 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker366 fun) (fanWalker366 arg) (\_ -> fanWalker366 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker184 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker245 body) Nothing (\inner -> fanWalker245 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 368; branches to fanWalker367, fanWalker184, fanWalker246.
fanWalker368 :: Core.Term -> Maybe Core.Term
fanWalker368 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker367 fun) (fanWalker367 arg) (\_ -> fanWalker367 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker184 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker246 body) Nothing (\inner -> fanWalker246 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 369; branches to fanWalker368, fanWalker185, fanWalker246.
fanWalker369 :: Core.Term -> Maybe Core.Term
fanWalker369 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker368 fun) (fanWalker368 arg) (\_ -> fanWalker368 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker185 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker246 body) Nothing (\inner -> fanWalker246 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 37; branches to fanWalker36, fanWalker19, fanWalker25.
fanWalker37 :: Core.Term -> Maybe Core.Term
fanWalker37 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker36 fun) (fanWalker36 arg) (\_ -> fanWalker36 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker19 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker25 body) Nothing (\inner -> fanWalker25 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 370; branches to fanWalker369, fanWalker185, fanWalker247.
fanWalker370 :: Core.Term -> Maybe Core.Term
fanWalker370 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker369 fun) (fanWalker369 arg) (\_ -> fanWalker369 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker185 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker247 body) Nothing (\inner -> fanWalker247 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 371; branches to fanWalker370, fanWalker186, fanWalker248.
fanWalker371 :: Core.Term -> Maybe Core.Term
fanWalker371 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker370 fun) (fanWalker370 arg) (\_ -> fanWalker370 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker186 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker248 body) Nothing (\inner -> fanWalker248 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 372; branches to fanWalker371, fanWalker186, fanWalker248.
fanWalker372 :: Core.Term -> Maybe Core.Term
fanWalker372 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker371 fun) (fanWalker371 arg) (\_ -> fanWalker371 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker186 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker248 body) Nothing (\inner -> fanWalker248 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 373; branches to fanWalker372, fanWalker187, fanWalker249.
fanWalker373 :: Core.Term -> Maybe Core.Term
fanWalker373 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker372 fun) (fanWalker372 arg) (\_ -> fanWalker372 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker187 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker249 body) Nothing (\inner -> fanWalker249 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 374; branches to fanWalker373, fanWalker187, fanWalker250.
fanWalker374 :: Core.Term -> Maybe Core.Term
fanWalker374 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker373 fun) (fanWalker373 arg) (\_ -> fanWalker373 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker187 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker250 body) Nothing (\inner -> fanWalker250 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 375; branches to fanWalker374, fanWalker188, fanWalker250.
fanWalker375 :: Core.Term -> Maybe Core.Term
fanWalker375 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker374 fun) (fanWalker374 arg) (\_ -> fanWalker374 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker188 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker250 body) Nothing (\inner -> fanWalker250 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 376; branches to fanWalker375, fanWalker188, fanWalker251.
fanWalker376 :: Core.Term -> Maybe Core.Term
fanWalker376 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker375 fun) (fanWalker375 arg) (\_ -> fanWalker375 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker188 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker251 body) Nothing (\inner -> fanWalker251 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 377; branches to fanWalker376, fanWalker189, fanWalker252.
fanWalker377 :: Core.Term -> Maybe Core.Term
fanWalker377 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker376 fun) (fanWalker376 arg) (\_ -> fanWalker376 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker189 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker252 body) Nothing (\inner -> fanWalker252 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 378; branches to fanWalker377, fanWalker189, fanWalker252.
fanWalker378 :: Core.Term -> Maybe Core.Term
fanWalker378 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker377 fun) (fanWalker377 arg) (\_ -> fanWalker377 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker189 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker252 body) Nothing (\inner -> fanWalker252 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 379; branches to fanWalker378, fanWalker190, fanWalker253.
fanWalker379 :: Core.Term -> Maybe Core.Term
fanWalker379 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker378 fun) (fanWalker378 arg) (\_ -> fanWalker378 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker190 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker253 body) Nothing (\inner -> fanWalker253 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 38; branches to fanWalker37, fanWalker19, fanWalker26.
fanWalker38 :: Core.Term -> Maybe Core.Term
fanWalker38 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker37 fun) (fanWalker37 arg) (\_ -> fanWalker37 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker19 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker26 body) Nothing (\inner -> fanWalker26 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 380; branches to fanWalker379, fanWalker190, fanWalker254.
fanWalker380 :: Core.Term -> Maybe Core.Term
fanWalker380 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker379 fun) (fanWalker379 arg) (\_ -> fanWalker379 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker190 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker254 body) Nothing (\inner -> fanWalker254 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 381; branches to fanWalker380, fanWalker191, fanWalker254.
fanWalker381 :: Core.Term -> Maybe Core.Term
fanWalker381 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker380 fun) (fanWalker380 arg) (\_ -> fanWalker380 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker191 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker254 body) Nothing (\inner -> fanWalker254 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 382; branches to fanWalker381, fanWalker191, fanWalker255.
fanWalker382 :: Core.Term -> Maybe Core.Term
fanWalker382 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker381 fun) (fanWalker381 arg) (\_ -> fanWalker381 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker191 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker255 body) Nothing (\inner -> fanWalker255 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 383; branches to fanWalker382, fanWalker192, fanWalker256.
fanWalker383 :: Core.Term -> Maybe Core.Term
fanWalker383 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker382 fun) (fanWalker382 arg) (\_ -> fanWalker382 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker192 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker256 body) Nothing (\inner -> fanWalker256 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 384; branches to fanWalker383, fanWalker192, fanWalker256.
fanWalker384 :: Core.Term -> Maybe Core.Term
fanWalker384 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker383 fun) (fanWalker383 arg) (\_ -> fanWalker383 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker192 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker256 body) Nothing (\inner -> fanWalker256 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 385; branches to fanWalker384, fanWalker193, fanWalker257.
fanWalker385 :: Core.Term -> Maybe Core.Term
fanWalker385 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker384 fun) (fanWalker384 arg) (\_ -> fanWalker384 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker193 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker257 body) Nothing (\inner -> fanWalker257 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 386; branches to fanWalker385, fanWalker193, fanWalker258.
fanWalker386 :: Core.Term -> Maybe Core.Term
fanWalker386 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker385 fun) (fanWalker385 arg) (\_ -> fanWalker385 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker193 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker258 body) Nothing (\inner -> fanWalker258 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 387; branches to fanWalker386, fanWalker194, fanWalker258.
fanWalker387 :: Core.Term -> Maybe Core.Term
fanWalker387 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker386 fun) (fanWalker386 arg) (\_ -> fanWalker386 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker194 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker258 body) Nothing (\inner -> fanWalker258 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 388; branches to fanWalker387, fanWalker194, fanWalker259.
fanWalker388 :: Core.Term -> Maybe Core.Term
fanWalker388 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker387 fun) (fanWalker387 arg) (\_ -> fanWalker387 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker194 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker259 body) Nothing (\inner -> fanWalker259 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 389; branches to fanWalker388, fanWalker195, fanWalker260.
fanWalker389 :: Core.Term -> Maybe Core.Term
fanWalker389 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker388 fun) (fanWalker388 arg) (\_ -> fanWalker388 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker195 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker260 body) Nothing (\inner -> fanWalker260 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 39; branches to fanWalker38, fanWalker20, fanWalker26.
fanWalker39 :: Core.Term -> Maybe Core.Term
fanWalker39 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker38 fun) (fanWalker38 arg) (\_ -> fanWalker38 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker20 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker26 body) Nothing (\inner -> fanWalker26 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 390; branches to fanWalker389, fanWalker195, fanWalker260.
fanWalker390 :: Core.Term -> Maybe Core.Term
fanWalker390 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker389 fun) (fanWalker389 arg) (\_ -> fanWalker389 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker195 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker260 body) Nothing (\inner -> fanWalker260 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 391; branches to fanWalker390, fanWalker196, fanWalker261.
fanWalker391 :: Core.Term -> Maybe Core.Term
fanWalker391 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker390 fun) (fanWalker390 arg) (\_ -> fanWalker390 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker196 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker261 body) Nothing (\inner -> fanWalker261 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 392; branches to fanWalker391, fanWalker196, fanWalker262.
fanWalker392 :: Core.Term -> Maybe Core.Term
fanWalker392 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker391 fun) (fanWalker391 arg) (\_ -> fanWalker391 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker196 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker262 body) Nothing (\inner -> fanWalker262 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 393; branches to fanWalker392, fanWalker197, fanWalker262.
fanWalker393 :: Core.Term -> Maybe Core.Term
fanWalker393 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker392 fun) (fanWalker392 arg) (\_ -> fanWalker392 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker197 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker262 body) Nothing (\inner -> fanWalker262 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 394; branches to fanWalker393, fanWalker197, fanWalker263.
fanWalker394 :: Core.Term -> Maybe Core.Term
fanWalker394 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker393 fun) (fanWalker393 arg) (\_ -> fanWalker393 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker197 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker263 body) Nothing (\inner -> fanWalker263 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 395; branches to fanWalker394, fanWalker198, fanWalker264.
fanWalker395 :: Core.Term -> Maybe Core.Term
fanWalker395 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker394 fun) (fanWalker394 arg) (\_ -> fanWalker394 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker198 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker264 body) Nothing (\inner -> fanWalker264 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 396; branches to fanWalker395, fanWalker198, fanWalker264.
fanWalker396 :: Core.Term -> Maybe Core.Term
fanWalker396 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker395 fun) (fanWalker395 arg) (\_ -> fanWalker395 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker198 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker264 body) Nothing (\inner -> fanWalker264 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 397; branches to fanWalker396, fanWalker199, fanWalker265.
fanWalker397 :: Core.Term -> Maybe Core.Term
fanWalker397 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker396 fun) (fanWalker396 arg) (\_ -> fanWalker396 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker199 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker265 body) Nothing (\inner -> fanWalker265 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 398; branches to fanWalker397, fanWalker199, fanWalker266.
fanWalker398 :: Core.Term -> Maybe Core.Term
fanWalker398 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker397 fun) (fanWalker397 arg) (\_ -> fanWalker397 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker199 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker266 body) Nothing (\inner -> fanWalker266 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 399; branches to fanWalker398, fanWalker200, fanWalker266.
fanWalker399 :: Core.Term -> Maybe Core.Term
fanWalker399 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker398 fun) (fanWalker398 arg) (\_ -> fanWalker398 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker200 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker266 body) Nothing (\inner -> fanWalker266 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 4; branches to fanWalker3, fanWalker2, fanWalker3.
fanWalker4 :: Core.Term -> Maybe Core.Term
fanWalker4 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker3 fun) (fanWalker3 arg) (\_ -> fanWalker3 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker2 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker3 body) Nothing (\inner -> fanWalker3 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 40; branches to fanWalker39, fanWalker20, fanWalker27.
fanWalker40 :: Core.Term -> Maybe Core.Term
fanWalker40 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker39 fun) (fanWalker39 arg) (\_ -> fanWalker39 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker20 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker27 body) Nothing (\inner -> fanWalker27 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 41; branches to fanWalker40, fanWalker21, fanWalker28.
fanWalker41 :: Core.Term -> Maybe Core.Term
fanWalker41 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker40 fun) (fanWalker40 arg) (\_ -> fanWalker40 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker21 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker28 body) Nothing (\inner -> fanWalker28 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 42; branches to fanWalker41, fanWalker21, fanWalker28.
fanWalker42 :: Core.Term -> Maybe Core.Term
fanWalker42 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker41 fun) (fanWalker41 arg) (\_ -> fanWalker41 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker21 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker28 body) Nothing (\inner -> fanWalker28 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 43; branches to fanWalker42, fanWalker22, fanWalker29.
fanWalker43 :: Core.Term -> Maybe Core.Term
fanWalker43 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker42 fun) (fanWalker42 arg) (\_ -> fanWalker42 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker22 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker29 body) Nothing (\inner -> fanWalker29 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 44; branches to fanWalker43, fanWalker22, fanWalker30.
fanWalker44 :: Core.Term -> Maybe Core.Term
fanWalker44 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker43 fun) (fanWalker43 arg) (\_ -> fanWalker43 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker22 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker30 body) Nothing (\inner -> fanWalker30 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 45; branches to fanWalker44, fanWalker23, fanWalker30.
fanWalker45 :: Core.Term -> Maybe Core.Term
fanWalker45 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker44 fun) (fanWalker44 arg) (\_ -> fanWalker44 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker23 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker30 body) Nothing (\inner -> fanWalker30 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 46; branches to fanWalker45, fanWalker23, fanWalker31.
fanWalker46 :: Core.Term -> Maybe Core.Term
fanWalker46 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker45 fun) (fanWalker45 arg) (\_ -> fanWalker45 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker23 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker31 body) Nothing (\inner -> fanWalker31 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 47; branches to fanWalker46, fanWalker24, fanWalker32.
fanWalker47 :: Core.Term -> Maybe Core.Term
fanWalker47 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker46 fun) (fanWalker46 arg) (\_ -> fanWalker46 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker24 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker32 body) Nothing (\inner -> fanWalker32 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 48; branches to fanWalker47, fanWalker24, fanWalker32.
fanWalker48 :: Core.Term -> Maybe Core.Term
fanWalker48 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker47 fun) (fanWalker47 arg) (\_ -> fanWalker47 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker24 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker32 body) Nothing (\inner -> fanWalker32 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 49; branches to fanWalker48, fanWalker25, fanWalker33.
fanWalker49 :: Core.Term -> Maybe Core.Term
fanWalker49 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker48 fun) (fanWalker48 arg) (\_ -> fanWalker48 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker25 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker33 body) Nothing (\inner -> fanWalker33 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 5; branches to fanWalker4, fanWalker3, fanWalker4.
fanWalker5 :: Core.Term -> Maybe Core.Term
fanWalker5 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker4 fun) (fanWalker4 arg) (\_ -> fanWalker4 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker3 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker4 body) Nothing (\inner -> fanWalker4 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 50; branches to fanWalker49, fanWalker25, fanWalker34.
fanWalker50 :: Core.Term -> Maybe Core.Term
fanWalker50 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker49 fun) (fanWalker49 arg) (\_ -> fanWalker49 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker25 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker34 body) Nothing (\inner -> fanWalker34 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 51; branches to fanWalker50, fanWalker26, fanWalker34.
fanWalker51 :: Core.Term -> Maybe Core.Term
fanWalker51 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker50 fun) (fanWalker50 arg) (\_ -> fanWalker50 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker26 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker34 body) Nothing (\inner -> fanWalker34 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 52; branches to fanWalker51, fanWalker26, fanWalker35.
fanWalker52 :: Core.Term -> Maybe Core.Term
fanWalker52 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker51 fun) (fanWalker51 arg) (\_ -> fanWalker51 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker26 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker35 body) Nothing (\inner -> fanWalker35 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 53; branches to fanWalker52, fanWalker27, fanWalker36.
fanWalker53 :: Core.Term -> Maybe Core.Term
fanWalker53 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker52 fun) (fanWalker52 arg) (\_ -> fanWalker52 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker27 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker36 body) Nothing (\inner -> fanWalker36 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 54; branches to fanWalker53, fanWalker27, fanWalker36.
fanWalker54 :: Core.Term -> Maybe Core.Term
fanWalker54 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker53 fun) (fanWalker53 arg) (\_ -> fanWalker53 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker27 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker36 body) Nothing (\inner -> fanWalker36 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 55; branches to fanWalker54, fanWalker28, fanWalker37.
fanWalker55 :: Core.Term -> Maybe Core.Term
fanWalker55 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker54 fun) (fanWalker54 arg) (\_ -> fanWalker54 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker28 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker37 body) Nothing (\inner -> fanWalker37 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 56; branches to fanWalker55, fanWalker28, fanWalker38.
fanWalker56 :: Core.Term -> Maybe Core.Term
fanWalker56 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker55 fun) (fanWalker55 arg) (\_ -> fanWalker55 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker28 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker38 body) Nothing (\inner -> fanWalker38 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 57; branches to fanWalker56, fanWalker29, fanWalker38.
fanWalker57 :: Core.Term -> Maybe Core.Term
fanWalker57 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker56 fun) (fanWalker56 arg) (\_ -> fanWalker56 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker29 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker38 body) Nothing (\inner -> fanWalker38 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 58; branches to fanWalker57, fanWalker29, fanWalker39.
fanWalker58 :: Core.Term -> Maybe Core.Term
fanWalker58 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker57 fun) (fanWalker57 arg) (\_ -> fanWalker57 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker29 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker39 body) Nothing (\inner -> fanWalker39 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 59; branches to fanWalker58, fanWalker30, fanWalker40.
fanWalker59 :: Core.Term -> Maybe Core.Term
fanWalker59 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker58 fun) (fanWalker58 arg) (\_ -> fanWalker58 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker30 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker40 body) Nothing (\inner -> fanWalker40 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 6; branches to fanWalker5, fanWalker3, fanWalker4.
fanWalker6 :: Core.Term -> Maybe Core.Term
fanWalker6 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker5 fun) (fanWalker5 arg) (\_ -> fanWalker5 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker3 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker4 body) Nothing (\inner -> fanWalker4 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 60; branches to fanWalker59, fanWalker30, fanWalker40.
fanWalker60 :: Core.Term -> Maybe Core.Term
fanWalker60 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker59 fun) (fanWalker59 arg) (\_ -> fanWalker59 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker30 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker40 body) Nothing (\inner -> fanWalker40 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 61; branches to fanWalker60, fanWalker31, fanWalker41.
fanWalker61 :: Core.Term -> Maybe Core.Term
fanWalker61 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker60 fun) (fanWalker60 arg) (\_ -> fanWalker60 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker31 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker41 body) Nothing (\inner -> fanWalker41 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 62; branches to fanWalker61, fanWalker31, fanWalker42.
fanWalker62 :: Core.Term -> Maybe Core.Term
fanWalker62 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker61 fun) (fanWalker61 arg) (\_ -> fanWalker61 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker31 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker42 body) Nothing (\inner -> fanWalker42 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 63; branches to fanWalker62, fanWalker32, fanWalker42.
fanWalker63 :: Core.Term -> Maybe Core.Term
fanWalker63 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker62 fun) (fanWalker62 arg) (\_ -> fanWalker62 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker32 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker42 body) Nothing (\inner -> fanWalker42 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 64; branches to fanWalker63, fanWalker32, fanWalker43.
fanWalker64 :: Core.Term -> Maybe Core.Term
fanWalker64 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker63 fun) (fanWalker63 arg) (\_ -> fanWalker63 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker32 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker43 body) Nothing (\inner -> fanWalker43 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 65; branches to fanWalker64, fanWalker33, fanWalker44.
fanWalker65 :: Core.Term -> Maybe Core.Term
fanWalker65 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker64 fun) (fanWalker64 arg) (\_ -> fanWalker64 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker33 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker44 body) Nothing (\inner -> fanWalker44 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 66; branches to fanWalker65, fanWalker33, fanWalker44.
fanWalker66 :: Core.Term -> Maybe Core.Term
fanWalker66 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker65 fun) (fanWalker65 arg) (\_ -> fanWalker65 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker33 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker44 body) Nothing (\inner -> fanWalker44 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 67; branches to fanWalker66, fanWalker34, fanWalker45.
fanWalker67 :: Core.Term -> Maybe Core.Term
fanWalker67 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker66 fun) (fanWalker66 arg) (\_ -> fanWalker66 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker34 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker45 body) Nothing (\inner -> fanWalker45 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 68; branches to fanWalker67, fanWalker34, fanWalker46.
fanWalker68 :: Core.Term -> Maybe Core.Term
fanWalker68 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker67 fun) (fanWalker67 arg) (\_ -> fanWalker67 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker34 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker46 body) Nothing (\inner -> fanWalker46 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 69; branches to fanWalker68, fanWalker35, fanWalker46.
fanWalker69 :: Core.Term -> Maybe Core.Term
fanWalker69 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker68 fun) (fanWalker68 arg) (\_ -> fanWalker68 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker35 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker46 body) Nothing (\inner -> fanWalker46 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 7; branches to fanWalker6, fanWalker4, fanWalker5.
fanWalker7 :: Core.Term -> Maybe Core.Term
fanWalker7 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker6 fun) (fanWalker6 arg) (\_ -> fanWalker6 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker4 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker5 body) Nothing (\inner -> fanWalker5 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 70; branches to fanWalker69, fanWalker35, fanWalker47.
fanWalker70 :: Core.Term -> Maybe Core.Term
fanWalker70 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker69 fun) (fanWalker69 arg) (\_ -> fanWalker69 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker35 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker47 body) Nothing (\inner -> fanWalker47 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 71; branches to fanWalker70, fanWalker36, fanWalker48.
fanWalker71 :: Core.Term -> Maybe Core.Term
fanWalker71 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker70 fun) (fanWalker70 arg) (\_ -> fanWalker70 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker36 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker48 body) Nothing (\inner -> fanWalker48 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 72; branches to fanWalker71, fanWalker36, fanWalker48.
fanWalker72 :: Core.Term -> Maybe Core.Term
fanWalker72 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker71 fun) (fanWalker71 arg) (\_ -> fanWalker71 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker36 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker48 body) Nothing (\inner -> fanWalker48 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 73; branches to fanWalker72, fanWalker37, fanWalker49.
fanWalker73 :: Core.Term -> Maybe Core.Term
fanWalker73 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker72 fun) (fanWalker72 arg) (\_ -> fanWalker72 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker37 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker49 body) Nothing (\inner -> fanWalker49 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 74; branches to fanWalker73, fanWalker37, fanWalker50.
fanWalker74 :: Core.Term -> Maybe Core.Term
fanWalker74 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker73 fun) (fanWalker73 arg) (\_ -> fanWalker73 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker37 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker50 body) Nothing (\inner -> fanWalker50 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 75; branches to fanWalker74, fanWalker38, fanWalker50.
fanWalker75 :: Core.Term -> Maybe Core.Term
fanWalker75 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker74 fun) (fanWalker74 arg) (\_ -> fanWalker74 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker38 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker50 body) Nothing (\inner -> fanWalker50 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 76; branches to fanWalker75, fanWalker38, fanWalker51.
fanWalker76 :: Core.Term -> Maybe Core.Term
fanWalker76 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker75 fun) (fanWalker75 arg) (\_ -> fanWalker75 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker38 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker51 body) Nothing (\inner -> fanWalker51 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 77; branches to fanWalker76, fanWalker39, fanWalker52.
fanWalker77 :: Core.Term -> Maybe Core.Term
fanWalker77 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker76 fun) (fanWalker76 arg) (\_ -> fanWalker76 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker39 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker52 body) Nothing (\inner -> fanWalker52 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 78; branches to fanWalker77, fanWalker39, fanWalker52.
fanWalker78 :: Core.Term -> Maybe Core.Term
fanWalker78 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker77 fun) (fanWalker77 arg) (\_ -> fanWalker77 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker39 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker52 body) Nothing (\inner -> fanWalker52 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 79; branches to fanWalker78, fanWalker40, fanWalker53.
fanWalker79 :: Core.Term -> Maybe Core.Term
fanWalker79 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker78 fun) (fanWalker78 arg) (\_ -> fanWalker78 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker40 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker53 body) Nothing (\inner -> fanWalker53 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 8; branches to fanWalker7, fanWalker4, fanWalker6.
fanWalker8 :: Core.Term -> Maybe Core.Term
fanWalker8 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker7 fun) (fanWalker7 arg) (\_ -> fanWalker7 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker4 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker6 body) Nothing (\inner -> fanWalker6 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 80; branches to fanWalker79, fanWalker40, fanWalker54.
fanWalker80 :: Core.Term -> Maybe Core.Term
fanWalker80 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker79 fun) (fanWalker79 arg) (\_ -> fanWalker79 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker40 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker54 body) Nothing (\inner -> fanWalker54 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 81; branches to fanWalker80, fanWalker41, fanWalker54.
fanWalker81 :: Core.Term -> Maybe Core.Term
fanWalker81 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker80 fun) (fanWalker80 arg) (\_ -> fanWalker80 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker41 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker54 body) Nothing (\inner -> fanWalker54 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 82; branches to fanWalker81, fanWalker41, fanWalker55.
fanWalker82 :: Core.Term -> Maybe Core.Term
fanWalker82 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker81 fun) (fanWalker81 arg) (\_ -> fanWalker81 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker41 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker55 body) Nothing (\inner -> fanWalker55 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 83; branches to fanWalker82, fanWalker42, fanWalker56.
fanWalker83 :: Core.Term -> Maybe Core.Term
fanWalker83 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker82 fun) (fanWalker82 arg) (\_ -> fanWalker82 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker42 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker56 body) Nothing (\inner -> fanWalker56 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 84; branches to fanWalker83, fanWalker42, fanWalker56.
fanWalker84 :: Core.Term -> Maybe Core.Term
fanWalker84 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker83 fun) (fanWalker83 arg) (\_ -> fanWalker83 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker42 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker56 body) Nothing (\inner -> fanWalker56 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 85; branches to fanWalker84, fanWalker43, fanWalker57.
fanWalker85 :: Core.Term -> Maybe Core.Term
fanWalker85 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker84 fun) (fanWalker84 arg) (\_ -> fanWalker84 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker43 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker57 body) Nothing (\inner -> fanWalker57 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 86; branches to fanWalker85, fanWalker43, fanWalker58.
fanWalker86 :: Core.Term -> Maybe Core.Term
fanWalker86 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker85 fun) (fanWalker85 arg) (\_ -> fanWalker85 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker43 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker58 body) Nothing (\inner -> fanWalker58 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 87; branches to fanWalker86, fanWalker44, fanWalker58.
fanWalker87 :: Core.Term -> Maybe Core.Term
fanWalker87 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker86 fun) (fanWalker86 arg) (\_ -> fanWalker86 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker44 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker58 body) Nothing (\inner -> fanWalker58 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 88; branches to fanWalker87, fanWalker44, fanWalker59.
fanWalker88 :: Core.Term -> Maybe Core.Term
fanWalker88 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker87 fun) (fanWalker87 arg) (\_ -> fanWalker87 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker44 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker59 body) Nothing (\inner -> fanWalker59 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 89; branches to fanWalker88, fanWalker45, fanWalker60.
fanWalker89 :: Core.Term -> Maybe Core.Term
fanWalker89 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker88 fun) (fanWalker88 arg) (\_ -> fanWalker88 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker45 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker60 body) Nothing (\inner -> fanWalker60 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 9; branches to fanWalker8, fanWalker5, fanWalker6.
fanWalker9 :: Core.Term -> Maybe Core.Term
fanWalker9 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker8 fun) (fanWalker8 arg) (\_ -> fanWalker8 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker5 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker6 body) Nothing (\inner -> fanWalker6 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 90; branches to fanWalker89, fanWalker45, fanWalker60.
fanWalker90 :: Core.Term -> Maybe Core.Term
fanWalker90 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker89 fun) (fanWalker89 arg) (\_ -> fanWalker89 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker45 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker60 body) Nothing (\inner -> fanWalker60 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 91; branches to fanWalker90, fanWalker46, fanWalker61.
fanWalker91 :: Core.Term -> Maybe Core.Term
fanWalker91 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker90 fun) (fanWalker90 arg) (\_ -> fanWalker90 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker46 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker61 body) Nothing (\inner -> fanWalker61 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 92; branches to fanWalker91, fanWalker46, fanWalker62.
fanWalker92 :: Core.Term -> Maybe Core.Term
fanWalker92 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker91 fun) (fanWalker91 arg) (\_ -> fanWalker91 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker46 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker62 body) Nothing (\inner -> fanWalker62 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 93; branches to fanWalker92, fanWalker47, fanWalker62.
fanWalker93 :: Core.Term -> Maybe Core.Term
fanWalker93 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker92 fun) (fanWalker92 arg) (\_ -> fanWalker92 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker47 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker62 body) Nothing (\inner -> fanWalker62 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 94; branches to fanWalker93, fanWalker47, fanWalker63.
fanWalker94 :: Core.Term -> Maybe Core.Term
fanWalker94 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker93 fun) (fanWalker93 arg) (\_ -> fanWalker93 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker47 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker63 body) Nothing (\inner -> fanWalker63 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 95; branches to fanWalker94, fanWalker48, fanWalker64.
fanWalker95 :: Core.Term -> Maybe Core.Term
fanWalker95 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker94 fun) (fanWalker94 arg) (\_ -> fanWalker94 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker48 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker64 body) Nothing (\inner -> fanWalker64 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 96; branches to fanWalker95, fanWalker48, fanWalker64.
fanWalker96 :: Core.Term -> Maybe Core.Term
fanWalker96 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker95 fun) (fanWalker95 arg) (\_ -> fanWalker95 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker48 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker64 body) Nothing (\inner -> fanWalker64 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 97; branches to fanWalker96, fanWalker49, fanWalker65.
fanWalker97 :: Core.Term -> Maybe Core.Term
fanWalker97 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker96 fun) (fanWalker96 arg) (\_ -> fanWalker96 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker49 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker65 body) Nothing (\inner -> fanWalker65 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 98; branches to fanWalker97, fanWalker49, fanWalker66.
fanWalker98 :: Core.Term -> Maybe Core.Term
fanWalker98 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker97 fun) (fanWalker97 arg) (\_ -> fanWalker97 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker49 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker66 body) Nothing (\inner -> fanWalker66 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
-- | Fan-out walker level 99; branches to fanWalker98, fanWalker50, fanWalker66.
fanWalker99 :: Core.Term -> Maybe Core.Term
fanWalker99 t =

      let stripped = Strip.deannotateTerm t
      in case stripped of
        Core.TermApplication v0 ->
          let fun = Core.applicationFunction v0
              arg = Core.applicationArgument v0
          in (Optionals.cases (fanWalker98 fun) (fanWalker98 arg) (\_ -> fanWalker98 fun))
        Core.TermLambda v0 ->
          let body = Core.lambdaBody v0
          in (fanWalker50 body)
        Core.TermLet v0 ->
          let body = Core.letBody v0
          in (Optionals.cases (fanWalker66 body) Nothing (\inner -> fanWalker66 inner))
        Core.TermVariable _ -> Just stripped
        Core.TermLiteral _ -> Just stripped
        _ -> Just stripped
