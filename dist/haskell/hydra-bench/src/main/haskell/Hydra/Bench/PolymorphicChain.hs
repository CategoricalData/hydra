-- Note: this is an automatically generated file. Do not edit.
-- | Polymorphic-chain inference benchmark. polyWalker_K :: Maybe a -> Maybe a; chains via Maybes.bind, instantiating forall a at each cross-def call site.

module Hydra.Bench.PolymorphicChain where
import qualified Hydra.Lib.Maybes as Maybes
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Polymorphic walker level 0; recurses to polyWalker0.
polyWalker0 :: t0 -> t0
polyWalker0 m = m
-- | Polymorphic walker level 1; recurses to polyWalker0.
polyWalker1 :: Maybe t0 -> Maybe t0
polyWalker1 m =

      let stepped = Maybes.bind m (\v -> polyWalker0 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 10; recurses to polyWalker9.
polyWalker10 :: Maybe t0 -> Maybe t0
polyWalker10 m =

      let stepped = Maybes.bind m (\v -> polyWalker9 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 100; recurses to polyWalker99.
polyWalker100 :: Maybe t0 -> Maybe t0
polyWalker100 m =

      let stepped = Maybes.bind m (\v -> polyWalker99 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 101; recurses to polyWalker100.
polyWalker101 :: Maybe t0 -> Maybe t0
polyWalker101 m =

      let stepped = Maybes.bind m (\v -> polyWalker100 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 102; recurses to polyWalker101.
polyWalker102 :: Maybe t0 -> Maybe t0
polyWalker102 m =

      let stepped = Maybes.bind m (\v -> polyWalker101 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 103; recurses to polyWalker102.
polyWalker103 :: Maybe t0 -> Maybe t0
polyWalker103 m =

      let stepped = Maybes.bind m (\v -> polyWalker102 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 104; recurses to polyWalker103.
polyWalker104 :: Maybe t0 -> Maybe t0
polyWalker104 m =

      let stepped = Maybes.bind m (\v -> polyWalker103 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 105; recurses to polyWalker104.
polyWalker105 :: Maybe t0 -> Maybe t0
polyWalker105 m =

      let stepped = Maybes.bind m (\v -> polyWalker104 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 106; recurses to polyWalker105.
polyWalker106 :: Maybe t0 -> Maybe t0
polyWalker106 m =

      let stepped = Maybes.bind m (\v -> polyWalker105 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 107; recurses to polyWalker106.
polyWalker107 :: Maybe t0 -> Maybe t0
polyWalker107 m =

      let stepped = Maybes.bind m (\v -> polyWalker106 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 108; recurses to polyWalker107.
polyWalker108 :: Maybe t0 -> Maybe t0
polyWalker108 m =

      let stepped = Maybes.bind m (\v -> polyWalker107 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 109; recurses to polyWalker108.
polyWalker109 :: Maybe t0 -> Maybe t0
polyWalker109 m =

      let stepped = Maybes.bind m (\v -> polyWalker108 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 11; recurses to polyWalker10.
polyWalker11 :: Maybe t0 -> Maybe t0
polyWalker11 m =

      let stepped = Maybes.bind m (\v -> polyWalker10 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 110; recurses to polyWalker109.
polyWalker110 :: Maybe t0 -> Maybe t0
polyWalker110 m =

      let stepped = Maybes.bind m (\v -> polyWalker109 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 111; recurses to polyWalker110.
polyWalker111 :: Maybe t0 -> Maybe t0
polyWalker111 m =

      let stepped = Maybes.bind m (\v -> polyWalker110 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 112; recurses to polyWalker111.
polyWalker112 :: Maybe t0 -> Maybe t0
polyWalker112 m =

      let stepped = Maybes.bind m (\v -> polyWalker111 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 113; recurses to polyWalker112.
polyWalker113 :: Maybe t0 -> Maybe t0
polyWalker113 m =

      let stepped = Maybes.bind m (\v -> polyWalker112 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 114; recurses to polyWalker113.
polyWalker114 :: Maybe t0 -> Maybe t0
polyWalker114 m =

      let stepped = Maybes.bind m (\v -> polyWalker113 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 115; recurses to polyWalker114.
polyWalker115 :: Maybe t0 -> Maybe t0
polyWalker115 m =

      let stepped = Maybes.bind m (\v -> polyWalker114 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 116; recurses to polyWalker115.
polyWalker116 :: Maybe t0 -> Maybe t0
polyWalker116 m =

      let stepped = Maybes.bind m (\v -> polyWalker115 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 117; recurses to polyWalker116.
polyWalker117 :: Maybe t0 -> Maybe t0
polyWalker117 m =

      let stepped = Maybes.bind m (\v -> polyWalker116 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 118; recurses to polyWalker117.
polyWalker118 :: Maybe t0 -> Maybe t0
polyWalker118 m =

      let stepped = Maybes.bind m (\v -> polyWalker117 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 119; recurses to polyWalker118.
polyWalker119 :: Maybe t0 -> Maybe t0
polyWalker119 m =

      let stepped = Maybes.bind m (\v -> polyWalker118 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 12; recurses to polyWalker11.
polyWalker12 :: Maybe t0 -> Maybe t0
polyWalker12 m =

      let stepped = Maybes.bind m (\v -> polyWalker11 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 120; recurses to polyWalker119.
polyWalker120 :: Maybe t0 -> Maybe t0
polyWalker120 m =

      let stepped = Maybes.bind m (\v -> polyWalker119 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 121; recurses to polyWalker120.
polyWalker121 :: Maybe t0 -> Maybe t0
polyWalker121 m =

      let stepped = Maybes.bind m (\v -> polyWalker120 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 122; recurses to polyWalker121.
polyWalker122 :: Maybe t0 -> Maybe t0
polyWalker122 m =

      let stepped = Maybes.bind m (\v -> polyWalker121 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 123; recurses to polyWalker122.
polyWalker123 :: Maybe t0 -> Maybe t0
polyWalker123 m =

      let stepped = Maybes.bind m (\v -> polyWalker122 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 124; recurses to polyWalker123.
polyWalker124 :: Maybe t0 -> Maybe t0
polyWalker124 m =

      let stepped = Maybes.bind m (\v -> polyWalker123 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 125; recurses to polyWalker124.
polyWalker125 :: Maybe t0 -> Maybe t0
polyWalker125 m =

      let stepped = Maybes.bind m (\v -> polyWalker124 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 126; recurses to polyWalker125.
polyWalker126 :: Maybe t0 -> Maybe t0
polyWalker126 m =

      let stepped = Maybes.bind m (\v -> polyWalker125 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 127; recurses to polyWalker126.
polyWalker127 :: Maybe t0 -> Maybe t0
polyWalker127 m =

      let stepped = Maybes.bind m (\v -> polyWalker126 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 128; recurses to polyWalker127.
polyWalker128 :: Maybe t0 -> Maybe t0
polyWalker128 m =

      let stepped = Maybes.bind m (\v -> polyWalker127 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 129; recurses to polyWalker128.
polyWalker129 :: Maybe t0 -> Maybe t0
polyWalker129 m =

      let stepped = Maybes.bind m (\v -> polyWalker128 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 13; recurses to polyWalker12.
polyWalker13 :: Maybe t0 -> Maybe t0
polyWalker13 m =

      let stepped = Maybes.bind m (\v -> polyWalker12 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 130; recurses to polyWalker129.
polyWalker130 :: Maybe t0 -> Maybe t0
polyWalker130 m =

      let stepped = Maybes.bind m (\v -> polyWalker129 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 131; recurses to polyWalker130.
polyWalker131 :: Maybe t0 -> Maybe t0
polyWalker131 m =

      let stepped = Maybes.bind m (\v -> polyWalker130 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 132; recurses to polyWalker131.
polyWalker132 :: Maybe t0 -> Maybe t0
polyWalker132 m =

      let stepped = Maybes.bind m (\v -> polyWalker131 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 133; recurses to polyWalker132.
polyWalker133 :: Maybe t0 -> Maybe t0
polyWalker133 m =

      let stepped = Maybes.bind m (\v -> polyWalker132 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 134; recurses to polyWalker133.
polyWalker134 :: Maybe t0 -> Maybe t0
polyWalker134 m =

      let stepped = Maybes.bind m (\v -> polyWalker133 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 135; recurses to polyWalker134.
polyWalker135 :: Maybe t0 -> Maybe t0
polyWalker135 m =

      let stepped = Maybes.bind m (\v -> polyWalker134 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 136; recurses to polyWalker135.
polyWalker136 :: Maybe t0 -> Maybe t0
polyWalker136 m =

      let stepped = Maybes.bind m (\v -> polyWalker135 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 137; recurses to polyWalker136.
polyWalker137 :: Maybe t0 -> Maybe t0
polyWalker137 m =

      let stepped = Maybes.bind m (\v -> polyWalker136 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 138; recurses to polyWalker137.
polyWalker138 :: Maybe t0 -> Maybe t0
polyWalker138 m =

      let stepped = Maybes.bind m (\v -> polyWalker137 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 139; recurses to polyWalker138.
polyWalker139 :: Maybe t0 -> Maybe t0
polyWalker139 m =

      let stepped = Maybes.bind m (\v -> polyWalker138 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 14; recurses to polyWalker13.
polyWalker14 :: Maybe t0 -> Maybe t0
polyWalker14 m =

      let stepped = Maybes.bind m (\v -> polyWalker13 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 140; recurses to polyWalker139.
polyWalker140 :: Maybe t0 -> Maybe t0
polyWalker140 m =

      let stepped = Maybes.bind m (\v -> polyWalker139 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 141; recurses to polyWalker140.
polyWalker141 :: Maybe t0 -> Maybe t0
polyWalker141 m =

      let stepped = Maybes.bind m (\v -> polyWalker140 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 142; recurses to polyWalker141.
polyWalker142 :: Maybe t0 -> Maybe t0
polyWalker142 m =

      let stepped = Maybes.bind m (\v -> polyWalker141 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 143; recurses to polyWalker142.
polyWalker143 :: Maybe t0 -> Maybe t0
polyWalker143 m =

      let stepped = Maybes.bind m (\v -> polyWalker142 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 144; recurses to polyWalker143.
polyWalker144 :: Maybe t0 -> Maybe t0
polyWalker144 m =

      let stepped = Maybes.bind m (\v -> polyWalker143 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 145; recurses to polyWalker144.
polyWalker145 :: Maybe t0 -> Maybe t0
polyWalker145 m =

      let stepped = Maybes.bind m (\v -> polyWalker144 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 146; recurses to polyWalker145.
polyWalker146 :: Maybe t0 -> Maybe t0
polyWalker146 m =

      let stepped = Maybes.bind m (\v -> polyWalker145 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 147; recurses to polyWalker146.
polyWalker147 :: Maybe t0 -> Maybe t0
polyWalker147 m =

      let stepped = Maybes.bind m (\v -> polyWalker146 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 148; recurses to polyWalker147.
polyWalker148 :: Maybe t0 -> Maybe t0
polyWalker148 m =

      let stepped = Maybes.bind m (\v -> polyWalker147 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 149; recurses to polyWalker148.
polyWalker149 :: Maybe t0 -> Maybe t0
polyWalker149 m =

      let stepped = Maybes.bind m (\v -> polyWalker148 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 15; recurses to polyWalker14.
polyWalker15 :: Maybe t0 -> Maybe t0
polyWalker15 m =

      let stepped = Maybes.bind m (\v -> polyWalker14 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 150; recurses to polyWalker149.
polyWalker150 :: Maybe t0 -> Maybe t0
polyWalker150 m =

      let stepped = Maybes.bind m (\v -> polyWalker149 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 151; recurses to polyWalker150.
polyWalker151 :: Maybe t0 -> Maybe t0
polyWalker151 m =

      let stepped = Maybes.bind m (\v -> polyWalker150 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 152; recurses to polyWalker151.
polyWalker152 :: Maybe t0 -> Maybe t0
polyWalker152 m =

      let stepped = Maybes.bind m (\v -> polyWalker151 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 153; recurses to polyWalker152.
polyWalker153 :: Maybe t0 -> Maybe t0
polyWalker153 m =

      let stepped = Maybes.bind m (\v -> polyWalker152 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 154; recurses to polyWalker153.
polyWalker154 :: Maybe t0 -> Maybe t0
polyWalker154 m =

      let stepped = Maybes.bind m (\v -> polyWalker153 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 155; recurses to polyWalker154.
polyWalker155 :: Maybe t0 -> Maybe t0
polyWalker155 m =

      let stepped = Maybes.bind m (\v -> polyWalker154 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 156; recurses to polyWalker155.
polyWalker156 :: Maybe t0 -> Maybe t0
polyWalker156 m =

      let stepped = Maybes.bind m (\v -> polyWalker155 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 157; recurses to polyWalker156.
polyWalker157 :: Maybe t0 -> Maybe t0
polyWalker157 m =

      let stepped = Maybes.bind m (\v -> polyWalker156 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 158; recurses to polyWalker157.
polyWalker158 :: Maybe t0 -> Maybe t0
polyWalker158 m =

      let stepped = Maybes.bind m (\v -> polyWalker157 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 159; recurses to polyWalker158.
polyWalker159 :: Maybe t0 -> Maybe t0
polyWalker159 m =

      let stepped = Maybes.bind m (\v -> polyWalker158 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 16; recurses to polyWalker15.
polyWalker16 :: Maybe t0 -> Maybe t0
polyWalker16 m =

      let stepped = Maybes.bind m (\v -> polyWalker15 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 160; recurses to polyWalker159.
polyWalker160 :: Maybe t0 -> Maybe t0
polyWalker160 m =

      let stepped = Maybes.bind m (\v -> polyWalker159 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 161; recurses to polyWalker160.
polyWalker161 :: Maybe t0 -> Maybe t0
polyWalker161 m =

      let stepped = Maybes.bind m (\v -> polyWalker160 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 162; recurses to polyWalker161.
polyWalker162 :: Maybe t0 -> Maybe t0
polyWalker162 m =

      let stepped = Maybes.bind m (\v -> polyWalker161 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 163; recurses to polyWalker162.
polyWalker163 :: Maybe t0 -> Maybe t0
polyWalker163 m =

      let stepped = Maybes.bind m (\v -> polyWalker162 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 164; recurses to polyWalker163.
polyWalker164 :: Maybe t0 -> Maybe t0
polyWalker164 m =

      let stepped = Maybes.bind m (\v -> polyWalker163 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 165; recurses to polyWalker164.
polyWalker165 :: Maybe t0 -> Maybe t0
polyWalker165 m =

      let stepped = Maybes.bind m (\v -> polyWalker164 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 166; recurses to polyWalker165.
polyWalker166 :: Maybe t0 -> Maybe t0
polyWalker166 m =

      let stepped = Maybes.bind m (\v -> polyWalker165 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 167; recurses to polyWalker166.
polyWalker167 :: Maybe t0 -> Maybe t0
polyWalker167 m =

      let stepped = Maybes.bind m (\v -> polyWalker166 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 168; recurses to polyWalker167.
polyWalker168 :: Maybe t0 -> Maybe t0
polyWalker168 m =

      let stepped = Maybes.bind m (\v -> polyWalker167 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 169; recurses to polyWalker168.
polyWalker169 :: Maybe t0 -> Maybe t0
polyWalker169 m =

      let stepped = Maybes.bind m (\v -> polyWalker168 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 17; recurses to polyWalker16.
polyWalker17 :: Maybe t0 -> Maybe t0
polyWalker17 m =

      let stepped = Maybes.bind m (\v -> polyWalker16 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 170; recurses to polyWalker169.
polyWalker170 :: Maybe t0 -> Maybe t0
polyWalker170 m =

      let stepped = Maybes.bind m (\v -> polyWalker169 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 171; recurses to polyWalker170.
polyWalker171 :: Maybe t0 -> Maybe t0
polyWalker171 m =

      let stepped = Maybes.bind m (\v -> polyWalker170 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 172; recurses to polyWalker171.
polyWalker172 :: Maybe t0 -> Maybe t0
polyWalker172 m =

      let stepped = Maybes.bind m (\v -> polyWalker171 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 173; recurses to polyWalker172.
polyWalker173 :: Maybe t0 -> Maybe t0
polyWalker173 m =

      let stepped = Maybes.bind m (\v -> polyWalker172 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 174; recurses to polyWalker173.
polyWalker174 :: Maybe t0 -> Maybe t0
polyWalker174 m =

      let stepped = Maybes.bind m (\v -> polyWalker173 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 175; recurses to polyWalker174.
polyWalker175 :: Maybe t0 -> Maybe t0
polyWalker175 m =

      let stepped = Maybes.bind m (\v -> polyWalker174 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 176; recurses to polyWalker175.
polyWalker176 :: Maybe t0 -> Maybe t0
polyWalker176 m =

      let stepped = Maybes.bind m (\v -> polyWalker175 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 177; recurses to polyWalker176.
polyWalker177 :: Maybe t0 -> Maybe t0
polyWalker177 m =

      let stepped = Maybes.bind m (\v -> polyWalker176 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 178; recurses to polyWalker177.
polyWalker178 :: Maybe t0 -> Maybe t0
polyWalker178 m =

      let stepped = Maybes.bind m (\v -> polyWalker177 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 179; recurses to polyWalker178.
polyWalker179 :: Maybe t0 -> Maybe t0
polyWalker179 m =

      let stepped = Maybes.bind m (\v -> polyWalker178 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 18; recurses to polyWalker17.
polyWalker18 :: Maybe t0 -> Maybe t0
polyWalker18 m =

      let stepped = Maybes.bind m (\v -> polyWalker17 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 180; recurses to polyWalker179.
polyWalker180 :: Maybe t0 -> Maybe t0
polyWalker180 m =

      let stepped = Maybes.bind m (\v -> polyWalker179 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 181; recurses to polyWalker180.
polyWalker181 :: Maybe t0 -> Maybe t0
polyWalker181 m =

      let stepped = Maybes.bind m (\v -> polyWalker180 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 182; recurses to polyWalker181.
polyWalker182 :: Maybe t0 -> Maybe t0
polyWalker182 m =

      let stepped = Maybes.bind m (\v -> polyWalker181 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 183; recurses to polyWalker182.
polyWalker183 :: Maybe t0 -> Maybe t0
polyWalker183 m =

      let stepped = Maybes.bind m (\v -> polyWalker182 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 184; recurses to polyWalker183.
polyWalker184 :: Maybe t0 -> Maybe t0
polyWalker184 m =

      let stepped = Maybes.bind m (\v -> polyWalker183 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 185; recurses to polyWalker184.
polyWalker185 :: Maybe t0 -> Maybe t0
polyWalker185 m =

      let stepped = Maybes.bind m (\v -> polyWalker184 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 186; recurses to polyWalker185.
polyWalker186 :: Maybe t0 -> Maybe t0
polyWalker186 m =

      let stepped = Maybes.bind m (\v -> polyWalker185 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 187; recurses to polyWalker186.
polyWalker187 :: Maybe t0 -> Maybe t0
polyWalker187 m =

      let stepped = Maybes.bind m (\v -> polyWalker186 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 188; recurses to polyWalker187.
polyWalker188 :: Maybe t0 -> Maybe t0
polyWalker188 m =

      let stepped = Maybes.bind m (\v -> polyWalker187 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 189; recurses to polyWalker188.
polyWalker189 :: Maybe t0 -> Maybe t0
polyWalker189 m =

      let stepped = Maybes.bind m (\v -> polyWalker188 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 19; recurses to polyWalker18.
polyWalker19 :: Maybe t0 -> Maybe t0
polyWalker19 m =

      let stepped = Maybes.bind m (\v -> polyWalker18 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 190; recurses to polyWalker189.
polyWalker190 :: Maybe t0 -> Maybe t0
polyWalker190 m =

      let stepped = Maybes.bind m (\v -> polyWalker189 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 191; recurses to polyWalker190.
polyWalker191 :: Maybe t0 -> Maybe t0
polyWalker191 m =

      let stepped = Maybes.bind m (\v -> polyWalker190 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 192; recurses to polyWalker191.
polyWalker192 :: Maybe t0 -> Maybe t0
polyWalker192 m =

      let stepped = Maybes.bind m (\v -> polyWalker191 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 193; recurses to polyWalker192.
polyWalker193 :: Maybe t0 -> Maybe t0
polyWalker193 m =

      let stepped = Maybes.bind m (\v -> polyWalker192 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 194; recurses to polyWalker193.
polyWalker194 :: Maybe t0 -> Maybe t0
polyWalker194 m =

      let stepped = Maybes.bind m (\v -> polyWalker193 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 195; recurses to polyWalker194.
polyWalker195 :: Maybe t0 -> Maybe t0
polyWalker195 m =

      let stepped = Maybes.bind m (\v -> polyWalker194 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 196; recurses to polyWalker195.
polyWalker196 :: Maybe t0 -> Maybe t0
polyWalker196 m =

      let stepped = Maybes.bind m (\v -> polyWalker195 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 197; recurses to polyWalker196.
polyWalker197 :: Maybe t0 -> Maybe t0
polyWalker197 m =

      let stepped = Maybes.bind m (\v -> polyWalker196 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 198; recurses to polyWalker197.
polyWalker198 :: Maybe t0 -> Maybe t0
polyWalker198 m =

      let stepped = Maybes.bind m (\v -> polyWalker197 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 199; recurses to polyWalker198.
polyWalker199 :: Maybe t0 -> Maybe t0
polyWalker199 m =

      let stepped = Maybes.bind m (\v -> polyWalker198 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 2; recurses to polyWalker1.
polyWalker2 :: Maybe t0 -> Maybe t0
polyWalker2 m =

      let stepped = Maybes.bind m (\v -> polyWalker1 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 20; recurses to polyWalker19.
polyWalker20 :: Maybe t0 -> Maybe t0
polyWalker20 m =

      let stepped = Maybes.bind m (\v -> polyWalker19 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 200; recurses to polyWalker199.
polyWalker200 :: Maybe t0 -> Maybe t0
polyWalker200 m =

      let stepped = Maybes.bind m (\v -> polyWalker199 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 201; recurses to polyWalker200.
polyWalker201 :: Maybe t0 -> Maybe t0
polyWalker201 m =

      let stepped = Maybes.bind m (\v -> polyWalker200 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 202; recurses to polyWalker201.
polyWalker202 :: Maybe t0 -> Maybe t0
polyWalker202 m =

      let stepped = Maybes.bind m (\v -> polyWalker201 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 203; recurses to polyWalker202.
polyWalker203 :: Maybe t0 -> Maybe t0
polyWalker203 m =

      let stepped = Maybes.bind m (\v -> polyWalker202 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 204; recurses to polyWalker203.
polyWalker204 :: Maybe t0 -> Maybe t0
polyWalker204 m =

      let stepped = Maybes.bind m (\v -> polyWalker203 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 205; recurses to polyWalker204.
polyWalker205 :: Maybe t0 -> Maybe t0
polyWalker205 m =

      let stepped = Maybes.bind m (\v -> polyWalker204 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 206; recurses to polyWalker205.
polyWalker206 :: Maybe t0 -> Maybe t0
polyWalker206 m =

      let stepped = Maybes.bind m (\v -> polyWalker205 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 207; recurses to polyWalker206.
polyWalker207 :: Maybe t0 -> Maybe t0
polyWalker207 m =

      let stepped = Maybes.bind m (\v -> polyWalker206 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 208; recurses to polyWalker207.
polyWalker208 :: Maybe t0 -> Maybe t0
polyWalker208 m =

      let stepped = Maybes.bind m (\v -> polyWalker207 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 209; recurses to polyWalker208.
polyWalker209 :: Maybe t0 -> Maybe t0
polyWalker209 m =

      let stepped = Maybes.bind m (\v -> polyWalker208 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 21; recurses to polyWalker20.
polyWalker21 :: Maybe t0 -> Maybe t0
polyWalker21 m =

      let stepped = Maybes.bind m (\v -> polyWalker20 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 210; recurses to polyWalker209.
polyWalker210 :: Maybe t0 -> Maybe t0
polyWalker210 m =

      let stepped = Maybes.bind m (\v -> polyWalker209 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 211; recurses to polyWalker210.
polyWalker211 :: Maybe t0 -> Maybe t0
polyWalker211 m =

      let stepped = Maybes.bind m (\v -> polyWalker210 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 212; recurses to polyWalker211.
polyWalker212 :: Maybe t0 -> Maybe t0
polyWalker212 m =

      let stepped = Maybes.bind m (\v -> polyWalker211 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 213; recurses to polyWalker212.
polyWalker213 :: Maybe t0 -> Maybe t0
polyWalker213 m =

      let stepped = Maybes.bind m (\v -> polyWalker212 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 214; recurses to polyWalker213.
polyWalker214 :: Maybe t0 -> Maybe t0
polyWalker214 m =

      let stepped = Maybes.bind m (\v -> polyWalker213 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 215; recurses to polyWalker214.
polyWalker215 :: Maybe t0 -> Maybe t0
polyWalker215 m =

      let stepped = Maybes.bind m (\v -> polyWalker214 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 216; recurses to polyWalker215.
polyWalker216 :: Maybe t0 -> Maybe t0
polyWalker216 m =

      let stepped = Maybes.bind m (\v -> polyWalker215 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 217; recurses to polyWalker216.
polyWalker217 :: Maybe t0 -> Maybe t0
polyWalker217 m =

      let stepped = Maybes.bind m (\v -> polyWalker216 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 218; recurses to polyWalker217.
polyWalker218 :: Maybe t0 -> Maybe t0
polyWalker218 m =

      let stepped = Maybes.bind m (\v -> polyWalker217 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 219; recurses to polyWalker218.
polyWalker219 :: Maybe t0 -> Maybe t0
polyWalker219 m =

      let stepped = Maybes.bind m (\v -> polyWalker218 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 22; recurses to polyWalker21.
polyWalker22 :: Maybe t0 -> Maybe t0
polyWalker22 m =

      let stepped = Maybes.bind m (\v -> polyWalker21 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 220; recurses to polyWalker219.
polyWalker220 :: Maybe t0 -> Maybe t0
polyWalker220 m =

      let stepped = Maybes.bind m (\v -> polyWalker219 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 221; recurses to polyWalker220.
polyWalker221 :: Maybe t0 -> Maybe t0
polyWalker221 m =

      let stepped = Maybes.bind m (\v -> polyWalker220 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 222; recurses to polyWalker221.
polyWalker222 :: Maybe t0 -> Maybe t0
polyWalker222 m =

      let stepped = Maybes.bind m (\v -> polyWalker221 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 223; recurses to polyWalker222.
polyWalker223 :: Maybe t0 -> Maybe t0
polyWalker223 m =

      let stepped = Maybes.bind m (\v -> polyWalker222 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 224; recurses to polyWalker223.
polyWalker224 :: Maybe t0 -> Maybe t0
polyWalker224 m =

      let stepped = Maybes.bind m (\v -> polyWalker223 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 225; recurses to polyWalker224.
polyWalker225 :: Maybe t0 -> Maybe t0
polyWalker225 m =

      let stepped = Maybes.bind m (\v -> polyWalker224 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 226; recurses to polyWalker225.
polyWalker226 :: Maybe t0 -> Maybe t0
polyWalker226 m =

      let stepped = Maybes.bind m (\v -> polyWalker225 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 227; recurses to polyWalker226.
polyWalker227 :: Maybe t0 -> Maybe t0
polyWalker227 m =

      let stepped = Maybes.bind m (\v -> polyWalker226 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 228; recurses to polyWalker227.
polyWalker228 :: Maybe t0 -> Maybe t0
polyWalker228 m =

      let stepped = Maybes.bind m (\v -> polyWalker227 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 229; recurses to polyWalker228.
polyWalker229 :: Maybe t0 -> Maybe t0
polyWalker229 m =

      let stepped = Maybes.bind m (\v -> polyWalker228 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 23; recurses to polyWalker22.
polyWalker23 :: Maybe t0 -> Maybe t0
polyWalker23 m =

      let stepped = Maybes.bind m (\v -> polyWalker22 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 230; recurses to polyWalker229.
polyWalker230 :: Maybe t0 -> Maybe t0
polyWalker230 m =

      let stepped = Maybes.bind m (\v -> polyWalker229 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 231; recurses to polyWalker230.
polyWalker231 :: Maybe t0 -> Maybe t0
polyWalker231 m =

      let stepped = Maybes.bind m (\v -> polyWalker230 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 232; recurses to polyWalker231.
polyWalker232 :: Maybe t0 -> Maybe t0
polyWalker232 m =

      let stepped = Maybes.bind m (\v -> polyWalker231 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 233; recurses to polyWalker232.
polyWalker233 :: Maybe t0 -> Maybe t0
polyWalker233 m =

      let stepped = Maybes.bind m (\v -> polyWalker232 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 234; recurses to polyWalker233.
polyWalker234 :: Maybe t0 -> Maybe t0
polyWalker234 m =

      let stepped = Maybes.bind m (\v -> polyWalker233 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 235; recurses to polyWalker234.
polyWalker235 :: Maybe t0 -> Maybe t0
polyWalker235 m =

      let stepped = Maybes.bind m (\v -> polyWalker234 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 236; recurses to polyWalker235.
polyWalker236 :: Maybe t0 -> Maybe t0
polyWalker236 m =

      let stepped = Maybes.bind m (\v -> polyWalker235 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 237; recurses to polyWalker236.
polyWalker237 :: Maybe t0 -> Maybe t0
polyWalker237 m =

      let stepped = Maybes.bind m (\v -> polyWalker236 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 238; recurses to polyWalker237.
polyWalker238 :: Maybe t0 -> Maybe t0
polyWalker238 m =

      let stepped = Maybes.bind m (\v -> polyWalker237 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 239; recurses to polyWalker238.
polyWalker239 :: Maybe t0 -> Maybe t0
polyWalker239 m =

      let stepped = Maybes.bind m (\v -> polyWalker238 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 24; recurses to polyWalker23.
polyWalker24 :: Maybe t0 -> Maybe t0
polyWalker24 m =

      let stepped = Maybes.bind m (\v -> polyWalker23 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 240; recurses to polyWalker239.
polyWalker240 :: Maybe t0 -> Maybe t0
polyWalker240 m =

      let stepped = Maybes.bind m (\v -> polyWalker239 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 241; recurses to polyWalker240.
polyWalker241 :: Maybe t0 -> Maybe t0
polyWalker241 m =

      let stepped = Maybes.bind m (\v -> polyWalker240 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 242; recurses to polyWalker241.
polyWalker242 :: Maybe t0 -> Maybe t0
polyWalker242 m =

      let stepped = Maybes.bind m (\v -> polyWalker241 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 243; recurses to polyWalker242.
polyWalker243 :: Maybe t0 -> Maybe t0
polyWalker243 m =

      let stepped = Maybes.bind m (\v -> polyWalker242 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 244; recurses to polyWalker243.
polyWalker244 :: Maybe t0 -> Maybe t0
polyWalker244 m =

      let stepped = Maybes.bind m (\v -> polyWalker243 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 245; recurses to polyWalker244.
polyWalker245 :: Maybe t0 -> Maybe t0
polyWalker245 m =

      let stepped = Maybes.bind m (\v -> polyWalker244 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 246; recurses to polyWalker245.
polyWalker246 :: Maybe t0 -> Maybe t0
polyWalker246 m =

      let stepped = Maybes.bind m (\v -> polyWalker245 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 247; recurses to polyWalker246.
polyWalker247 :: Maybe t0 -> Maybe t0
polyWalker247 m =

      let stepped = Maybes.bind m (\v -> polyWalker246 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 248; recurses to polyWalker247.
polyWalker248 :: Maybe t0 -> Maybe t0
polyWalker248 m =

      let stepped = Maybes.bind m (\v -> polyWalker247 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 249; recurses to polyWalker248.
polyWalker249 :: Maybe t0 -> Maybe t0
polyWalker249 m =

      let stepped = Maybes.bind m (\v -> polyWalker248 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 25; recurses to polyWalker24.
polyWalker25 :: Maybe t0 -> Maybe t0
polyWalker25 m =

      let stepped = Maybes.bind m (\v -> polyWalker24 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 250; recurses to polyWalker249.
polyWalker250 :: Maybe t0 -> Maybe t0
polyWalker250 m =

      let stepped = Maybes.bind m (\v -> polyWalker249 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 251; recurses to polyWalker250.
polyWalker251 :: Maybe t0 -> Maybe t0
polyWalker251 m =

      let stepped = Maybes.bind m (\v -> polyWalker250 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 252; recurses to polyWalker251.
polyWalker252 :: Maybe t0 -> Maybe t0
polyWalker252 m =

      let stepped = Maybes.bind m (\v -> polyWalker251 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 253; recurses to polyWalker252.
polyWalker253 :: Maybe t0 -> Maybe t0
polyWalker253 m =

      let stepped = Maybes.bind m (\v -> polyWalker252 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 254; recurses to polyWalker253.
polyWalker254 :: Maybe t0 -> Maybe t0
polyWalker254 m =

      let stepped = Maybes.bind m (\v -> polyWalker253 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 255; recurses to polyWalker254.
polyWalker255 :: Maybe t0 -> Maybe t0
polyWalker255 m =

      let stepped = Maybes.bind m (\v -> polyWalker254 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 256; recurses to polyWalker255.
polyWalker256 :: Maybe t0 -> Maybe t0
polyWalker256 m =

      let stepped = Maybes.bind m (\v -> polyWalker255 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 257; recurses to polyWalker256.
polyWalker257 :: Maybe t0 -> Maybe t0
polyWalker257 m =

      let stepped = Maybes.bind m (\v -> polyWalker256 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 258; recurses to polyWalker257.
polyWalker258 :: Maybe t0 -> Maybe t0
polyWalker258 m =

      let stepped = Maybes.bind m (\v -> polyWalker257 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 259; recurses to polyWalker258.
polyWalker259 :: Maybe t0 -> Maybe t0
polyWalker259 m =

      let stepped = Maybes.bind m (\v -> polyWalker258 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 26; recurses to polyWalker25.
polyWalker26 :: Maybe t0 -> Maybe t0
polyWalker26 m =

      let stepped = Maybes.bind m (\v -> polyWalker25 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 260; recurses to polyWalker259.
polyWalker260 :: Maybe t0 -> Maybe t0
polyWalker260 m =

      let stepped = Maybes.bind m (\v -> polyWalker259 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 261; recurses to polyWalker260.
polyWalker261 :: Maybe t0 -> Maybe t0
polyWalker261 m =

      let stepped = Maybes.bind m (\v -> polyWalker260 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 262; recurses to polyWalker261.
polyWalker262 :: Maybe t0 -> Maybe t0
polyWalker262 m =

      let stepped = Maybes.bind m (\v -> polyWalker261 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 263; recurses to polyWalker262.
polyWalker263 :: Maybe t0 -> Maybe t0
polyWalker263 m =

      let stepped = Maybes.bind m (\v -> polyWalker262 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 264; recurses to polyWalker263.
polyWalker264 :: Maybe t0 -> Maybe t0
polyWalker264 m =

      let stepped = Maybes.bind m (\v -> polyWalker263 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 265; recurses to polyWalker264.
polyWalker265 :: Maybe t0 -> Maybe t0
polyWalker265 m =

      let stepped = Maybes.bind m (\v -> polyWalker264 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 266; recurses to polyWalker265.
polyWalker266 :: Maybe t0 -> Maybe t0
polyWalker266 m =

      let stepped = Maybes.bind m (\v -> polyWalker265 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 267; recurses to polyWalker266.
polyWalker267 :: Maybe t0 -> Maybe t0
polyWalker267 m =

      let stepped = Maybes.bind m (\v -> polyWalker266 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 268; recurses to polyWalker267.
polyWalker268 :: Maybe t0 -> Maybe t0
polyWalker268 m =

      let stepped = Maybes.bind m (\v -> polyWalker267 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 269; recurses to polyWalker268.
polyWalker269 :: Maybe t0 -> Maybe t0
polyWalker269 m =

      let stepped = Maybes.bind m (\v -> polyWalker268 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 27; recurses to polyWalker26.
polyWalker27 :: Maybe t0 -> Maybe t0
polyWalker27 m =

      let stepped = Maybes.bind m (\v -> polyWalker26 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 270; recurses to polyWalker269.
polyWalker270 :: Maybe t0 -> Maybe t0
polyWalker270 m =

      let stepped = Maybes.bind m (\v -> polyWalker269 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 271; recurses to polyWalker270.
polyWalker271 :: Maybe t0 -> Maybe t0
polyWalker271 m =

      let stepped = Maybes.bind m (\v -> polyWalker270 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 272; recurses to polyWalker271.
polyWalker272 :: Maybe t0 -> Maybe t0
polyWalker272 m =

      let stepped = Maybes.bind m (\v -> polyWalker271 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 273; recurses to polyWalker272.
polyWalker273 :: Maybe t0 -> Maybe t0
polyWalker273 m =

      let stepped = Maybes.bind m (\v -> polyWalker272 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 274; recurses to polyWalker273.
polyWalker274 :: Maybe t0 -> Maybe t0
polyWalker274 m =

      let stepped = Maybes.bind m (\v -> polyWalker273 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 275; recurses to polyWalker274.
polyWalker275 :: Maybe t0 -> Maybe t0
polyWalker275 m =

      let stepped = Maybes.bind m (\v -> polyWalker274 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 276; recurses to polyWalker275.
polyWalker276 :: Maybe t0 -> Maybe t0
polyWalker276 m =

      let stepped = Maybes.bind m (\v -> polyWalker275 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 277; recurses to polyWalker276.
polyWalker277 :: Maybe t0 -> Maybe t0
polyWalker277 m =

      let stepped = Maybes.bind m (\v -> polyWalker276 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 278; recurses to polyWalker277.
polyWalker278 :: Maybe t0 -> Maybe t0
polyWalker278 m =

      let stepped = Maybes.bind m (\v -> polyWalker277 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 279; recurses to polyWalker278.
polyWalker279 :: Maybe t0 -> Maybe t0
polyWalker279 m =

      let stepped = Maybes.bind m (\v -> polyWalker278 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 28; recurses to polyWalker27.
polyWalker28 :: Maybe t0 -> Maybe t0
polyWalker28 m =

      let stepped = Maybes.bind m (\v -> polyWalker27 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 280; recurses to polyWalker279.
polyWalker280 :: Maybe t0 -> Maybe t0
polyWalker280 m =

      let stepped = Maybes.bind m (\v -> polyWalker279 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 281; recurses to polyWalker280.
polyWalker281 :: Maybe t0 -> Maybe t0
polyWalker281 m =

      let stepped = Maybes.bind m (\v -> polyWalker280 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 282; recurses to polyWalker281.
polyWalker282 :: Maybe t0 -> Maybe t0
polyWalker282 m =

      let stepped = Maybes.bind m (\v -> polyWalker281 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 283; recurses to polyWalker282.
polyWalker283 :: Maybe t0 -> Maybe t0
polyWalker283 m =

      let stepped = Maybes.bind m (\v -> polyWalker282 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 284; recurses to polyWalker283.
polyWalker284 :: Maybe t0 -> Maybe t0
polyWalker284 m =

      let stepped = Maybes.bind m (\v -> polyWalker283 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 285; recurses to polyWalker284.
polyWalker285 :: Maybe t0 -> Maybe t0
polyWalker285 m =

      let stepped = Maybes.bind m (\v -> polyWalker284 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 286; recurses to polyWalker285.
polyWalker286 :: Maybe t0 -> Maybe t0
polyWalker286 m =

      let stepped = Maybes.bind m (\v -> polyWalker285 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 287; recurses to polyWalker286.
polyWalker287 :: Maybe t0 -> Maybe t0
polyWalker287 m =

      let stepped = Maybes.bind m (\v -> polyWalker286 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 288; recurses to polyWalker287.
polyWalker288 :: Maybe t0 -> Maybe t0
polyWalker288 m =

      let stepped = Maybes.bind m (\v -> polyWalker287 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 289; recurses to polyWalker288.
polyWalker289 :: Maybe t0 -> Maybe t0
polyWalker289 m =

      let stepped = Maybes.bind m (\v -> polyWalker288 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 29; recurses to polyWalker28.
polyWalker29 :: Maybe t0 -> Maybe t0
polyWalker29 m =

      let stepped = Maybes.bind m (\v -> polyWalker28 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 290; recurses to polyWalker289.
polyWalker290 :: Maybe t0 -> Maybe t0
polyWalker290 m =

      let stepped = Maybes.bind m (\v -> polyWalker289 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 291; recurses to polyWalker290.
polyWalker291 :: Maybe t0 -> Maybe t0
polyWalker291 m =

      let stepped = Maybes.bind m (\v -> polyWalker290 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 292; recurses to polyWalker291.
polyWalker292 :: Maybe t0 -> Maybe t0
polyWalker292 m =

      let stepped = Maybes.bind m (\v -> polyWalker291 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 293; recurses to polyWalker292.
polyWalker293 :: Maybe t0 -> Maybe t0
polyWalker293 m =

      let stepped = Maybes.bind m (\v -> polyWalker292 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 294; recurses to polyWalker293.
polyWalker294 :: Maybe t0 -> Maybe t0
polyWalker294 m =

      let stepped = Maybes.bind m (\v -> polyWalker293 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 295; recurses to polyWalker294.
polyWalker295 :: Maybe t0 -> Maybe t0
polyWalker295 m =

      let stepped = Maybes.bind m (\v -> polyWalker294 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 296; recurses to polyWalker295.
polyWalker296 :: Maybe t0 -> Maybe t0
polyWalker296 m =

      let stepped = Maybes.bind m (\v -> polyWalker295 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 297; recurses to polyWalker296.
polyWalker297 :: Maybe t0 -> Maybe t0
polyWalker297 m =

      let stepped = Maybes.bind m (\v -> polyWalker296 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 298; recurses to polyWalker297.
polyWalker298 :: Maybe t0 -> Maybe t0
polyWalker298 m =

      let stepped = Maybes.bind m (\v -> polyWalker297 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 299; recurses to polyWalker298.
polyWalker299 :: Maybe t0 -> Maybe t0
polyWalker299 m =

      let stepped = Maybes.bind m (\v -> polyWalker298 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 3; recurses to polyWalker2.
polyWalker3 :: Maybe t0 -> Maybe t0
polyWalker3 m =

      let stepped = Maybes.bind m (\v -> polyWalker2 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 30; recurses to polyWalker29.
polyWalker30 :: Maybe t0 -> Maybe t0
polyWalker30 m =

      let stepped = Maybes.bind m (\v -> polyWalker29 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 300; recurses to polyWalker299.
polyWalker300 :: Maybe t0 -> Maybe t0
polyWalker300 m =

      let stepped = Maybes.bind m (\v -> polyWalker299 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 301; recurses to polyWalker300.
polyWalker301 :: Maybe t0 -> Maybe t0
polyWalker301 m =

      let stepped = Maybes.bind m (\v -> polyWalker300 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 302; recurses to polyWalker301.
polyWalker302 :: Maybe t0 -> Maybe t0
polyWalker302 m =

      let stepped = Maybes.bind m (\v -> polyWalker301 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 303; recurses to polyWalker302.
polyWalker303 :: Maybe t0 -> Maybe t0
polyWalker303 m =

      let stepped = Maybes.bind m (\v -> polyWalker302 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 304; recurses to polyWalker303.
polyWalker304 :: Maybe t0 -> Maybe t0
polyWalker304 m =

      let stepped = Maybes.bind m (\v -> polyWalker303 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 305; recurses to polyWalker304.
polyWalker305 :: Maybe t0 -> Maybe t0
polyWalker305 m =

      let stepped = Maybes.bind m (\v -> polyWalker304 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 306; recurses to polyWalker305.
polyWalker306 :: Maybe t0 -> Maybe t0
polyWalker306 m =

      let stepped = Maybes.bind m (\v -> polyWalker305 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 307; recurses to polyWalker306.
polyWalker307 :: Maybe t0 -> Maybe t0
polyWalker307 m =

      let stepped = Maybes.bind m (\v -> polyWalker306 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 308; recurses to polyWalker307.
polyWalker308 :: Maybe t0 -> Maybe t0
polyWalker308 m =

      let stepped = Maybes.bind m (\v -> polyWalker307 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 309; recurses to polyWalker308.
polyWalker309 :: Maybe t0 -> Maybe t0
polyWalker309 m =

      let stepped = Maybes.bind m (\v -> polyWalker308 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 31; recurses to polyWalker30.
polyWalker31 :: Maybe t0 -> Maybe t0
polyWalker31 m =

      let stepped = Maybes.bind m (\v -> polyWalker30 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 310; recurses to polyWalker309.
polyWalker310 :: Maybe t0 -> Maybe t0
polyWalker310 m =

      let stepped = Maybes.bind m (\v -> polyWalker309 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 311; recurses to polyWalker310.
polyWalker311 :: Maybe t0 -> Maybe t0
polyWalker311 m =

      let stepped = Maybes.bind m (\v -> polyWalker310 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 312; recurses to polyWalker311.
polyWalker312 :: Maybe t0 -> Maybe t0
polyWalker312 m =

      let stepped = Maybes.bind m (\v -> polyWalker311 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 313; recurses to polyWalker312.
polyWalker313 :: Maybe t0 -> Maybe t0
polyWalker313 m =

      let stepped = Maybes.bind m (\v -> polyWalker312 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 314; recurses to polyWalker313.
polyWalker314 :: Maybe t0 -> Maybe t0
polyWalker314 m =

      let stepped = Maybes.bind m (\v -> polyWalker313 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 315; recurses to polyWalker314.
polyWalker315 :: Maybe t0 -> Maybe t0
polyWalker315 m =

      let stepped = Maybes.bind m (\v -> polyWalker314 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 316; recurses to polyWalker315.
polyWalker316 :: Maybe t0 -> Maybe t0
polyWalker316 m =

      let stepped = Maybes.bind m (\v -> polyWalker315 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 317; recurses to polyWalker316.
polyWalker317 :: Maybe t0 -> Maybe t0
polyWalker317 m =

      let stepped = Maybes.bind m (\v -> polyWalker316 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 318; recurses to polyWalker317.
polyWalker318 :: Maybe t0 -> Maybe t0
polyWalker318 m =

      let stepped = Maybes.bind m (\v -> polyWalker317 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 319; recurses to polyWalker318.
polyWalker319 :: Maybe t0 -> Maybe t0
polyWalker319 m =

      let stepped = Maybes.bind m (\v -> polyWalker318 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 32; recurses to polyWalker31.
polyWalker32 :: Maybe t0 -> Maybe t0
polyWalker32 m =

      let stepped = Maybes.bind m (\v -> polyWalker31 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 320; recurses to polyWalker319.
polyWalker320 :: Maybe t0 -> Maybe t0
polyWalker320 m =

      let stepped = Maybes.bind m (\v -> polyWalker319 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 321; recurses to polyWalker320.
polyWalker321 :: Maybe t0 -> Maybe t0
polyWalker321 m =

      let stepped = Maybes.bind m (\v -> polyWalker320 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 322; recurses to polyWalker321.
polyWalker322 :: Maybe t0 -> Maybe t0
polyWalker322 m =

      let stepped = Maybes.bind m (\v -> polyWalker321 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 323; recurses to polyWalker322.
polyWalker323 :: Maybe t0 -> Maybe t0
polyWalker323 m =

      let stepped = Maybes.bind m (\v -> polyWalker322 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 324; recurses to polyWalker323.
polyWalker324 :: Maybe t0 -> Maybe t0
polyWalker324 m =

      let stepped = Maybes.bind m (\v -> polyWalker323 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 325; recurses to polyWalker324.
polyWalker325 :: Maybe t0 -> Maybe t0
polyWalker325 m =

      let stepped = Maybes.bind m (\v -> polyWalker324 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 326; recurses to polyWalker325.
polyWalker326 :: Maybe t0 -> Maybe t0
polyWalker326 m =

      let stepped = Maybes.bind m (\v -> polyWalker325 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 327; recurses to polyWalker326.
polyWalker327 :: Maybe t0 -> Maybe t0
polyWalker327 m =

      let stepped = Maybes.bind m (\v -> polyWalker326 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 328; recurses to polyWalker327.
polyWalker328 :: Maybe t0 -> Maybe t0
polyWalker328 m =

      let stepped = Maybes.bind m (\v -> polyWalker327 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 329; recurses to polyWalker328.
polyWalker329 :: Maybe t0 -> Maybe t0
polyWalker329 m =

      let stepped = Maybes.bind m (\v -> polyWalker328 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 33; recurses to polyWalker32.
polyWalker33 :: Maybe t0 -> Maybe t0
polyWalker33 m =

      let stepped = Maybes.bind m (\v -> polyWalker32 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 330; recurses to polyWalker329.
polyWalker330 :: Maybe t0 -> Maybe t0
polyWalker330 m =

      let stepped = Maybes.bind m (\v -> polyWalker329 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 331; recurses to polyWalker330.
polyWalker331 :: Maybe t0 -> Maybe t0
polyWalker331 m =

      let stepped = Maybes.bind m (\v -> polyWalker330 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 332; recurses to polyWalker331.
polyWalker332 :: Maybe t0 -> Maybe t0
polyWalker332 m =

      let stepped = Maybes.bind m (\v -> polyWalker331 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 333; recurses to polyWalker332.
polyWalker333 :: Maybe t0 -> Maybe t0
polyWalker333 m =

      let stepped = Maybes.bind m (\v -> polyWalker332 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 334; recurses to polyWalker333.
polyWalker334 :: Maybe t0 -> Maybe t0
polyWalker334 m =

      let stepped = Maybes.bind m (\v -> polyWalker333 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 335; recurses to polyWalker334.
polyWalker335 :: Maybe t0 -> Maybe t0
polyWalker335 m =

      let stepped = Maybes.bind m (\v -> polyWalker334 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 336; recurses to polyWalker335.
polyWalker336 :: Maybe t0 -> Maybe t0
polyWalker336 m =

      let stepped = Maybes.bind m (\v -> polyWalker335 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 337; recurses to polyWalker336.
polyWalker337 :: Maybe t0 -> Maybe t0
polyWalker337 m =

      let stepped = Maybes.bind m (\v -> polyWalker336 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 338; recurses to polyWalker337.
polyWalker338 :: Maybe t0 -> Maybe t0
polyWalker338 m =

      let stepped = Maybes.bind m (\v -> polyWalker337 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 339; recurses to polyWalker338.
polyWalker339 :: Maybe t0 -> Maybe t0
polyWalker339 m =

      let stepped = Maybes.bind m (\v -> polyWalker338 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 34; recurses to polyWalker33.
polyWalker34 :: Maybe t0 -> Maybe t0
polyWalker34 m =

      let stepped = Maybes.bind m (\v -> polyWalker33 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 340; recurses to polyWalker339.
polyWalker340 :: Maybe t0 -> Maybe t0
polyWalker340 m =

      let stepped = Maybes.bind m (\v -> polyWalker339 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 341; recurses to polyWalker340.
polyWalker341 :: Maybe t0 -> Maybe t0
polyWalker341 m =

      let stepped = Maybes.bind m (\v -> polyWalker340 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 342; recurses to polyWalker341.
polyWalker342 :: Maybe t0 -> Maybe t0
polyWalker342 m =

      let stepped = Maybes.bind m (\v -> polyWalker341 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 343; recurses to polyWalker342.
polyWalker343 :: Maybe t0 -> Maybe t0
polyWalker343 m =

      let stepped = Maybes.bind m (\v -> polyWalker342 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 344; recurses to polyWalker343.
polyWalker344 :: Maybe t0 -> Maybe t0
polyWalker344 m =

      let stepped = Maybes.bind m (\v -> polyWalker343 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 345; recurses to polyWalker344.
polyWalker345 :: Maybe t0 -> Maybe t0
polyWalker345 m =

      let stepped = Maybes.bind m (\v -> polyWalker344 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 346; recurses to polyWalker345.
polyWalker346 :: Maybe t0 -> Maybe t0
polyWalker346 m =

      let stepped = Maybes.bind m (\v -> polyWalker345 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 347; recurses to polyWalker346.
polyWalker347 :: Maybe t0 -> Maybe t0
polyWalker347 m =

      let stepped = Maybes.bind m (\v -> polyWalker346 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 348; recurses to polyWalker347.
polyWalker348 :: Maybe t0 -> Maybe t0
polyWalker348 m =

      let stepped = Maybes.bind m (\v -> polyWalker347 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 349; recurses to polyWalker348.
polyWalker349 :: Maybe t0 -> Maybe t0
polyWalker349 m =

      let stepped = Maybes.bind m (\v -> polyWalker348 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 35; recurses to polyWalker34.
polyWalker35 :: Maybe t0 -> Maybe t0
polyWalker35 m =

      let stepped = Maybes.bind m (\v -> polyWalker34 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 350; recurses to polyWalker349.
polyWalker350 :: Maybe t0 -> Maybe t0
polyWalker350 m =

      let stepped = Maybes.bind m (\v -> polyWalker349 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 351; recurses to polyWalker350.
polyWalker351 :: Maybe t0 -> Maybe t0
polyWalker351 m =

      let stepped = Maybes.bind m (\v -> polyWalker350 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 352; recurses to polyWalker351.
polyWalker352 :: Maybe t0 -> Maybe t0
polyWalker352 m =

      let stepped = Maybes.bind m (\v -> polyWalker351 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 353; recurses to polyWalker352.
polyWalker353 :: Maybe t0 -> Maybe t0
polyWalker353 m =

      let stepped = Maybes.bind m (\v -> polyWalker352 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 354; recurses to polyWalker353.
polyWalker354 :: Maybe t0 -> Maybe t0
polyWalker354 m =

      let stepped = Maybes.bind m (\v -> polyWalker353 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 355; recurses to polyWalker354.
polyWalker355 :: Maybe t0 -> Maybe t0
polyWalker355 m =

      let stepped = Maybes.bind m (\v -> polyWalker354 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 356; recurses to polyWalker355.
polyWalker356 :: Maybe t0 -> Maybe t0
polyWalker356 m =

      let stepped = Maybes.bind m (\v -> polyWalker355 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 357; recurses to polyWalker356.
polyWalker357 :: Maybe t0 -> Maybe t0
polyWalker357 m =

      let stepped = Maybes.bind m (\v -> polyWalker356 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 358; recurses to polyWalker357.
polyWalker358 :: Maybe t0 -> Maybe t0
polyWalker358 m =

      let stepped = Maybes.bind m (\v -> polyWalker357 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 359; recurses to polyWalker358.
polyWalker359 :: Maybe t0 -> Maybe t0
polyWalker359 m =

      let stepped = Maybes.bind m (\v -> polyWalker358 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 36; recurses to polyWalker35.
polyWalker36 :: Maybe t0 -> Maybe t0
polyWalker36 m =

      let stepped = Maybes.bind m (\v -> polyWalker35 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 360; recurses to polyWalker359.
polyWalker360 :: Maybe t0 -> Maybe t0
polyWalker360 m =

      let stepped = Maybes.bind m (\v -> polyWalker359 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 361; recurses to polyWalker360.
polyWalker361 :: Maybe t0 -> Maybe t0
polyWalker361 m =

      let stepped = Maybes.bind m (\v -> polyWalker360 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 362; recurses to polyWalker361.
polyWalker362 :: Maybe t0 -> Maybe t0
polyWalker362 m =

      let stepped = Maybes.bind m (\v -> polyWalker361 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 363; recurses to polyWalker362.
polyWalker363 :: Maybe t0 -> Maybe t0
polyWalker363 m =

      let stepped = Maybes.bind m (\v -> polyWalker362 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 364; recurses to polyWalker363.
polyWalker364 :: Maybe t0 -> Maybe t0
polyWalker364 m =

      let stepped = Maybes.bind m (\v -> polyWalker363 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 365; recurses to polyWalker364.
polyWalker365 :: Maybe t0 -> Maybe t0
polyWalker365 m =

      let stepped = Maybes.bind m (\v -> polyWalker364 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 366; recurses to polyWalker365.
polyWalker366 :: Maybe t0 -> Maybe t0
polyWalker366 m =

      let stepped = Maybes.bind m (\v -> polyWalker365 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 367; recurses to polyWalker366.
polyWalker367 :: Maybe t0 -> Maybe t0
polyWalker367 m =

      let stepped = Maybes.bind m (\v -> polyWalker366 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 368; recurses to polyWalker367.
polyWalker368 :: Maybe t0 -> Maybe t0
polyWalker368 m =

      let stepped = Maybes.bind m (\v -> polyWalker367 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 369; recurses to polyWalker368.
polyWalker369 :: Maybe t0 -> Maybe t0
polyWalker369 m =

      let stepped = Maybes.bind m (\v -> polyWalker368 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 37; recurses to polyWalker36.
polyWalker37 :: Maybe t0 -> Maybe t0
polyWalker37 m =

      let stepped = Maybes.bind m (\v -> polyWalker36 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 370; recurses to polyWalker369.
polyWalker370 :: Maybe t0 -> Maybe t0
polyWalker370 m =

      let stepped = Maybes.bind m (\v -> polyWalker369 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 371; recurses to polyWalker370.
polyWalker371 :: Maybe t0 -> Maybe t0
polyWalker371 m =

      let stepped = Maybes.bind m (\v -> polyWalker370 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 372; recurses to polyWalker371.
polyWalker372 :: Maybe t0 -> Maybe t0
polyWalker372 m =

      let stepped = Maybes.bind m (\v -> polyWalker371 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 373; recurses to polyWalker372.
polyWalker373 :: Maybe t0 -> Maybe t0
polyWalker373 m =

      let stepped = Maybes.bind m (\v -> polyWalker372 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 374; recurses to polyWalker373.
polyWalker374 :: Maybe t0 -> Maybe t0
polyWalker374 m =

      let stepped = Maybes.bind m (\v -> polyWalker373 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 375; recurses to polyWalker374.
polyWalker375 :: Maybe t0 -> Maybe t0
polyWalker375 m =

      let stepped = Maybes.bind m (\v -> polyWalker374 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 376; recurses to polyWalker375.
polyWalker376 :: Maybe t0 -> Maybe t0
polyWalker376 m =

      let stepped = Maybes.bind m (\v -> polyWalker375 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 377; recurses to polyWalker376.
polyWalker377 :: Maybe t0 -> Maybe t0
polyWalker377 m =

      let stepped = Maybes.bind m (\v -> polyWalker376 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 378; recurses to polyWalker377.
polyWalker378 :: Maybe t0 -> Maybe t0
polyWalker378 m =

      let stepped = Maybes.bind m (\v -> polyWalker377 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 379; recurses to polyWalker378.
polyWalker379 :: Maybe t0 -> Maybe t0
polyWalker379 m =

      let stepped = Maybes.bind m (\v -> polyWalker378 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 38; recurses to polyWalker37.
polyWalker38 :: Maybe t0 -> Maybe t0
polyWalker38 m =

      let stepped = Maybes.bind m (\v -> polyWalker37 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 380; recurses to polyWalker379.
polyWalker380 :: Maybe t0 -> Maybe t0
polyWalker380 m =

      let stepped = Maybes.bind m (\v -> polyWalker379 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 381; recurses to polyWalker380.
polyWalker381 :: Maybe t0 -> Maybe t0
polyWalker381 m =

      let stepped = Maybes.bind m (\v -> polyWalker380 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 382; recurses to polyWalker381.
polyWalker382 :: Maybe t0 -> Maybe t0
polyWalker382 m =

      let stepped = Maybes.bind m (\v -> polyWalker381 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 383; recurses to polyWalker382.
polyWalker383 :: Maybe t0 -> Maybe t0
polyWalker383 m =

      let stepped = Maybes.bind m (\v -> polyWalker382 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 384; recurses to polyWalker383.
polyWalker384 :: Maybe t0 -> Maybe t0
polyWalker384 m =

      let stepped = Maybes.bind m (\v -> polyWalker383 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 385; recurses to polyWalker384.
polyWalker385 :: Maybe t0 -> Maybe t0
polyWalker385 m =

      let stepped = Maybes.bind m (\v -> polyWalker384 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 386; recurses to polyWalker385.
polyWalker386 :: Maybe t0 -> Maybe t0
polyWalker386 m =

      let stepped = Maybes.bind m (\v -> polyWalker385 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 387; recurses to polyWalker386.
polyWalker387 :: Maybe t0 -> Maybe t0
polyWalker387 m =

      let stepped = Maybes.bind m (\v -> polyWalker386 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 388; recurses to polyWalker387.
polyWalker388 :: Maybe t0 -> Maybe t0
polyWalker388 m =

      let stepped = Maybes.bind m (\v -> polyWalker387 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 389; recurses to polyWalker388.
polyWalker389 :: Maybe t0 -> Maybe t0
polyWalker389 m =

      let stepped = Maybes.bind m (\v -> polyWalker388 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 39; recurses to polyWalker38.
polyWalker39 :: Maybe t0 -> Maybe t0
polyWalker39 m =

      let stepped = Maybes.bind m (\v -> polyWalker38 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 390; recurses to polyWalker389.
polyWalker390 :: Maybe t0 -> Maybe t0
polyWalker390 m =

      let stepped = Maybes.bind m (\v -> polyWalker389 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 391; recurses to polyWalker390.
polyWalker391 :: Maybe t0 -> Maybe t0
polyWalker391 m =

      let stepped = Maybes.bind m (\v -> polyWalker390 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 392; recurses to polyWalker391.
polyWalker392 :: Maybe t0 -> Maybe t0
polyWalker392 m =

      let stepped = Maybes.bind m (\v -> polyWalker391 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 393; recurses to polyWalker392.
polyWalker393 :: Maybe t0 -> Maybe t0
polyWalker393 m =

      let stepped = Maybes.bind m (\v -> polyWalker392 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 394; recurses to polyWalker393.
polyWalker394 :: Maybe t0 -> Maybe t0
polyWalker394 m =

      let stepped = Maybes.bind m (\v -> polyWalker393 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 395; recurses to polyWalker394.
polyWalker395 :: Maybe t0 -> Maybe t0
polyWalker395 m =

      let stepped = Maybes.bind m (\v -> polyWalker394 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 396; recurses to polyWalker395.
polyWalker396 :: Maybe t0 -> Maybe t0
polyWalker396 m =

      let stepped = Maybes.bind m (\v -> polyWalker395 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 397; recurses to polyWalker396.
polyWalker397 :: Maybe t0 -> Maybe t0
polyWalker397 m =

      let stepped = Maybes.bind m (\v -> polyWalker396 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 398; recurses to polyWalker397.
polyWalker398 :: Maybe t0 -> Maybe t0
polyWalker398 m =

      let stepped = Maybes.bind m (\v -> polyWalker397 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 399; recurses to polyWalker398.
polyWalker399 :: Maybe t0 -> Maybe t0
polyWalker399 m =

      let stepped = Maybes.bind m (\v -> polyWalker398 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 4; recurses to polyWalker3.
polyWalker4 :: Maybe t0 -> Maybe t0
polyWalker4 m =

      let stepped = Maybes.bind m (\v -> polyWalker3 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 40; recurses to polyWalker39.
polyWalker40 :: Maybe t0 -> Maybe t0
polyWalker40 m =

      let stepped = Maybes.bind m (\v -> polyWalker39 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 41; recurses to polyWalker40.
polyWalker41 :: Maybe t0 -> Maybe t0
polyWalker41 m =

      let stepped = Maybes.bind m (\v -> polyWalker40 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 42; recurses to polyWalker41.
polyWalker42 :: Maybe t0 -> Maybe t0
polyWalker42 m =

      let stepped = Maybes.bind m (\v -> polyWalker41 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 43; recurses to polyWalker42.
polyWalker43 :: Maybe t0 -> Maybe t0
polyWalker43 m =

      let stepped = Maybes.bind m (\v -> polyWalker42 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 44; recurses to polyWalker43.
polyWalker44 :: Maybe t0 -> Maybe t0
polyWalker44 m =

      let stepped = Maybes.bind m (\v -> polyWalker43 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 45; recurses to polyWalker44.
polyWalker45 :: Maybe t0 -> Maybe t0
polyWalker45 m =

      let stepped = Maybes.bind m (\v -> polyWalker44 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 46; recurses to polyWalker45.
polyWalker46 :: Maybe t0 -> Maybe t0
polyWalker46 m =

      let stepped = Maybes.bind m (\v -> polyWalker45 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 47; recurses to polyWalker46.
polyWalker47 :: Maybe t0 -> Maybe t0
polyWalker47 m =

      let stepped = Maybes.bind m (\v -> polyWalker46 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 48; recurses to polyWalker47.
polyWalker48 :: Maybe t0 -> Maybe t0
polyWalker48 m =

      let stepped = Maybes.bind m (\v -> polyWalker47 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 49; recurses to polyWalker48.
polyWalker49 :: Maybe t0 -> Maybe t0
polyWalker49 m =

      let stepped = Maybes.bind m (\v -> polyWalker48 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 5; recurses to polyWalker4.
polyWalker5 :: Maybe t0 -> Maybe t0
polyWalker5 m =

      let stepped = Maybes.bind m (\v -> polyWalker4 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 50; recurses to polyWalker49.
polyWalker50 :: Maybe t0 -> Maybe t0
polyWalker50 m =

      let stepped = Maybes.bind m (\v -> polyWalker49 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 51; recurses to polyWalker50.
polyWalker51 :: Maybe t0 -> Maybe t0
polyWalker51 m =

      let stepped = Maybes.bind m (\v -> polyWalker50 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 52; recurses to polyWalker51.
polyWalker52 :: Maybe t0 -> Maybe t0
polyWalker52 m =

      let stepped = Maybes.bind m (\v -> polyWalker51 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 53; recurses to polyWalker52.
polyWalker53 :: Maybe t0 -> Maybe t0
polyWalker53 m =

      let stepped = Maybes.bind m (\v -> polyWalker52 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 54; recurses to polyWalker53.
polyWalker54 :: Maybe t0 -> Maybe t0
polyWalker54 m =

      let stepped = Maybes.bind m (\v -> polyWalker53 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 55; recurses to polyWalker54.
polyWalker55 :: Maybe t0 -> Maybe t0
polyWalker55 m =

      let stepped = Maybes.bind m (\v -> polyWalker54 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 56; recurses to polyWalker55.
polyWalker56 :: Maybe t0 -> Maybe t0
polyWalker56 m =

      let stepped = Maybes.bind m (\v -> polyWalker55 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 57; recurses to polyWalker56.
polyWalker57 :: Maybe t0 -> Maybe t0
polyWalker57 m =

      let stepped = Maybes.bind m (\v -> polyWalker56 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 58; recurses to polyWalker57.
polyWalker58 :: Maybe t0 -> Maybe t0
polyWalker58 m =

      let stepped = Maybes.bind m (\v -> polyWalker57 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 59; recurses to polyWalker58.
polyWalker59 :: Maybe t0 -> Maybe t0
polyWalker59 m =

      let stepped = Maybes.bind m (\v -> polyWalker58 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 6; recurses to polyWalker5.
polyWalker6 :: Maybe t0 -> Maybe t0
polyWalker6 m =

      let stepped = Maybes.bind m (\v -> polyWalker5 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 60; recurses to polyWalker59.
polyWalker60 :: Maybe t0 -> Maybe t0
polyWalker60 m =

      let stepped = Maybes.bind m (\v -> polyWalker59 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 61; recurses to polyWalker60.
polyWalker61 :: Maybe t0 -> Maybe t0
polyWalker61 m =

      let stepped = Maybes.bind m (\v -> polyWalker60 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 62; recurses to polyWalker61.
polyWalker62 :: Maybe t0 -> Maybe t0
polyWalker62 m =

      let stepped = Maybes.bind m (\v -> polyWalker61 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 63; recurses to polyWalker62.
polyWalker63 :: Maybe t0 -> Maybe t0
polyWalker63 m =

      let stepped = Maybes.bind m (\v -> polyWalker62 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 64; recurses to polyWalker63.
polyWalker64 :: Maybe t0 -> Maybe t0
polyWalker64 m =

      let stepped = Maybes.bind m (\v -> polyWalker63 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 65; recurses to polyWalker64.
polyWalker65 :: Maybe t0 -> Maybe t0
polyWalker65 m =

      let stepped = Maybes.bind m (\v -> polyWalker64 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 66; recurses to polyWalker65.
polyWalker66 :: Maybe t0 -> Maybe t0
polyWalker66 m =

      let stepped = Maybes.bind m (\v -> polyWalker65 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 67; recurses to polyWalker66.
polyWalker67 :: Maybe t0 -> Maybe t0
polyWalker67 m =

      let stepped = Maybes.bind m (\v -> polyWalker66 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 68; recurses to polyWalker67.
polyWalker68 :: Maybe t0 -> Maybe t0
polyWalker68 m =

      let stepped = Maybes.bind m (\v -> polyWalker67 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 69; recurses to polyWalker68.
polyWalker69 :: Maybe t0 -> Maybe t0
polyWalker69 m =

      let stepped = Maybes.bind m (\v -> polyWalker68 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 7; recurses to polyWalker6.
polyWalker7 :: Maybe t0 -> Maybe t0
polyWalker7 m =

      let stepped = Maybes.bind m (\v -> polyWalker6 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 70; recurses to polyWalker69.
polyWalker70 :: Maybe t0 -> Maybe t0
polyWalker70 m =

      let stepped = Maybes.bind m (\v -> polyWalker69 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 71; recurses to polyWalker70.
polyWalker71 :: Maybe t0 -> Maybe t0
polyWalker71 m =

      let stepped = Maybes.bind m (\v -> polyWalker70 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 72; recurses to polyWalker71.
polyWalker72 :: Maybe t0 -> Maybe t0
polyWalker72 m =

      let stepped = Maybes.bind m (\v -> polyWalker71 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 73; recurses to polyWalker72.
polyWalker73 :: Maybe t0 -> Maybe t0
polyWalker73 m =

      let stepped = Maybes.bind m (\v -> polyWalker72 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 74; recurses to polyWalker73.
polyWalker74 :: Maybe t0 -> Maybe t0
polyWalker74 m =

      let stepped = Maybes.bind m (\v -> polyWalker73 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 75; recurses to polyWalker74.
polyWalker75 :: Maybe t0 -> Maybe t0
polyWalker75 m =

      let stepped = Maybes.bind m (\v -> polyWalker74 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 76; recurses to polyWalker75.
polyWalker76 :: Maybe t0 -> Maybe t0
polyWalker76 m =

      let stepped = Maybes.bind m (\v -> polyWalker75 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 77; recurses to polyWalker76.
polyWalker77 :: Maybe t0 -> Maybe t0
polyWalker77 m =

      let stepped = Maybes.bind m (\v -> polyWalker76 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 78; recurses to polyWalker77.
polyWalker78 :: Maybe t0 -> Maybe t0
polyWalker78 m =

      let stepped = Maybes.bind m (\v -> polyWalker77 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 79; recurses to polyWalker78.
polyWalker79 :: Maybe t0 -> Maybe t0
polyWalker79 m =

      let stepped = Maybes.bind m (\v -> polyWalker78 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 8; recurses to polyWalker7.
polyWalker8 :: Maybe t0 -> Maybe t0
polyWalker8 m =

      let stepped = Maybes.bind m (\v -> polyWalker7 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 80; recurses to polyWalker79.
polyWalker80 :: Maybe t0 -> Maybe t0
polyWalker80 m =

      let stepped = Maybes.bind m (\v -> polyWalker79 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 81; recurses to polyWalker80.
polyWalker81 :: Maybe t0 -> Maybe t0
polyWalker81 m =

      let stepped = Maybes.bind m (\v -> polyWalker80 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 82; recurses to polyWalker81.
polyWalker82 :: Maybe t0 -> Maybe t0
polyWalker82 m =

      let stepped = Maybes.bind m (\v -> polyWalker81 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 83; recurses to polyWalker82.
polyWalker83 :: Maybe t0 -> Maybe t0
polyWalker83 m =

      let stepped = Maybes.bind m (\v -> polyWalker82 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 84; recurses to polyWalker83.
polyWalker84 :: Maybe t0 -> Maybe t0
polyWalker84 m =

      let stepped = Maybes.bind m (\v -> polyWalker83 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 85; recurses to polyWalker84.
polyWalker85 :: Maybe t0 -> Maybe t0
polyWalker85 m =

      let stepped = Maybes.bind m (\v -> polyWalker84 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 86; recurses to polyWalker85.
polyWalker86 :: Maybe t0 -> Maybe t0
polyWalker86 m =

      let stepped = Maybes.bind m (\v -> polyWalker85 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 87; recurses to polyWalker86.
polyWalker87 :: Maybe t0 -> Maybe t0
polyWalker87 m =

      let stepped = Maybes.bind m (\v -> polyWalker86 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 88; recurses to polyWalker87.
polyWalker88 :: Maybe t0 -> Maybe t0
polyWalker88 m =

      let stepped = Maybes.bind m (\v -> polyWalker87 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 89; recurses to polyWalker88.
polyWalker89 :: Maybe t0 -> Maybe t0
polyWalker89 m =

      let stepped = Maybes.bind m (\v -> polyWalker88 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 9; recurses to polyWalker8.
polyWalker9 :: Maybe t0 -> Maybe t0
polyWalker9 m =

      let stepped = Maybes.bind m (\v -> polyWalker8 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 90; recurses to polyWalker89.
polyWalker90 :: Maybe t0 -> Maybe t0
polyWalker90 m =

      let stepped = Maybes.bind m (\v -> polyWalker89 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 91; recurses to polyWalker90.
polyWalker91 :: Maybe t0 -> Maybe t0
polyWalker91 m =

      let stepped = Maybes.bind m (\v -> polyWalker90 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 92; recurses to polyWalker91.
polyWalker92 :: Maybe t0 -> Maybe t0
polyWalker92 m =

      let stepped = Maybes.bind m (\v -> polyWalker91 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 93; recurses to polyWalker92.
polyWalker93 :: Maybe t0 -> Maybe t0
polyWalker93 m =

      let stepped = Maybes.bind m (\v -> polyWalker92 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 94; recurses to polyWalker93.
polyWalker94 :: Maybe t0 -> Maybe t0
polyWalker94 m =

      let stepped = Maybes.bind m (\v -> polyWalker93 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 95; recurses to polyWalker94.
polyWalker95 :: Maybe t0 -> Maybe t0
polyWalker95 m =

      let stepped = Maybes.bind m (\v -> polyWalker94 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 96; recurses to polyWalker95.
polyWalker96 :: Maybe t0 -> Maybe t0
polyWalker96 m =

      let stepped = Maybes.bind m (\v -> polyWalker95 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 97; recurses to polyWalker96.
polyWalker97 :: Maybe t0 -> Maybe t0
polyWalker97 m =

      let stepped = Maybes.bind m (\v -> polyWalker96 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 98; recurses to polyWalker97.
polyWalker98 :: Maybe t0 -> Maybe t0
polyWalker98 m =

      let stepped = Maybes.bind m (\v -> polyWalker97 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
-- | Polymorphic walker level 99; recurses to polyWalker98.
polyWalker99 :: Maybe t0 -> Maybe t0
polyWalker99 m =

      let stepped = Maybes.bind m (\v -> polyWalker98 (Just v))
      in (Maybes.maybe Nothing (\w -> Just w) stepped)
