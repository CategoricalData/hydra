-- Note: this is an automatically generated file. Do not edit.

-- | Term decoders for hydra.tabular

module Hydra.Decode.Tabular where

import qualified Hydra.Core as Core
import qualified Hydra.Extract.Helpers as Helpers
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

dataRow :: ((Graph.Graph -> Core.Term -> Either Util.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Util.DecodingError (Tabular.DataRow t0))
dataRow v cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermWrap v1 -> (Eithers.map (\b -> Tabular.DataRow b) (Helpers.decodeList (Helpers.decodeMaybe v) cx (Core.wrappedTermBody v1)))
  _ -> (Left (Util.DecodingError "expected wrapped type hydra.tabular.DataRow"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

headerRow :: (Graph.Graph -> Core.Term -> Either Util.DecodingError Tabular.HeaderRow)
headerRow cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermWrap v1 -> (Eithers.map (\b -> Tabular.HeaderRow b) (Helpers.decodeList (\cx -> \raw -> Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
    Core.TermLiteral v2 -> ((\x -> case x of
      Core.LiteralString v3 -> (Right v3)
      _ -> (Left (Util.DecodingError "expected string literal"))) v2)
    _ -> (Left (Util.DecodingError "expected literal"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw)) cx (Core.wrappedTermBody v1)))
  _ -> (Left (Util.DecodingError "expected wrapped type hydra.tabular.HeaderRow"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))

table :: ((Graph.Graph -> Core.Term -> Either Util.DecodingError t0) -> Graph.Graph -> Core.Term -> Either Util.DecodingError (Tabular.Table t0))
table v cx raw = (Eithers.either (\err -> Left (Util.DecodingError err)) (\stripped -> (\x -> case x of
  Core.TermRecord v1 ->  
    let fieldMap = (Helpers.toFieldMap v1)
    in (Eithers.bind (Helpers.requireField "header" (Helpers.decodeMaybe headerRow) fieldMap cx) (\field_header -> Eithers.bind (Helpers.requireField "data" (Helpers.decodeList (dataRow v)) fieldMap cx) (\field_data -> Right (Tabular.Table {
      Tabular.tableHeader = field_header,
      Tabular.tableData = field_data}))))
  _ -> (Left (Util.DecodingError "expected record of type hydra.tabular.Table"))) stripped) (Lexical.stripAndDereferenceTermEither cx raw))
