-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.stac.items

module Hydra.Dsl.Stac.Items where

import qualified Hydra.Core as Core
import qualified Hydra.Geojson.Model as Model
import qualified Hydra.Iana.Linkrelations as Linkrelations
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Stac.Items as Items
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Map as M

asset :: Phantoms.TTerm Items.Uri -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm (Maybe Items.MediaType) -> Phantoms.TTerm [Items.Role] -> Phantoms.TTerm Items.Asset
asset href title description type_ roles =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.stac.items.Asset"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "href"),
          Core.fieldTerm = (Phantoms.unTTerm href)},
        Core.Field {
          Core.fieldName = (Core.Name "title"),
          Core.fieldTerm = (Phantoms.unTTerm title)},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm description)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "roles"),
          Core.fieldTerm = (Phantoms.unTTerm roles)}]}))

assetDescription :: Phantoms.TTerm Items.Asset -> Phantoms.TTerm (Maybe String)
assetDescription x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.stac.items.Asset"),
        Core.projectionField = (Core.Name "description")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

assetHref :: Phantoms.TTerm Items.Asset -> Phantoms.TTerm Items.Uri
assetHref x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.stac.items.Asset"),
        Core.projectionField = (Core.Name "href")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

assetRoles :: Phantoms.TTerm Items.Asset -> Phantoms.TTerm [Items.Role]
assetRoles x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.stac.items.Asset"),
        Core.projectionField = (Core.Name "roles")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

assetTitle :: Phantoms.TTerm Items.Asset -> Phantoms.TTerm (Maybe String)
assetTitle x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.stac.items.Asset"),
        Core.projectionField = (Core.Name "title")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

assetType :: Phantoms.TTerm Items.Asset -> Phantoms.TTerm (Maybe Items.MediaType)
assetType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.stac.items.Asset"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

assetWithDescription :: Phantoms.TTerm Items.Asset -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Items.Asset
assetWithDescription original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.stac.items.Asset"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "href"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Asset"),
              Core.projectionField = (Core.Name "href")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "title"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Asset"),
              Core.projectionField = (Core.Name "title")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Asset"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "roles"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Asset"),
              Core.projectionField = (Core.Name "roles")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

assetWithHref :: Phantoms.TTerm Items.Asset -> Phantoms.TTerm Items.Uri -> Phantoms.TTerm Items.Asset
assetWithHref original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.stac.items.Asset"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "href"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "title"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Asset"),
              Core.projectionField = (Core.Name "title")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Asset"),
              Core.projectionField = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Asset"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "roles"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Asset"),
              Core.projectionField = (Core.Name "roles")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

assetWithRoles :: Phantoms.TTerm Items.Asset -> Phantoms.TTerm [Items.Role] -> Phantoms.TTerm Items.Asset
assetWithRoles original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.stac.items.Asset"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "href"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Asset"),
              Core.projectionField = (Core.Name "href")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "title"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Asset"),
              Core.projectionField = (Core.Name "title")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Asset"),
              Core.projectionField = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Asset"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "roles"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

assetWithTitle :: Phantoms.TTerm Items.Asset -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Items.Asset
assetWithTitle original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.stac.items.Asset"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "href"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Asset"),
              Core.projectionField = (Core.Name "href")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "title"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Asset"),
              Core.projectionField = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Asset"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "roles"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Asset"),
              Core.projectionField = (Core.Name "roles")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

assetWithType :: Phantoms.TTerm Items.Asset -> Phantoms.TTerm (Maybe Items.MediaType) -> Phantoms.TTerm Items.Asset
assetWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.stac.items.Asset"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "href"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Asset"),
              Core.projectionField = (Core.Name "href")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "title"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Asset"),
              Core.projectionField = (Core.Name "title")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Asset"),
              Core.projectionField = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "roles"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Asset"),
              Core.projectionField = (Core.Name "roles")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

item :: Phantoms.TTerm Model.Feature -> Phantoms.TTerm Items.StacVersion -> Phantoms.TTerm [Items.Url] -> Phantoms.TTerm [Items.Link] -> Phantoms.TTerm (M.Map String Items.Asset) -> Phantoms.TTerm (Maybe Model.Id) -> Phantoms.TTerm Items.Item
item feature stacVersion stacExtensions links assets collection =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.stac.items.Item"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "feature"),
          Core.fieldTerm = (Phantoms.unTTerm feature)},
        Core.Field {
          Core.fieldName = (Core.Name "stacVersion"),
          Core.fieldTerm = (Phantoms.unTTerm stacVersion)},
        Core.Field {
          Core.fieldName = (Core.Name "stacExtensions"),
          Core.fieldTerm = (Phantoms.unTTerm stacExtensions)},
        Core.Field {
          Core.fieldName = (Core.Name "links"),
          Core.fieldTerm = (Phantoms.unTTerm links)},
        Core.Field {
          Core.fieldName = (Core.Name "assets"),
          Core.fieldTerm = (Phantoms.unTTerm assets)},
        Core.Field {
          Core.fieldName = (Core.Name "collection"),
          Core.fieldTerm = (Phantoms.unTTerm collection)}]}))

itemAssets :: Phantoms.TTerm Items.Item -> Phantoms.TTerm (M.Map String Items.Asset)
itemAssets x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.stac.items.Item"),
        Core.projectionField = (Core.Name "assets")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

itemCollection :: Phantoms.TTerm Items.Item -> Phantoms.TTerm (Maybe Model.Id)
itemCollection x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.stac.items.Item"),
        Core.projectionField = (Core.Name "collection")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

itemFeature :: Phantoms.TTerm Items.Item -> Phantoms.TTerm Model.Feature
itemFeature x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.stac.items.Item"),
        Core.projectionField = (Core.Name "feature")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

itemLinks :: Phantoms.TTerm Items.Item -> Phantoms.TTerm [Items.Link]
itemLinks x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.stac.items.Item"),
        Core.projectionField = (Core.Name "links")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

itemStacExtensions :: Phantoms.TTerm Items.Item -> Phantoms.TTerm [Items.Url]
itemStacExtensions x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.stac.items.Item"),
        Core.projectionField = (Core.Name "stacExtensions")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

itemStacVersion :: Phantoms.TTerm Items.Item -> Phantoms.TTerm Items.StacVersion
itemStacVersion x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.stac.items.Item"),
        Core.projectionField = (Core.Name "stacVersion")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

itemWithAssets :: Phantoms.TTerm Items.Item -> Phantoms.TTerm (M.Map String Items.Asset) -> Phantoms.TTerm Items.Item
itemWithAssets original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.stac.items.Item"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "feature"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Item"),
              Core.projectionField = (Core.Name "feature")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stacVersion"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Item"),
              Core.projectionField = (Core.Name "stacVersion")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stacExtensions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Item"),
              Core.projectionField = (Core.Name "stacExtensions")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "links"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Item"),
              Core.projectionField = (Core.Name "links")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "assets"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "collection"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Item"),
              Core.projectionField = (Core.Name "collection")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

itemWithCollection :: Phantoms.TTerm Items.Item -> Phantoms.TTerm (Maybe Model.Id) -> Phantoms.TTerm Items.Item
itemWithCollection original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.stac.items.Item"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "feature"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Item"),
              Core.projectionField = (Core.Name "feature")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stacVersion"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Item"),
              Core.projectionField = (Core.Name "stacVersion")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stacExtensions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Item"),
              Core.projectionField = (Core.Name "stacExtensions")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "links"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Item"),
              Core.projectionField = (Core.Name "links")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "assets"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Item"),
              Core.projectionField = (Core.Name "assets")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "collection"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

itemWithFeature :: Phantoms.TTerm Items.Item -> Phantoms.TTerm Model.Feature -> Phantoms.TTerm Items.Item
itemWithFeature original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.stac.items.Item"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "feature"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "stacVersion"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Item"),
              Core.projectionField = (Core.Name "stacVersion")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stacExtensions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Item"),
              Core.projectionField = (Core.Name "stacExtensions")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "links"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Item"),
              Core.projectionField = (Core.Name "links")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "assets"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Item"),
              Core.projectionField = (Core.Name "assets")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "collection"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Item"),
              Core.projectionField = (Core.Name "collection")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

itemWithLinks :: Phantoms.TTerm Items.Item -> Phantoms.TTerm [Items.Link] -> Phantoms.TTerm Items.Item
itemWithLinks original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.stac.items.Item"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "feature"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Item"),
              Core.projectionField = (Core.Name "feature")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stacVersion"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Item"),
              Core.projectionField = (Core.Name "stacVersion")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stacExtensions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Item"),
              Core.projectionField = (Core.Name "stacExtensions")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "links"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "assets"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Item"),
              Core.projectionField = (Core.Name "assets")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "collection"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Item"),
              Core.projectionField = (Core.Name "collection")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

itemWithStacExtensions :: Phantoms.TTerm Items.Item -> Phantoms.TTerm [Items.Url] -> Phantoms.TTerm Items.Item
itemWithStacExtensions original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.stac.items.Item"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "feature"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Item"),
              Core.projectionField = (Core.Name "feature")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stacVersion"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Item"),
              Core.projectionField = (Core.Name "stacVersion")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stacExtensions"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "links"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Item"),
              Core.projectionField = (Core.Name "links")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "assets"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Item"),
              Core.projectionField = (Core.Name "assets")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "collection"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Item"),
              Core.projectionField = (Core.Name "collection")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

itemWithStacVersion :: Phantoms.TTerm Items.Item -> Phantoms.TTerm Items.StacVersion -> Phantoms.TTerm Items.Item
itemWithStacVersion original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.stac.items.Item"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "feature"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Item"),
              Core.projectionField = (Core.Name "feature")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "stacVersion"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "stacExtensions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Item"),
              Core.projectionField = (Core.Name "stacExtensions")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "links"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Item"),
              Core.projectionField = (Core.Name "links")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "assets"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Item"),
              Core.projectionField = (Core.Name "assets")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "collection"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Item"),
              Core.projectionField = (Core.Name "collection")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

link :: Phantoms.TTerm Items.Url -> Phantoms.TTerm Items.RelationType -> Phantoms.TTerm (Maybe Items.MediaType) -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Items.Link
link href rel type_ title =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.stac.items.Link"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "href"),
          Core.fieldTerm = (Phantoms.unTTerm href)},
        Core.Field {
          Core.fieldName = (Core.Name "rel"),
          Core.fieldTerm = (Phantoms.unTTerm rel)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "title"),
          Core.fieldTerm = (Phantoms.unTTerm title)}]}))

linkHref :: Phantoms.TTerm Items.Link -> Phantoms.TTerm Items.Url
linkHref x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.stac.items.Link"),
        Core.projectionField = (Core.Name "href")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

linkRel :: Phantoms.TTerm Items.Link -> Phantoms.TTerm Items.RelationType
linkRel x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.stac.items.Link"),
        Core.projectionField = (Core.Name "rel")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

linkTitle :: Phantoms.TTerm Items.Link -> Phantoms.TTerm (Maybe String)
linkTitle x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.stac.items.Link"),
        Core.projectionField = (Core.Name "title")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

linkType :: Phantoms.TTerm Items.Link -> Phantoms.TTerm (Maybe Items.MediaType)
linkType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.stac.items.Link"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

linkWithHref :: Phantoms.TTerm Items.Link -> Phantoms.TTerm Items.Url -> Phantoms.TTerm Items.Link
linkWithHref original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.stac.items.Link"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "href"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Link"),
              Core.projectionField = (Core.Name "rel")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Link"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "title"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Link"),
              Core.projectionField = (Core.Name "title")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

linkWithRel :: Phantoms.TTerm Items.Link -> Phantoms.TTerm Items.RelationType -> Phantoms.TTerm Items.Link
linkWithRel original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.stac.items.Link"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "href"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Link"),
              Core.projectionField = (Core.Name "href")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rel"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Link"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "title"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Link"),
              Core.projectionField = (Core.Name "title")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

linkWithTitle :: Phantoms.TTerm Items.Link -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Items.Link
linkWithTitle original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.stac.items.Link"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "href"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Link"),
              Core.projectionField = (Core.Name "href")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Link"),
              Core.projectionField = (Core.Name "rel")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Link"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "title"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

linkWithType :: Phantoms.TTerm Items.Link -> Phantoms.TTerm (Maybe Items.MediaType) -> Phantoms.TTerm Items.Link
linkWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.stac.items.Link"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "href"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Link"),
              Core.projectionField = (Core.Name "href")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rel"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Link"),
              Core.projectionField = (Core.Name "rel")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "title"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.stac.items.Link"),
              Core.projectionField = (Core.Name "title")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

mediaType :: Phantoms.TTerm String -> Phantoms.TTerm Items.MediaType
mediaType x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.stac.items.MediaType"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

relationTypeIana :: Phantoms.TTerm Linkrelations.LinkRelationType -> Phantoms.TTerm Items.RelationType
relationTypeIana x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.stac.items.RelationType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "iana"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

relationTypeOther :: Phantoms.TTerm String -> Phantoms.TTerm Items.RelationType
relationTypeOther x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.stac.items.RelationType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "other"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

relationTypeStac :: Phantoms.TTerm Items.StacRelationType -> Phantoms.TTerm Items.RelationType
relationTypeStac x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.stac.items.RelationType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "stac"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

roleData :: Phantoms.TTerm Items.Role
roleData =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.stac.items.Role"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "data"),
        Core.fieldTerm = Core.TermUnit}}))

roleMetadata :: Phantoms.TTerm Items.Role
roleMetadata =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.stac.items.Role"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "metadata"),
        Core.fieldTerm = Core.TermUnit}}))

roleOther :: Phantoms.TTerm String -> Phantoms.TTerm Items.Role
roleOther x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.stac.items.Role"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "other"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

roleOverview :: Phantoms.TTerm Items.Role
roleOverview =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.stac.items.Role"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "overview"),
        Core.fieldTerm = Core.TermUnit}}))

roleThumbnail :: Phantoms.TTerm Items.Role
roleThumbnail =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.stac.items.Role"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "thumbnail"),
        Core.fieldTerm = Core.TermUnit}}))

stacRelationTypeCollection :: Phantoms.TTerm Items.StacRelationType
stacRelationTypeCollection =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.stac.items.StacRelationType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "collection"),
        Core.fieldTerm = Core.TermUnit}}))

stacRelationTypeDerivedFrom :: Phantoms.TTerm Items.StacRelationType
stacRelationTypeDerivedFrom =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.stac.items.StacRelationType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "derivedFrom"),
        Core.fieldTerm = Core.TermUnit}}))

stacRelationTypeParent :: Phantoms.TTerm Items.StacRelationType
stacRelationTypeParent =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.stac.items.StacRelationType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "parent"),
        Core.fieldTerm = Core.TermUnit}}))

stacRelationTypeRoot :: Phantoms.TTerm Items.StacRelationType
stacRelationTypeRoot =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.stac.items.StacRelationType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "root"),
        Core.fieldTerm = Core.TermUnit}}))

stacRelationTypeSelf :: Phantoms.TTerm Items.StacRelationType
stacRelationTypeSelf =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.stac.items.StacRelationType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "self"),
        Core.fieldTerm = Core.TermUnit}}))

stacVersion :: Phantoms.TTerm String -> Phantoms.TTerm Items.StacVersion
stacVersion x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.stac.items.StacVersion"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unMediaType :: Phantoms.TTerm Items.MediaType -> Phantoms.TTerm String
unMediaType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.stac.items.MediaType")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unStacVersion :: Phantoms.TTerm Items.StacVersion -> Phantoms.TTerm String
unStacVersion x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.stac.items.StacVersion")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unUri :: Phantoms.TTerm Items.Uri -> Phantoms.TTerm String
unUri x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.stac.items.Uri")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unUrl :: Phantoms.TTerm Items.Url -> Phantoms.TTerm String
unUrl x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.stac.items.Url")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

uri :: Phantoms.TTerm String -> Phantoms.TTerm Items.Uri
uri x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.stac.items.Uri"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

url :: Phantoms.TTerm String -> Phantoms.TTerm Items.Url
url x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.stac.items.Url"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
