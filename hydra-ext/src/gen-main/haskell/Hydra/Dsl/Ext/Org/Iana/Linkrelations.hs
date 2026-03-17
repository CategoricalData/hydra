-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.ext.org.iana.linkrelations

module Hydra.Dsl.Ext.Org.Iana.Linkrelations where

import qualified Hydra.Core as Core
import qualified Hydra.Ext.Org.Iana.Linkrelations as Linkrelations
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

linkRelationTypeAbout :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeAbout = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "about"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeAcl :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeAcl = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "acl"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeAlternate :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeAlternate = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "alternate"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeAmphtml :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeAmphtml = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "amphtml"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeAppendix :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeAppendix = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "appendix"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeApple_touch_icon :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeApple_touch_icon = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "apple-touch-icon"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeApple_touch_startup_image :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeApple_touch_startup_image = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "apple-touch-startup-image"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeArchives :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeArchives = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "archives"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeAuthor :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeAuthor = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "author"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeBlocked_by :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeBlocked_by = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "blocked-by"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeBookmark :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeBookmark = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "bookmark"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeCanonical :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeCanonical = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "canonical"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeChapter :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeChapter = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "chapter"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeCite_as :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeCite_as = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "cite-as"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeCollection :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeCollection = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "collection"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeContents :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeContents = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "contents"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeConvertedfrom :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeConvertedfrom = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "convertedfrom"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeCopyright :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeCopyright = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "copyright"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeCreate_form :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeCreate_form = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "create-form"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeCurrent :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeCurrent = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "current"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeDescribedby :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeDescribedby = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "describedby"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeDescribes :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeDescribes = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "describes"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeDisclosure :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeDisclosure = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "disclosure"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeDns_prefetch :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeDns_prefetch = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "dns-prefetch"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeDuplicate :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeDuplicate = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "duplicate"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeEdit :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeEdit = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "edit"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeEdit_form :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeEdit_form = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "edit-form"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeEdit_media :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeEdit_media = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "edit-media"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeEnclosure :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeEnclosure = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "enclosure"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeExternal :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeExternal = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "external"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeFirst :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeFirst = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "first"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeGlossary :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeGlossary = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "glossary"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeHelp :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeHelp = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "help"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeHosts :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeHosts = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "hosts"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeHub :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeHub = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "hub"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeIcon :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeIcon = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "icon"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeIndex :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeIndex = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "index"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeIntervalafter :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeIntervalafter = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "intervalafter"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeIntervalbefore :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeIntervalbefore = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "intervalbefore"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeIntervalcontains :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeIntervalcontains = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "intervalcontains"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeIntervaldisjoint :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeIntervaldisjoint = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "intervaldisjoint"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeIntervalduring :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeIntervalduring = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "intervalduring"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeIntervalequals :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeIntervalequals = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "intervalequals"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeIntervalfinishedby :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeIntervalfinishedby = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "intervalfinishedby"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeIntervalfinishes :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeIntervalfinishes = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "intervalfinishes"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeIntervalin :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeIntervalin = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "intervalin"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeIntervalmeets :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeIntervalmeets = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "intervalmeets"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeIntervalmetby :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeIntervalmetby = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "intervalmetby"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeIntervaloverlappedby :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeIntervaloverlappedby = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "intervaloverlappedby"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeIntervaloverlaps :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeIntervaloverlaps = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "intervaloverlaps"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeIntervalstartedby :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeIntervalstartedby = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "intervalstartedby"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeIntervalstarts :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeIntervalstarts = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "intervalstarts"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeItem :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeItem = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "item"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeLast :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeLast = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "last"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeLatest_version :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeLatest_version = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "latest-version"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeLicense :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeLicense = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "license"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeLinkset :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeLinkset = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "linkset"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeLrdd :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeLrdd = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "lrdd"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeManifest :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeManifest = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "manifest"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeMask_icon :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeMask_icon = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "mask-icon"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeMedia_feed :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeMedia_feed = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "media-feed"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeMemento :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeMemento = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "memento"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeMicropub :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeMicropub = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "micropub"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeModulepreload :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeModulepreload = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "modulepreload"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeMonitor :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeMonitor = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "monitor"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeMonitor_group :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeMonitor_group = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "monitor-group"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeNext :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeNext = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "next"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeNext_archive :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeNext_archive = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "next-archive"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeNofollow :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeNofollow = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "nofollow"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeNoopener :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeNoopener = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "noopener"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeNoreferrer :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeNoreferrer = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "noreferrer"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeOpener :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeOpener = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "opener"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeLocal_id :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeLocal_id = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "openid2.local_id"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeProvider :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeProvider = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "openid2.provider"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeOriginal :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeOriginal = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "original"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeP3pv1 :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeP3pv1 = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "p3pv1"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypePayment :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypePayment = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "payment"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypePingback :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypePingback = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "pingback"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypePreconnect :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypePreconnect = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "preconnect"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypePredecessor_version :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypePredecessor_version = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "predecessor-version"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypePrefetch :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypePrefetch = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "prefetch"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypePreload :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypePreload = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "preload"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypePrerender :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypePrerender = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "prerender"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypePrev :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypePrev = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "prev"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypePreview :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypePreview = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "preview"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypePrevious :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypePrevious = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "previous"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypePrev_archive :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypePrev_archive = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "prev-archive"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypePrivacy_policy :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypePrivacy_policy = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "privacy-policy"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeProfile :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeProfile = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "profile"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypePublication :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypePublication = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "publication"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeRelated :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeRelated = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "related"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeRestconf :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeRestconf = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "restconf"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeReplies :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeReplies = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "replies"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeRuleinput :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeRuleinput = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "ruleinput"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeSearch :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeSearch = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "search"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeSection :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeSection = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "section"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeSelf :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeSelf = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "self"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeService :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeService = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "service"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeService_desc :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeService_desc = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "service-desc"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeService_doc :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeService_doc = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "service-doc"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeService_meta :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeService_meta = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "service-meta"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeSiptrunkingcapability :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeSiptrunkingcapability = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "siptrunkingcapability"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeSponsored :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeSponsored = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "sponsored"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeStart :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeStart = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "start"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeStatus :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeStatus = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "status"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeStylesheet :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeStylesheet = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "stylesheet"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeSubsection :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeSubsection = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "subsection"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeSuccessor_version :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeSuccessor_version = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "successor-version"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeSunset :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeSunset = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "sunset"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeTag :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeTag = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "tag"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeTerms_of_service :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeTerms_of_service = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "terms-of-service"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeTimegate :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeTimegate = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "timegate"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeTimemap :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeTimemap = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "timemap"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeType :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeType = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "type"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeUgc :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeUgc = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "ugc"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeUp :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeUp = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "up"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeVersion_history :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeVersion_history = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "version-history"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeVia :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeVia = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "via"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeWebmention :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeWebmention = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "webmention"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeWorking_copy :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeWorking_copy = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "working-copy"),
    Core.fieldTerm = Core.TermUnit}})))

linkRelationTypeWorking_copy_of :: (Phantoms.TTerm Linkrelations.LinkRelationType)
linkRelationTypeWorking_copy_of = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.iana.linkrelations.LinkRelationType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "working-copy-of"),
    Core.fieldTerm = Core.TermUnit}})))
