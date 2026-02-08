module Hydra.Ext.Sources.Other.Osv where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel
import           Hydra.Dsl.Annotations
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types                 ((>:))
import qualified Hydra.Dsl.Types                 as T
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Data.List                       as L
import qualified Data.Map                        as M
import qualified Data.Set                        as S
import qualified Data.Maybe                      as Y


ns :: Namespace
ns = Namespace "hydra.ext.dev.osv.schema"

define :: String -> Type -> Binding
define = defineType ns

osv :: String -> Type
osv = typeref ns

-- Note: database_specific and ecosystem_specific fields are ignored, though they must be tolerated when reading entry JSON
module_ :: Module
module_ = Module ns elements [] [] $
    Just "See https://ossf.github.io/osv-schema"
  where
    elements = [
      credited,
      ecosystem,
      entry,
      event,
      id_,
      markdown,
      osvVersion,
      package_,
      packageVersions,
      reference,
      referenceType,
      severity,
      severityScore,
      severityType,
      timestamp,
      url,
      version,
      versionOrStar,
      versionOrZero,
      versionRange,
      versionType]

credited :: Binding
credited = define "Credited" $
  T.record [
    "name">: T.string,
    "contact">: T.maybe $ T.list $ osv "Url"]

ecosystem :: Binding
ecosystem = define "Ecosystem" $
  doc ("One of a limited set of defined ecosystems, currently "
    ++ "Go, npm, OSS-Fuzz, PyPI, RubyGems, crates.io, Packagist, Maven, NuGet, Linux, Debian, Hex, Android, GitHub Actions, or Pub") $
  T.wrap T.string

entry :: Binding
entry = define "Entry" $
  T.record [
    "schemaVersion">:
      doc "The default value is '1.0.0', matching version 1.0 of the OSV Schema" $
      T.maybe $ osv "OsvVersion",
    "id">: osv "Id",
    "modified">: osv "Timestamp",
    "published">: T.maybe $ osv "Timestamp",
    "withdrawn">: T.maybe $ osv "Timestamp",
    "aliases">: T.maybe $ T.list $ osv "Id",
    "related">: T.maybe $ T.list $ osv "Id",
    "summary">: T.maybe T.string,
    "details">: T.maybe $ osv "Markdown",
    "severity">: T.maybe $ T.list $ osv "Severity",
    "affected">: T.maybe  $ T.list $ osv "PackageVersions",
    "references">: T.maybe $ T.list $ osv "Reference",
    "credits">: T.maybe $ T.list $ osv "Credited"]

event :: Binding
event = define "Event" $
  T.union [
    "introduced">: osv "VersionOrZero",
    "fixed">: osv "Version",
    "lastAffected">: osv "Version",
    "limit">: osv "VersionOrStar"]

id_ :: Binding
id_ = define "Id" $
  doc ("A string of the format <DB>-<ENTRYID>, where DB names the database and ENTRYID is in the format used "
    ++ "by the database. For example: OSV-2020-111, CVE-2021-3114, or GHSA-vp9c-fpxx-744v") $
  T.wrap T.string

markdown :: Binding
markdown = define "Markdown" $
  doc "CommonMark markdown text" $
  T.wrap T.string

osvVersion :: Binding
osvVersion = define "OsvVersion" $
  doc "A string which follows the SemVer 2.0.0 format, with no leading 'v' prefix" $
  T.wrap T.string

package_ :: Binding
package_ = define "Package" $
  T.record [
    "ecosystem">: osv "Ecosystem",
    "name">: T.string,
    "purl">: T.maybe $ osv "Url"]

packageVersions :: Binding
packageVersions = define "PackageVersions" $
  T.record [
    "package">: osv "Package",
    "ranges">: T.maybe $ T.list $ osv "VersionRange",
    "versions">: T.maybe $ T.list $ osv "Version"]

reference :: Binding
reference = define "Reference" $
  T.record [
    "type">: osv "ReferenceType",
    "url">: osv "Url"]

referenceType :: Binding
referenceType = define "ReferenceType" $
  doc "One of ADVISORY, ARTICLE, REPORT, FIX, GIT, PACKAGE, EVIDENCE, or WEB" $
  T.wrap T.string

severity :: Binding
severity = define "Severity" $
  T.record [
    "type">: osv "SeverityType",
    "score">: osv "SeverityScore"]

severityScore :: Binding
severityScore = define "SeverityScore" $ T.wrap T.string

severityType :: Binding
severityType = define "SeverityType" $
  doc "The value CVSS_V3, or future supported types" $
  T.wrap T.string

timestamp :: Binding
timestamp = define "Timestamp" $
  doc "An RFC3339-formatted timestamp in UTC (ending in 'Z')" $
  T.wrap T.string

url :: Binding
url = define "Url" $ T.wrap T.string

version :: Binding
version = define "Version" $
  doc "A version number in an ecosystem-specific format" $
  T.wrap T.string

versionOrStar :: Binding
versionOrStar = define "VersionOrStar" $
  doc "An ecosystem-specific version number, or the string '*' representing infinity" $
  T.wrap T.string

versionOrZero :: Binding
versionOrZero = define "VersionOrZero" $
  doc "An ecosystem-specific version number, or the string '0' representing a version that sorts before any other version" $
  T.wrap T.string

versionRange :: Binding
versionRange = define "VersionRange" $
  T.record [
    "type">: osv "VersionType",
    "repo">: T.maybe $ osv "Url",
    "events">: T.list $ osv "Event"]

versionType :: Binding
versionType = define "VersionType" $
  doc "One of the values 'SEMVER', 'ECOSYSTEM', or 'GIT" $
  T.wrap T.string
