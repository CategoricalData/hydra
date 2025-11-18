module Hydra.Ext.Sources.Other.Osv where

-- Standard imports for type-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import Hydra.Dsl.Types as Types
import qualified Hydra.Sources.Kernel.Types.Accessors   as Accessors
import qualified Hydra.Sources.Kernel.Types.Ast         as Ast
import qualified Hydra.Sources.Kernel.Types.Classes     as Classes
import qualified Hydra.Sources.Kernel.Types.Coders      as Coders
import qualified Hydra.Sources.Kernel.Types.Compute     as Compute
import qualified Hydra.Sources.Kernel.Types.Constraints as Constraints
import qualified Hydra.Sources.Kernel.Types.Core        as Core
import qualified Hydra.Sources.Kernel.Types.Grammar     as Grammar
import qualified Hydra.Sources.Kernel.Types.Graph       as Graph
import qualified Hydra.Sources.Kernel.Types.Json        as Json
import qualified Hydra.Sources.Kernel.Types.Module      as Module
import qualified Hydra.Sources.Kernel.Types.Phantoms    as Phantoms
import qualified Hydra.Sources.Kernel.Types.Query       as Query
import qualified Hydra.Sources.Kernel.Types.Relational  as Relational
import qualified Hydra.Sources.Kernel.Types.Tabular     as Tabular
import qualified Hydra.Sources.Kernel.Types.Testing     as Testing
import qualified Hydra.Sources.Kernel.Types.Topology    as Topology
import qualified Hydra.Sources.Kernel.Types.Typing      as Typing
import qualified Hydra.Sources.Kernel.Types.Util        as Util
import qualified Hydra.Sources.Kernel.Types.Variants    as Variants
import qualified Hydra.Sources.Kernel.Types.Workflow    as Workflow
import qualified Data.Int                               as I
import qualified Data.List                              as L
import qualified Data.Map                               as M
import qualified Data.Set                               as S
import qualified Data.Maybe                             as Y


-- Note: database_specific and ecosystem_specific fields are ignored, though they must be tolerated when reading entry JSON
osvSchemaModule :: Module
osvSchemaModule = Module ns elements [] [] $
    Just "See https://ossf.github.io/osv-schema"
  where
    ns = Namespace "hydra.ext.dev.osv.schema"
    def = datatype ns
    osv = typeref ns

    elements = [

      def "Credited" $
        record [
          "name">: string,
          "contact">: optional $ list $ osv "Url"],

      def "Ecosystem" $
        doc ("One of a limited set of defined ecosystems, currently "
          ++ "Go, npm, OSS-Fuzz, PyPI, RubyGems, crates.io, Packagist, Maven, NuGet, Linux, Debian, Hex, Android, GitHub Actions, or Pub") $
        wrap string,

      def "Entry" $
        record [
          "schemaVersion">:
            doc "The default value is '1.0.0', matching version 1.0 of the OSV Schema" $
            optional $ osv "OsvVersion",
          "id">: osv "Id",
          "modified">: osv "Timestamp",
          "published">: optional $ osv "Timestamp",
          "withdrawn">: optional $ osv "Timestamp",
          "aliases">: optional $ list $ osv "Id",
          "related">: optional $ list $ osv "Id",
          "summary">: optional string,
          "details">: optional $ osv "Markdown",
          "severity">: optional $ list $ osv "Severity",
          "affected">: optional  $ list $ osv "PackageVersions",
          "references">: optional $ list $ osv "Reference",
          "credits">: optional $ list $ osv "Credited"],

      def "Event" $
        union [
          "introduced">: osv "VersionOrZero",
          "fixed">: osv "Version",
          "lastAffected">: osv "Version",
          "limit">: osv "VersionOrStar"],

      def "Id" $
        doc ("A string of the format <DB>-<ENTRYID>, where DB names the database and ENTRYID is in the format used "
          ++ "by the database. For example: OSV-2020-111, CVE-2021-3114, or GHSA-vp9c-fpxx-744v") $
        wrap string,

      def "Markdown" $
        doc "CommonMark markdown text" $
        wrap string,

      def "OsvVersion" $
        doc "A string which follows the SemVer 2.0.0 format, with no leading 'v' prefix" $
        wrap string,

      def "Package" $
        record [
          "ecosystem">: osv "Ecosystem",
          "name">: string,
          "purl">: optional $ osv "Url"],

      def "PackageVersions" $
        record [
          "package">: osv "Package",
          "ranges">: optional $ list $ osv "VersionRange",
          "versions">: optional $ list $ osv "Version"],

      def "Reference" $
        record [
          "type">: osv "ReferenceType",
          "url">: osv "Url"],

      def "ReferenceType" $
        doc "One of ADVISORY, ARTICLE, REPORT, FIX, GIT, PACKAGE, EVIDENCE, or WEB" $
        wrap string,

      def "Severity" $
        record [
          "type">: osv "SeverityType",
          "score">: osv "SeverityScore"],

      def "SeverityScore" $ wrap string,

      def "SeverityType" $
        doc "The value CVSS_V3, or future supported types" $
        wrap string,

      def "Timestamp" $
        doc "An RFC3339-formatted timestamp in UTC (ending in 'Z')" $
        wrap string,

      def "Url" $ wrap string,

      def "Version" $
        doc "A version number in an ecosystem-specific format" $
        wrap string,

      def "VersionOrStar" $
        doc "An ecosystem-specific version number, or the string '*' representing infinity" $
        wrap string,

      def "VersionOrZero" $
        doc "An ecosystem-specific version number, or the string '0' representing a version that sorts before any other version" $
        wrap string,

      def "VersionRange" $
        record [
          "type">: osv "VersionType",
          "repo">: optional $ osv "Url",
          "events">: list $ osv "Event"],

      def "VersionType" $
        doc "One of the values 'SEMVER', 'ECOSYSTEM', or 'GIT" $
        wrap string]
