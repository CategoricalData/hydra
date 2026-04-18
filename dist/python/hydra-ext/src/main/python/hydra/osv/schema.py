# Note: this is an automatically generated file. Do not edit.

r"""See https://ossf.github.io/osv-schema."""

from __future__ import annotations
from dataclasses import dataclass
from functools import lru_cache
from hydra.dsl.python import Maybe, Node, frozenlist
from typing import Annotated, TypeAlias, cast
import hydra.core

@dataclass(frozen=True)
class Credited:
    name: str
    contact: Maybe[frozenlist[Url]]

    TYPE_ = hydra.core.Name("hydra.osv.schema.Credited")
    NAME = hydra.core.Name("name")
    CONTACT = hydra.core.Name("contact")

class Ecosystem(Node[str]):
    r"""One of a limited set of defined ecosystems, currently Go, npm, OSS-Fuzz, PyPI, RubyGems, crates.io, Packagist, Maven, NuGet, Linux, Debian, Hex, Android, GitHub Actions, or Pub."""

Ecosystem.TYPE_ = hydra.core.Name("hydra.osv.schema.Ecosystem")

@dataclass(frozen=True)
class Entry:
    schema_version: Annotated[Maybe[OsvVersion], "The default value is '1.0.0', matching version 1.0 of the OSV Schema"]
    id: Id
    modified: Timestamp
    published: Maybe[Timestamp]
    withdrawn: Maybe[Timestamp]
    aliases: Maybe[frozenlist[Id]]
    related: Maybe[frozenlist[Id]]
    summary: Maybe[str]
    details: Maybe[Markdown]
    severity: Maybe[frozenlist[Severity]]
    affected: Maybe[frozenlist[PackageVersions]]
    references: Maybe[frozenlist[Reference]]
    credits: Maybe[frozenlist[Credited]]

    TYPE_ = hydra.core.Name("hydra.osv.schema.Entry")
    SCHEMA_VERSION = hydra.core.Name("schemaVersion")
    ID = hydra.core.Name("id")
    MODIFIED = hydra.core.Name("modified")
    PUBLISHED = hydra.core.Name("published")
    WITHDRAWN = hydra.core.Name("withdrawn")
    ALIASES = hydra.core.Name("aliases")
    RELATED = hydra.core.Name("related")
    SUMMARY = hydra.core.Name("summary")
    DETAILS = hydra.core.Name("details")
    SEVERITY = hydra.core.Name("severity")
    AFFECTED = hydra.core.Name("affected")
    REFERENCES = hydra.core.Name("references")
    CREDITS = hydra.core.Name("credits")

class EventIntroduced(Node["VersionOrZero"]):
    ...

class EventFixed(Node["Version"]):
    ...

class EventLastAffected(Node["Version"]):
    ...

class EventLimit(Node["VersionOrStar"]):
    ...

class _EventMeta(type):
    def __getitem__(cls, item):
        return object

class Event(metaclass=_EventMeta):
    r"""EventIntroduced | EventFixed | EventLastAffected | EventLimit"""

    TYPE_ = hydra.core.Name("hydra.osv.schema.Event")
    INTRODUCED = hydra.core.Name("introduced")
    FIXED = hydra.core.Name("fixed")
    LAST_AFFECTED = hydra.core.Name("lastAffected")
    LIMIT = hydra.core.Name("limit")

class Id(Node[str]):
    r"""A string of the format <DB>-<ENTRYID>, where DB names the database and ENTRYID is in the format used by the database. For example: OSV-2020-111, CVE-2021-3114, or GHSA-vp9c-fpxx-744v."""

Id.TYPE_ = hydra.core.Name("hydra.osv.schema.Id")

class Markdown(Node[str]):
    r"""CommonMark markdown text."""

Markdown.TYPE_ = hydra.core.Name("hydra.osv.schema.Markdown")

class OsvVersion(Node[str]):
    r"""A string which follows the SemVer 2.0.0 format, with no leading 'v' prefix."""

OsvVersion.TYPE_ = hydra.core.Name("hydra.osv.schema.OsvVersion")

@dataclass(frozen=True)
class Package:
    ecosystem: Ecosystem
    name: str
    purl: Maybe[Url]

    TYPE_ = hydra.core.Name("hydra.osv.schema.Package")
    ECOSYSTEM = hydra.core.Name("ecosystem")
    NAME = hydra.core.Name("name")
    PURL = hydra.core.Name("purl")

@dataclass(frozen=True)
class PackageVersions:
    package: Package
    ranges: Maybe[frozenlist[VersionRange]]
    versions: Maybe[frozenlist[Version]]

    TYPE_ = hydra.core.Name("hydra.osv.schema.PackageVersions")
    PACKAGE = hydra.core.Name("package")
    RANGES = hydra.core.Name("ranges")
    VERSIONS = hydra.core.Name("versions")

@dataclass(frozen=True)
class Reference:
    type: ReferenceType
    url: Url

    TYPE_ = hydra.core.Name("hydra.osv.schema.Reference")
    TYPE = hydra.core.Name("type")
    URL = hydra.core.Name("url")

class ReferenceType(Node[str]):
    r"""One of ADVISORY, ARTICLE, REPORT, FIX, GIT, PACKAGE, EVIDENCE, or WEB."""

ReferenceType.TYPE_ = hydra.core.Name("hydra.osv.schema.ReferenceType")

@dataclass(frozen=True)
class Severity:
    type: SeverityType
    score: SeverityScore

    TYPE_ = hydra.core.Name("hydra.osv.schema.Severity")
    TYPE = hydra.core.Name("type")
    SCORE = hydra.core.Name("score")

class SeverityScore(Node[str]):
    ...

SeverityScore.TYPE_ = hydra.core.Name("hydra.osv.schema.SeverityScore")

class SeverityType(Node[str]):
    r"""The value CVSS_V3, or future supported types."""

SeverityType.TYPE_ = hydra.core.Name("hydra.osv.schema.SeverityType")

class Timestamp(Node[str]):
    r"""An RFC3339-formatted timestamp in UTC (ending in 'Z')."""

Timestamp.TYPE_ = hydra.core.Name("hydra.osv.schema.Timestamp")

class Url(Node[str]):
    ...

Url.TYPE_ = hydra.core.Name("hydra.osv.schema.Url")

class Version(Node[str]):
    r"""A version number in an ecosystem-specific format."""

Version.TYPE_ = hydra.core.Name("hydra.osv.schema.Version")

class VersionOrStar(Node[str]):
    r"""An ecosystem-specific version number, or the string '*' representing infinity."""

VersionOrStar.TYPE_ = hydra.core.Name("hydra.osv.schema.VersionOrStar")

class VersionOrZero(Node[str]):
    r"""An ecosystem-specific version number, or the string '0' representing a version that sorts before any other version."""

VersionOrZero.TYPE_ = hydra.core.Name("hydra.osv.schema.VersionOrZero")

@dataclass(frozen=True)
class VersionRange:
    type: VersionType
    repo: Maybe[Url]
    events: frozenlist[Event]

    TYPE_ = hydra.core.Name("hydra.osv.schema.VersionRange")
    TYPE = hydra.core.Name("type")
    REPO = hydra.core.Name("repo")
    EVENTS = hydra.core.Name("events")

class VersionType(Node[str]):
    r"""One of the values 'SEMVER', 'ECOSYSTEM', or 'GIT."""

VersionType.TYPE_ = hydra.core.Name("hydra.osv.schema.VersionType")
