# Note: this is an automatically generated file. Do not edit.

r"""All IANA Link Relation Types, as of 2022-10-11. See https://www.iana.org/assignments/link-relations/link-relations.xhtml."""

from __future__ import annotations
from enum import Enum
from functools import lru_cache
from typing import TypeAlias, cast
import hydra.core

class LinkRelationType(Enum):
    r"""An IANA link relation type."""

    ABOUT = hydra.core.Name("about")
    r"""Refers to a resource that is the subject of the link's context. [RFC6903], section 2"""

    ACL = hydra.core.Name("acl")
    r"""Asserts that the link target provides an access control resource for the link context. [https://solidproject.org/TR/wac#acl-link-relation]"""

    ALTERNATE = hydra.core.Name("alternate")
    r"""Refers to a substitute for this context [HTML]"""

    AMPHTML = hydra.core.Name("amphtml")
    r"""Used to reference alternative content that uses the AMP profile of the HTML format. [AMP HTML]"""

    APPENDIX = hydra.core.Name("appendix")
    r"""Refers to an appendix. [HTML 4.01 Specification]"""

    APPLE_TOUCH_ICON = hydra.core.Name("apple-touch-icon")
    r"""Refers to an icon for the context. Synonym for icon. [Configuring Web Applications]"""

    APPLE_TOUCH_STARTUP_IMAGE = hydra.core.Name("apple-touch-startup-image")
    r"""Refers to a launch screen for the context. [Configuring Web Applications]"""

    ARCHIVES = hydra.core.Name("archives")
    r"""Refers to a collection of records, documents, or other materials of historical interest. [HTML5]"""

    AUTHOR = hydra.core.Name("author")
    r"""Refers to the context's author. [HTML]"""

    BLOCKED_BY = hydra.core.Name("blocked-by")
    r"""Identifies the entity that blocks access to a resource following receipt of a legal demand. [RFC7725]"""

    BOOKMARK = hydra.core.Name("bookmark")
    r"""Gives a permanent link to use for bookmarking purposes. [HTML]"""

    CANONICAL = hydra.core.Name("canonical")
    r"""Designates the preferred version of a resource (the IRI and its contents). [RFC6596]"""

    CHAPTER = hydra.core.Name("chapter")
    r"""Refers to a chapter in a collection of resources. [HTML 4.01 Specification]"""

    CITE_AS = hydra.core.Name("cite-as")
    r"""Indicates that the link target is preferred over the link context for the purpose of permanent citation. [RFC8574]"""

    COLLECTION = hydra.core.Name("collection")
    r"""The target IRI points to a resource which represents the collection resource for the context IRI. [RFC6573]"""

    CONTENTS = hydra.core.Name("contents")
    r"""Refers to a table of contents. [HTML 4.01 Specification]"""

    CONVERTEDFROM = hydra.core.Name("convertedfrom")
    r"""The document linked to was later converted to the document that contains this link relation. For example, an RFC can have a link to the Internet-Draft that became the RFC; in that case, the link relation would be "convertedFrom". [RFC7991] This relation is different than "predecessor-version" in that "predecessor-version" is for items in a version control system. It is also different than "previous" in that this relation is used for converted resources, not those that are part of a sequence of resources."""

    COPYRIGHT = hydra.core.Name("copyright")
    r"""Refers to a copyright statement that applies to the link's context. [HTML 4.01 Specification]"""

    CREATE_FORM = hydra.core.Name("create-form")
    r"""The target IRI points to a resource where a submission form can be obtained. [RFC6861]"""

    CURRENT = hydra.core.Name("current")
    r"""Refers to a resource containing the most recent item(s) in a collection of resources. [RFC5005]"""

    DESCRIBEDBY = hydra.core.Name("describedby")
    r"""Refers to a resource providing information about the link's context. [Protocol for Web Description Resources (POWDER)]"""

    DESCRIBES = hydra.core.Name("describes")
    r"""The relationship A 'describes' B asserts that resource A provides a description of resource B. There are no constraints on the format or representation of either A or B, neither are there any further constraints on either resource. [RFC6892] This link relation type is the inverse of the 'describedby' relation type. While 'describedby' establishes a relation from the described resource back to the resource that describes it, 'describes' established a relation from the describing resource to the resource it describes. If B is 'describedby' A, then A 'describes' B."""

    DISCLOSURE = hydra.core.Name("disclosure")
    r"""Refers to a list of patent disclosures made with respect to material for which 'disclosure' relation is specified. [RFC6579]"""

    DNS_PREFETCH = hydra.core.Name("dns-prefetch")
    r"""Used to indicate an origin that will be used to fetch required resources for the link context, and that the user agent ought to resolve as early as possible. [Resource Hints]"""

    DUPLICATE = hydra.core.Name("duplicate")
    r"""Refers to a resource whose available representations are byte-for-byte identical with the corresponding representations of the context IRI. [RFC6249] This relation is for static resources. That is, an HTTP GET request on any duplicate will return the same representation. It does not make sense for dynamic or POSTable resources and should not be used for them."""

    EDIT = hydra.core.Name("edit")
    r"""Refers to a resource that can be used to edit the link's context. [RFC5023]"""

    EDIT_FORM = hydra.core.Name("edit-form")
    r"""The target IRI points to a resource where a submission form for editing associated resource can be obtained. [RFC6861]"""

    EDIT_MEDIA = hydra.core.Name("edit-media")
    r"""Refers to a resource that can be used to edit media associated with the link's context. [RFC5023]"""

    ENCLOSURE = hydra.core.Name("enclosure")
    r"""Identifies a related resource that is potentially large and might require special handling. [RFC4287]"""

    EXTERNAL = hydra.core.Name("external")
    r"""Refers to a resource that is not part of the same site as the current context. [HTML]"""

    FIRST = hydra.core.Name("first")
    r"""An IRI that refers to the furthest preceding resource in a series of resources. [RFC8288] This relation type registration did not indicate a reference. Originally requested by Mark Nottingham in December 2004."""

    GLOSSARY = hydra.core.Name("glossary")
    r"""Refers to a glossary of terms. [HTML 4.01 Specification]"""

    HELP = hydra.core.Name("help")
    r"""Refers to context-sensitive help. [HTML]"""

    HOSTS = hydra.core.Name("hosts")
    r"""Refers to a resource hosted by the server indicated by the link context. [RFC6690] This relation is used in CoRE where links are retrieved as a "/.well-known/core" resource representation, and is the default relation type in the CoRE Link Format."""

    HUB = hydra.core.Name("hub")
    r"""Refers to a hub that enables registration for notification of updates to the context. [WebSub] This relation type was requested by Brett Slatkin."""

    ICON = hydra.core.Name("icon")
    r"""Refers to an icon representing the link's context. [HTML]"""

    INDEX = hydra.core.Name("index")
    r"""Refers to an index. [HTML 4.01 Specification]"""

    INTERVALAFTER = hydra.core.Name("intervalafter")
    r"""refers to a resource associated with a time interval that ends before the beginning of the time interval associated with the context resource [Time Ontology in OWL] section 4.2.21"""

    INTERVALBEFORE = hydra.core.Name("intervalbefore")
    r"""refers to a resource associated with a time interval that begins after the end of the time interval associated with the context resource [Time Ontology in OWL] section 4.2.22"""

    INTERVALCONTAINS = hydra.core.Name("intervalcontains")
    r"""refers to a resource associated with a time interval that begins after the beginning of the time interval associated with the context resource, and ends before the end of the time interval associated with the context resource [Time Ontology in OWL] section 4.2.23"""

    INTERVALDISJOINT = hydra.core.Name("intervaldisjoint")
    r"""refers to a resource associated with a time interval that begins after the end of the time interval associated with the context resource, or ends before the beginning of the time interval associated with the context resource [Time Ontology in OWL] section 4.2.24"""

    INTERVALDURING = hydra.core.Name("intervalduring")
    r"""refers to a resource associated with a time interval that begins before the beginning of the time interval associated with the context resource, and ends after the end of the time interval associated with the context resource [Time Ontology in OWL] section 4.2.25"""

    INTERVALEQUALS = hydra.core.Name("intervalequals")
    r"""refers to a resource associated with a time interval whose beginning coincides with the beginning of the time interval associated with the context resource, and whose end coincides with the end of the time interval associated with the context resource [Time Ontology in OWL] section 4.2.26"""

    INTERVALFINISHEDBY = hydra.core.Name("intervalfinishedby")
    r"""refers to a resource associated with a time interval that begins after the beginning of the time interval associated with the context resource, and whose end coincides with the end of the time interval associated with the context resource [Time Ontology in OWL] section 4.2.27"""

    INTERVALFINISHES = hydra.core.Name("intervalfinishes")
    r"""refers to a resource associated with a time interval that begins before the beginning of the time interval associated with the context resource, and whose end coincides with the end of the time interval associated with the context resource [Time Ontology in OWL] section 4.2.28"""

    INTERVALIN = hydra.core.Name("intervalin")
    r"""refers to a resource associated with a time interval that begins before or is coincident with the beginning of the time interval associated with the context resource, and ends after or is coincident with the end of the time interval associated with the context resource [Time Ontology in OWL] section 4.2.29"""

    INTERVALMEETS = hydra.core.Name("intervalmeets")
    r"""refers to a resource associated with a time interval whose beginning coincides with the end of the time interval associated with the context resource [Time Ontology in OWL] section 4.2.30"""

    INTERVALMETBY = hydra.core.Name("intervalmetby")
    r"""refers to a resource associated with a time interval whose end coincides with the beginning of the time interval associated with the context resource [Time Ontology in OWL] section 4.2.31"""

    INTERVALOVERLAPPEDBY = hydra.core.Name("intervaloverlappedby")
    r"""refers to a resource associated with a time interval that begins before the beginning of the time interval associated with the context resource, and ends after the beginning of the time interval associated with the context resource [Time Ontology in OWL] section 4.2.32"""

    INTERVALOVERLAPS = hydra.core.Name("intervaloverlaps")
    r"""refers to a resource associated with a time interval that begins before the end of the time interval associated with the context resource, and ends after the end of the time interval associated with the context resource [Time Ontology in OWL] section 4.2.33"""

    INTERVALSTARTEDBY = hydra.core.Name("intervalstartedby")
    r"""refers to a resource associated with a time interval whose beginning coincides with the beginning of the time interval associated with the context resource, and ends before the end of the time interval associated with the context resource [Time Ontology in OWL] section 4.2.34"""

    INTERVALSTARTS = hydra.core.Name("intervalstarts")
    r"""refers to a resource associated with a time interval whose beginning coincides with the beginning of the time interval associated with the context resource, and ends after the end of the time interval associated with the context resource [Time Ontology in OWL] section 4.2.35"""

    ITEM = hydra.core.Name("item")
    r"""The target IRI points to a resource that is a member of the collection represented by the context IRI. [RFC6573]"""

    LAST = hydra.core.Name("last")
    r"""An IRI that refers to the furthest following resource in a series of resources. [RFC8288] This relation type registration did not indicate a reference. Originally requested by Mark Nottingham in December 2004."""

    LATEST_VERSION = hydra.core.Name("latest-version")
    r"""Points to a resource containing the latest (e.g., current) version of the context. [RFC5829]"""

    LICENSE = hydra.core.Name("license")
    r"""Refers to a license associated with this context. [RFC4946] For implications of use in HTML, see: http://www.w3.org/TR/html5/links.html#link-type-license"""

    LINKSET = hydra.core.Name("linkset")
    r"""The link target of a link with the "linkset" relation type provides a set of links, including links in which the link context of the link participates. [RFC9264]"""

    LRDD = hydra.core.Name("lrdd")
    r"""Refers to further information about the link's context, expressed as a LRDD ("Link-based Resource Descriptor Document") resource. See [RFC6415] for information about processing this relation type in host-meta documents. When used elsewhere, it refers to additional links and other metadata. Multiple instances indicate additional LRDD resources. LRDD resources MUST have an "application/xrd+xml" representation, and MAY have others. [RFC6415]"""

    MANIFEST = hydra.core.Name("manifest")
    r"""Links to a manifest file for the context. [Web App Manifest]"""

    MASK_ICON = hydra.core.Name("mask-icon")
    r"""Refers to a mask that can be applied to the icon for the context. [Creating Pinned Tab Icons]"""

    MEDIA_FEED = hydra.core.Name("media-feed")
    r"""Refers to a feed of personalised media recommendations relevant to the link context. [https://wicg.github.io/media-feeds/#discovery-of-media-feeds]"""

    MEMENTO = hydra.core.Name("memento")
    r"""The Target IRI points to a Memento, a fixed resource that will not change state anymore. [RFC7089] A Memento for an Original Resource is a resource that encapsulates a prior state of the Original Resource."""

    MICROPUB = hydra.core.Name("micropub")
    r"""Links to the context's Micropub endpoint. [Micropub]"""

    MODULEPRELOAD = hydra.core.Name("modulepreload")
    r"""Refers to a module that the user agent is to preemptively fetch and store for use in the current context. [HTML]"""

    MONITOR = hydra.core.Name("monitor")
    r"""Refers to a resource that can be used to monitor changes in an HTTP resource. [RFC5989]"""

    MONITOR_GROUP = hydra.core.Name("monitor-group")
    r"""Refers to a resource that can be used to monitor changes in a specified group of HTTP resources. [RFC5989]"""

    NEXT = hydra.core.Name("next")
    r"""Indicates that the link's context is a part of a series, and that the next in the series is the link target. [HTML]"""

    NEXT_ARCHIVE = hydra.core.Name("next-archive")
    r"""Refers to the immediately following archive resource. [RFC5005]"""

    NOFOLLOW = hydra.core.Name("nofollow")
    r"""Indicates that the context's original author or publisher does not endorse the link target. [HTML]"""

    NOOPENER = hydra.core.Name("noopener")
    r"""Indicates that any newly created top-level browsing context which results from following the link will not be an auxiliary browsing context. [HTML]"""

    NOREFERRER = hydra.core.Name("noreferrer")
    r"""Indicates that no referrer information is to be leaked when following the link. [HTML]"""

    OPENER = hydra.core.Name("opener")
    r"""Indicates that any newly created top-level browsing context which results from following the link will be an auxiliary browsing context. [HTML]"""

    LOCAL_ID = hydra.core.Name("openid2.local_id")
    r"""Refers to an OpenID Authentication server on which the context relies for an assertion that the end user controls an Identifier. [OpenID Authentication 2.0 - Final]"""

    PROVIDER = hydra.core.Name("openid2.provider")
    r"""Refers to a resource which accepts OpenID Authentication protocol messages for the context. [OpenID Authentication 2.0 - Final]"""

    ORIGINAL = hydra.core.Name("original")
    r"""The Target IRI points to an Original Resource. [RFC7089] An Original Resource is a resource that exists or used to exist, and for which access to one of its prior states may be required."""

    P3PV1 = hydra.core.Name("p3pv1")
    r"""Refers to a P3P privacy policy for the context. [The Platform for Privacy Preferences 1.0 (P3P1.0) Specification]"""

    PAYMENT = hydra.core.Name("payment")
    r"""Indicates a resource where payment is accepted. [RFC8288] This relation type registration did not indicate a reference. Requested by Joshua Kinberg and Robert Sayre. It is meant as a general way to facilitate acts of payment, and thus this specification makes no assumptions on the type of payment or transaction protocol. Examples may include a web page where donations are accepted or where goods and services are available for purchase. rel="payment" is not intended to initiate an automated transaction. In Atom documents, a link element with a rel="payment" attribute may exist at the feed/channel level and/or the entry/item level. For example, a rel="payment" link at the feed/channel level may point to a "tip jar" URI, whereas an entry/ item containing a book review may include a rel="payment" link that points to the location where the book may be purchased through an online retailer."""

    PINGBACK = hydra.core.Name("pingback")
    r"""Gives the address of the pingback resource for the link context. [Pingback 1.0]"""

    PRECONNECT = hydra.core.Name("preconnect")
    r"""Used to indicate an origin that will be used to fetch required resources for the link context. Initiating an early connection, which includes the DNS lookup, TCP handshake, and optional TLS negotiation, allows the user agent to mask the high latency costs of establishing a connection. [Resource Hints]"""

    PREDECESSOR_VERSION = hydra.core.Name("predecessor-version")
    r"""Points to a resource containing the predecessor version in the version history. [RFC5829]"""

    PREFETCH = hydra.core.Name("prefetch")
    r"""The prefetch link relation type is used to identify a resource that might be required by the next navigation from the link context, and that the user agent ought to fetch, such that the user agent can deliver a faster response once the resource is requested in the future. [Resource Hints]"""

    PRELOAD = hydra.core.Name("preload")
    r"""Refers to a resource that should be loaded early in the processing of the link's context, without blocking rendering. [Preload] Additional target attributes establish the detailed fetch properties of the link."""

    PRERENDER = hydra.core.Name("prerender")
    r"""Used to identify a resource that might be required by the next navigation from the link context, and that the user agent ought to fetch and execute, such that the user agent can deliver a faster response once the resource is requested in the future. [Resource Hints]"""

    PREV = hydra.core.Name("prev")
    r"""Indicates that the link's context is a part of a series, and that the previous in the series is the link target. [HTML]"""

    PREVIEW = hydra.core.Name("preview")
    r"""Refers to a resource that provides a preview of the link's context. [RFC6903], section 3"""

    PREVIOUS = hydra.core.Name("previous")
    r"""Refers to the previous resource in an ordered series of resources. Synonym for "prev". [HTML 4.01 Specification]"""

    PREV_ARCHIVE = hydra.core.Name("prev-archive")
    r"""Refers to the immediately preceding archive resource. [RFC5005]"""

    PRIVACY_POLICY = hydra.core.Name("privacy-policy")
    r"""Refers to a privacy policy associated with the link's context. [RFC6903], section 4"""

    PROFILE = hydra.core.Name("profile")
    r"""Identifying that a resource representation conforms to a certain profile, without affecting the non-profile semantics of the resource representation. [RFC6906] Profile URIs are primarily intended to be used as identifiers, and thus clients SHOULD NOT indiscriminately access profile URIs."""

    PUBLICATION = hydra.core.Name("publication")
    r"""Links to a publication manifest. A manifest represents structured information about a publication, such as informative metadata, a list of resources, and a default reading order. [Publication Manifest]"""

    RELATED = hydra.core.Name("related")
    r"""Identifies a related resource. [RFC4287]"""

    RESTCONF = hydra.core.Name("restconf")
    r"""Identifies the root of RESTCONF API as configured on this HTTP server. The "restconf" relation defines the root of the API defined in RFC8040. Subsequent revisions of RESTCONF will use alternate relation values to support protocol versioning. [RFC8040]"""

    REPLIES = hydra.core.Name("replies")
    r"""Identifies a resource that is a reply to the context of the link. [RFC4685]"""

    RULEINPUT = hydra.core.Name("ruleinput")
    r"""The resource identified by the link target provides an input value to an instance of a rule, where the resource which represents the rule instance is identified by the link context. [OCF Core Optional 2.2.0]"""

    SEARCH = hydra.core.Name("search")
    r"""Refers to a resource that can be used to search through the link's context and related resources. [OpenSearch]"""

    SECTION = hydra.core.Name("section")
    r"""Refers to a section in a collection of resources. [HTML 4.01 Specification]"""

    SELF = hydra.core.Name("self")
    r"""Conveys an identifier for the link's context. [RFC4287]"""

    SERVICE = hydra.core.Name("service")
    r"""Indicates a URI that can be used to retrieve a service document. [RFC5023] When used in an Atom document, this relation type specifies Atom Publishing Protocol service documents by default. Requested by James Snell."""

    SERVICE_DESC = hydra.core.Name("service-desc")
    r"""Identifies service description for the context that is primarily intended for consumption by machines. [RFC8631]"""

    SERVICE_DOC = hydra.core.Name("service-doc")
    r"""Identifies service documentation for the context that is primarily intended for human consumption. [RFC8631]"""

    SERVICE_META = hydra.core.Name("service-meta")
    r"""Identifies general metadata for the context that is primarily intended for consumption by machines. [RFC8631]"""

    SIPTRUNKINGCAPABILITY = hydra.core.Name("siptrunkingcapability")
    r"""refers to a capability document that defines parameters or configuration requirements for automated peering and communication channel negotiation of the Session Initiation Protocol (SIP). [draft-engi-siptrunkingcapability-link]"""

    SPONSORED = hydra.core.Name("sponsored")
    r"""Refers to a resource that is within a context that is sponsored (such as advertising or another compensation agreement). [Google Blog post 09-2019]"""

    START = hydra.core.Name("start")
    r"""Refers to the first resource in a collection of resources. [HTML 4.01 Specification]"""

    STATUS = hydra.core.Name("status")
    r"""Identifies a resource that represents the context's status. [RFC8631]"""

    STYLESHEET = hydra.core.Name("stylesheet")
    r"""Refers to a stylesheet. [HTML]"""

    SUBSECTION = hydra.core.Name("subsection")
    r"""Refers to a resource serving as a subsection in a collection of resources. [HTML 4.01 Specification]"""

    SUCCESSOR_VERSION = hydra.core.Name("successor-version")
    r"""Points to a resource containing the successor version in the version history. [RFC5829]"""

    SUNSET = hydra.core.Name("sunset")
    r"""Identifies a resource that provides information about the context's retirement policy. [RFC8594]"""

    TAG = hydra.core.Name("tag")
    r"""Gives a tag (identified by the given address) that applies to the current document. [HTML]"""

    TERMS_OF_SERVICE = hydra.core.Name("terms-of-service")
    r"""Refers to the terms of service associated with the link's context. [RFC6903], section 5"""

    TIMEGATE = hydra.core.Name("timegate")
    r"""The Target IRI points to a TimeGate for an Original Resource. [RFC7089] A TimeGate for an Original Resource is a resource that is capable of datetime negotiation to support access to prior states of the Original Resource."""

    TIMEMAP = hydra.core.Name("timemap")
    r"""The Target IRI points to a TimeMap for an Original Resource. [RFC7089] A TimeMap for an Original Resource is a resource from which a list of URIs of Mementos of the Original Resource is available."""

    TYPE = hydra.core.Name("type")
    r"""Refers to a resource identifying the abstract semantic type of which the link's context is considered to be an instance. [RFC6903], section 6"""

    UGC = hydra.core.Name("ugc")
    r"""Refers to a resource that is within a context that is User Generated Content. [Google Blog post 09-2019]"""

    UP = hydra.core.Name("up")
    r"""Refers to a parent document in a hierarchy of documents. [RFC8288] This relation type registration did not indicate a reference. Requested by Noah Slater."""

    VERSION_HISTORY = hydra.core.Name("version-history")
    r"""Points to a resource containing the version history for the context. [RFC5829]"""

    VIA = hydra.core.Name("via")
    r"""Identifies a resource that is the source of the information in the link's context. [RFC4287]"""

    WEBMENTION = hydra.core.Name("webmention")
    r"""Identifies a target URI that supports the Webmention protocol. This allows clients that mention a resource in some form of publishing process to contact that endpoint and inform it that this resource has been mentioned. [Webmention] This is a similar "Linkback" mechanism to the ones of Refback, Trackback, and Pingback. It uses a different protocol, though, and thus should be discoverable through its own link relation type."""

    WORKING_COPY = hydra.core.Name("working-copy")
    r"""Points to a working copy for this resource. [RFC5829]"""

    WORKING_COPY_OF = hydra.core.Name("working-copy-of")
    r"""Points to the versioned resource from which this working copy was obtained. [RFC5829]"""

LinkRelationType.TYPE_ = hydra.core.Name("hydra.iana.linkrelations.LinkRelationType")
