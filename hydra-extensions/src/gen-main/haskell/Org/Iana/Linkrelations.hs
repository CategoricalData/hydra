-- | All IANA Link Relation Types, as of 2022-10-11. See https://www.iana.org/assignments/link-relations/link-relations.xhtml

module Org.Iana.Linkrelations where

import qualified Hydra.Core as Core
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

-- | An IANA link relation type
data LinkRelationType = 
  -- | Refers to a resource that is the subject of the link's context. [RFC6903], section 2
  LinkRelationTypeAbout  |
  -- | Asserts that the link target provides an access control resource for the link context. [https://solidproject.org/TR/wac#acl-link-relation]
  LinkRelationTypeAcl  |
  -- | Refers to a substitute for this context [HTML]
  LinkRelationTypeAlternate  |
  -- | Used to reference alternative content that uses the AMP profile of the HTML format. [AMP HTML]
  LinkRelationTypeAmphtml  |
  -- | Refers to an appendix. [HTML 4.01 Specification]
  LinkRelationTypeAppendix  |
  -- | Refers to an icon for the context. Synonym for icon. [Configuring Web Applications]
  LinkRelationTypeApple_touch_icon  |
  -- | Refers to a launch screen for the context. [Configuring Web Applications]
  LinkRelationTypeApple_touch_startup_image  |
  -- | Refers to a collection of records, documents, or other materials of historical interest. [HTML5]
  LinkRelationTypeArchives  |
  -- | Refers to the context's author. [HTML]
  LinkRelationTypeAuthor  |
  -- | Identifies the entity that blocks access to a resource following receipt of a legal demand. [RFC7725]
  LinkRelationTypeBlocked_by  |
  -- | Gives a permanent link to use for bookmarking purposes. [HTML]
  LinkRelationTypeBookmark  |
  -- | Designates the preferred version of a resource (the IRI and its contents). [RFC6596]
  LinkRelationTypeCanonical  |
  -- | Refers to a chapter in a collection of resources. [HTML 4.01 Specification]
  LinkRelationTypeChapter  |
  -- | Indicates that the link target is preferred over the link context for the purpose of permanent citation. [RFC8574]
  LinkRelationTypeCite_as  |
  -- | The target IRI points to a resource which represents the collection resource for the context IRI. [RFC6573]
  LinkRelationTypeCollection  |
  -- | Refers to a table of contents. [HTML 4.01 Specification]
  LinkRelationTypeContents  |
  -- | The document linked to was later converted to the document that contains this link relation. For example, an RFC can have a link to the Internet-Draft that became the RFC; in that case, the link relation would be "convertedFrom". [RFC7991] This relation is different than "predecessor-version" in that "predecessor-version" is for items in a version control system. It is also different than "previous" in that this relation is used for converted resources, not those that are part of a sequence of resources.
  LinkRelationTypeConvertedfrom  |
  -- | Refers to a copyright statement that applies to the link's context. [HTML 4.01 Specification]
  LinkRelationTypeCopyright  |
  -- | The target IRI points to a resource where a submission form can be obtained. [RFC6861]
  LinkRelationTypeCreate_form  |
  -- | Refers to a resource containing the most recent item(s) in a collection of resources. [RFC5005]
  LinkRelationTypeCurrent  |
  -- | Refers to a resource providing information about the link's context. [Protocol for Web Description Resources (POWDER)]
  LinkRelationTypeDescribedby  |
  -- | The relationship A 'describes' B asserts that resource A provides a description of resource B. There are no constraints on the format or representation of either A or B, neither are there any further constraints on either resource. [RFC6892] This link relation type is the inverse of the 'describedby' relation type. While 'describedby' establishes a relation from the described resource back to the resource that describes it, 'describes' established a relation from the describing resource to the resource it describes. If B is 'describedby' A, then A 'describes' B.
  LinkRelationTypeDescribes  |
  -- | Refers to a list of patent disclosures made with respect to material for which 'disclosure' relation is specified. [RFC6579]
  LinkRelationTypeDisclosure  |
  -- | Used to indicate an origin that will be used to fetch required resources for the link context, and that the user agent ought to resolve as early as possible. [Resource Hints]
  LinkRelationTypeDns_prefetch  |
  -- | Refers to a resource whose available representations are byte-for-byte identical with the corresponding representations of the context IRI. [RFC6249] This relation is for static resources. That is, an HTTP GET request on any duplicate will return the same representation. It does not make sense for dynamic or POSTable resources and should not be used for them.
  LinkRelationTypeDuplicate  |
  -- | Refers to a resource that can be used to edit the link's context. [RFC5023]
  LinkRelationTypeEdit  |
  -- | The target IRI points to a resource where a submission form for editing associated resource can be obtained. [RFC6861]
  LinkRelationTypeEdit_form  |
  -- | Refers to a resource that can be used to edit media associated with the link's context. [RFC5023]
  LinkRelationTypeEdit_media  |
  -- | Identifies a related resource that is potentially large and might require special handling. [RFC4287]
  LinkRelationTypeEnclosure  |
  -- | Refers to a resource that is not part of the same site as the current context. [HTML]
  LinkRelationTypeExternal  |
  -- | An IRI that refers to the furthest preceding resource in a series of resources. [RFC8288] This relation type registration did not indicate a reference. Originally requested by Mark Nottingham in December 2004.
  LinkRelationTypeFirst  |
  -- | Refers to a glossary of terms. [HTML 4.01 Specification]
  LinkRelationTypeGlossary  |
  -- | Refers to context-sensitive help. [HTML]
  LinkRelationTypeHelp  |
  -- | Refers to a resource hosted by the server indicated by the link context. [RFC6690] This relation is used in CoRE where links are retrieved as a "/.well-known/core" resource representation, and is the default relation type in the CoRE Link Format.
  LinkRelationTypeHosts  |
  -- | Refers to a hub that enables registration for notification of updates to the context. [WebSub] This relation type was requested by Brett Slatkin.
  LinkRelationTypeHub  |
  -- | Refers to an icon representing the link's context. [HTML]
  LinkRelationTypeIcon  |
  -- | Refers to an index. [HTML 4.01 Specification]
  LinkRelationTypeIndex  |
  -- | refers to a resource associated with a time interval that ends before the beginning of the time interval associated with the context resource [Time Ontology in OWL] section 4.2.21
  LinkRelationTypeIntervalafter  |
  -- | refers to a resource associated with a time interval that begins after the end of the time interval associated with the context resource [Time Ontology in OWL] section 4.2.22
  LinkRelationTypeIntervalbefore  |
  -- | refers to a resource associated with a time interval that begins after the beginning of the time interval associated with the context resource, and ends before the end of the time interval associated with the context resource [Time Ontology in OWL] section 4.2.23
  LinkRelationTypeIntervalcontains  |
  -- | refers to a resource associated with a time interval that begins after the end of the time interval associated with the context resource, or ends before the beginning of the time interval associated with the context resource [Time Ontology in OWL] section 4.2.24
  LinkRelationTypeIntervaldisjoint  |
  -- | refers to a resource associated with a time interval that begins before the beginning of the time interval associated with the context resource, and ends after the end of the time interval associated with the context resource [Time Ontology in OWL] section 4.2.25
  LinkRelationTypeIntervalduring  |
  -- | refers to a resource associated with a time interval whose beginning coincides with the beginning of the time interval associated with the context resource, and whose end coincides with the end of the time interval associated with the context resource [Time Ontology in OWL] section 4.2.26
  LinkRelationTypeIntervalequals  |
  -- | refers to a resource associated with a time interval that begins after the beginning of the time interval associated with the context resource, and whose end coincides with the end of the time interval associated with the context resource [Time Ontology in OWL] section 4.2.27
  LinkRelationTypeIntervalfinishedby  |
  -- | refers to a resource associated with a time interval that begins before the beginning of the time interval associated with the context resource, and whose end coincides with the end of the time interval associated with the context resource [Time Ontology in OWL] section 4.2.28
  LinkRelationTypeIntervalfinishes  |
  -- | refers to a resource associated with a time interval that begins before or is coincident with the beginning of the time interval associated with the context resource, and ends after or is coincident with the end of the time interval associated with the context resource [Time Ontology in OWL] section 4.2.29
  LinkRelationTypeIntervalin  |
  -- | refers to a resource associated with a time interval whose beginning coincides with the end of the time interval associated with the context resource [Time Ontology in OWL] section 4.2.30
  LinkRelationTypeIntervalmeets  |
  -- | refers to a resource associated with a time interval whose end coincides with the beginning of the time interval associated with the context resource [Time Ontology in OWL] section 4.2.31
  LinkRelationTypeIntervalmetby  |
  -- | refers to a resource associated with a time interval that begins before the beginning of the time interval associated with the context resource, and ends after the beginning of the time interval associated with the context resource [Time Ontology in OWL] section 4.2.32
  LinkRelationTypeIntervaloverlappedby  |
  -- | refers to a resource associated with a time interval that begins before the end of the time interval associated with the context resource, and ends after the end of the time interval associated with the context resource [Time Ontology in OWL] section 4.2.33
  LinkRelationTypeIntervaloverlaps  |
  -- | refers to a resource associated with a time interval whose beginning coincides with the beginning of the time interval associated with the context resource, and ends before the end of the time interval associated with the context resource [Time Ontology in OWL] section 4.2.34
  LinkRelationTypeIntervalstartedby  |
  -- | refers to a resource associated with a time interval whose beginning coincides with the beginning of the time interval associated with the context resource, and ends after the end of the time interval associated with the context resource [Time Ontology in OWL] section 4.2.35
  LinkRelationTypeIntervalstarts  |
  -- | The target IRI points to a resource that is a member of the collection represented by the context IRI. [RFC6573]
  LinkRelationTypeItem  |
  -- | An IRI that refers to the furthest following resource in a series of resources. [RFC8288] This relation type registration did not indicate a reference. Originally requested by Mark Nottingham in December 2004.
  LinkRelationTypeLast  |
  -- | Points to a resource containing the latest (e.g., current) version of the context. [RFC5829]
  LinkRelationTypeLatest_version  |
  -- | Refers to a license associated with this context. [RFC4946] For implications of use in HTML, see: http://www.w3.org/TR/html5/links.html#link-type-license
  LinkRelationTypeLicense  |
  -- | The link target of a link with the "linkset" relation type provides a set of links, including links in which the link context of the link participates. [RFC9264]
  LinkRelationTypeLinkset  |
  -- | Refers to further information about the link's context, expressed as a LRDD ("Link-based Resource Descriptor Document") resource. See [RFC6415] for information about processing this relation type in host-meta documents. When used elsewhere, it refers to additional links and other metadata. Multiple instances indicate additional LRDD resources. LRDD resources MUST have an "application/xrd+xml" representation, and MAY have others. [RFC6415]
  LinkRelationTypeLrdd  |
  -- | Links to a manifest file for the context. [Web App Manifest]
  LinkRelationTypeManifest  |
  -- | Refers to a mask that can be applied to the icon for the context. [Creating Pinned Tab Icons]
  LinkRelationTypeMask_icon  |
  -- | Refers to a feed of personalised media recommendations relevant to the link context. [https://wicg.github.io/media-feeds/#discovery-of-media-feeds]
  LinkRelationTypeMedia_feed  |
  -- | The Target IRI points to a Memento, a fixed resource that will not change state anymore. [RFC7089] A Memento for an Original Resource is a resource that encapsulates a prior state of the Original Resource.
  LinkRelationTypeMemento  |
  -- | Links to the context's Micropub endpoint. [Micropub]
  LinkRelationTypeMicropub  |
  -- | Refers to a module that the user agent is to preemptively fetch and store for use in the current context. [HTML]
  LinkRelationTypeModulepreload  |
  -- | Refers to a resource that can be used to monitor changes in an HTTP resource. [RFC5989]
  LinkRelationTypeMonitor  |
  -- | Refers to a resource that can be used to monitor changes in a specified group of HTTP resources. [RFC5989]
  LinkRelationTypeMonitor_group  |
  -- | Indicates that the link's context is a part of a series, and that the next in the series is the link target. [HTML]
  LinkRelationTypeNext  |
  -- | Refers to the immediately following archive resource. [RFC5005]
  LinkRelationTypeNext_archive  |
  -- | Indicates that the context’s original author or publisher does not endorse the link target. [HTML]
  LinkRelationTypeNofollow  |
  -- | Indicates that any newly created top-level browsing context which results from following the link will not be an auxiliary browsing context. [HTML]
  LinkRelationTypeNoopener  |
  -- | Indicates that no referrer information is to be leaked when following the link. [HTML]
  LinkRelationTypeNoreferrer  |
  -- | Indicates that any newly created top-level browsing context which results from following the link will be an auxiliary browsing context. [HTML]
  LinkRelationTypeOpener  |
  -- | Refers to an OpenID Authentication server on which the context relies for an assertion that the end user controls an Identifier. [OpenID Authentication 2.0 - Final]
  LinkRelationTypeOpenid2_local_id  |
  -- | Refers to a resource which accepts OpenID Authentication protocol messages for the context. [OpenID Authentication 2.0 - Final]
  LinkRelationTypeOpenid2_provider  |
  -- | The Target IRI points to an Original Resource. [RFC7089] An Original Resource is a resource that exists or used to exist, and for which access to one of its prior states may be required.
  LinkRelationTypeOriginal  |
  -- | Refers to a P3P privacy policy for the context. [The Platform for Privacy Preferences 1.0 (P3P1.0) Specification]
  LinkRelationTypeP3pv1  |
  -- | Indicates a resource where payment is accepted. [RFC8288] This relation type registration did not indicate a reference. Requested by Joshua Kinberg and Robert Sayre. It is meant as a general way to facilitate acts of payment, and thus this specification makes no assumptions on the type of payment or transaction protocol. Examples may include a web page where donations are accepted or where goods and services are available for purchase. rel="payment" is not intended to initiate an automated transaction. In Atom documents, a link element with a rel="payment" attribute may exist at the feed/channel level and/or the entry/item level. For example, a rel="payment" link at the feed/channel level may point to a "tip jar" URI, whereas an entry/ item containing a book review may include a rel="payment" link that points to the location where the book may be purchased through an online retailer.
  LinkRelationTypePayment  |
  -- | Gives the address of the pingback resource for the link context. [Pingback 1.0]
  LinkRelationTypePingback  |
  -- | Used to indicate an origin that will be used to fetch required resources for the link context. Initiating an early connection, which includes the DNS lookup, TCP handshake, and optional TLS negotiation, allows the user agent to mask the high latency costs of establishing a connection. [Resource Hints]
  LinkRelationTypePreconnect  |
  -- | Points to a resource containing the predecessor version in the version history. [RFC5829]
  LinkRelationTypePredecessor_version  |
  -- | The prefetch link relation type is used to identify a resource that might be required by the next navigation from the link context, and that the user agent ought to fetch, such that the user agent can deliver a faster response once the resource is requested in the future. [Resource Hints]
  LinkRelationTypePrefetch  |
  -- | Refers to a resource that should be loaded early in the processing of the link's context, without blocking rendering. [Preload] Additional target attributes establish the detailed fetch properties of the link.
  LinkRelationTypePreload  |
  -- | Used to identify a resource that might be required by the next navigation from the link context, and that the user agent ought to fetch and execute, such that the user agent can deliver a faster response once the resource is requested in the future. [Resource Hints]
  LinkRelationTypePrerender  |
  -- | Indicates that the link's context is a part of a series, and that the previous in the series is the link target. [HTML]
  LinkRelationTypePrev  |
  -- | Refers to a resource that provides a preview of the link's context. [RFC6903], section 3
  LinkRelationTypePreview  |
  -- | Refers to the previous resource in an ordered series of resources. Synonym for "prev". [HTML 4.01 Specification]
  LinkRelationTypePrevious  |
  -- | Refers to the immediately preceding archive resource. [RFC5005]
  LinkRelationTypePrev_archive  |
  -- | Refers to a privacy policy associated with the link's context. [RFC6903], section 4
  LinkRelationTypePrivacy_policy  |
  -- | Identifying that a resource representation conforms to a certain profile, without affecting the non-profile semantics of the resource representation. [RFC6906] Profile URIs are primarily intended to be used as identifiers, and thus clients SHOULD NOT indiscriminately access profile URIs.
  LinkRelationTypeProfile  |
  -- | Links to a publication manifest. A manifest represents structured information about a publication, such as informative metadata, a list of resources, and a default reading order. [Publication Manifest]
  LinkRelationTypePublication  |
  -- | Identifies a related resource. [RFC4287]
  LinkRelationTypeRelated  |
  -- | Identifies the root of RESTCONF API as configured on this HTTP server. The "restconf" relation defines the root of the API defined in RFC8040. Subsequent revisions of RESTCONF will use alternate relation values to support protocol versioning. [RFC8040]
  LinkRelationTypeRestconf  |
  -- | Identifies a resource that is a reply to the context of the link. [RFC4685]
  LinkRelationTypeReplies  |
  -- | The resource identified by the link target provides an input value to an instance of a rule, where the resource which represents the rule instance is identified by the link context. [OCF Core Optional 2.2.0]
  LinkRelationTypeRuleinput  |
  -- | Refers to a resource that can be used to search through the link's context and related resources. [OpenSearch]
  LinkRelationTypeSearch  |
  -- | Refers to a section in a collection of resources. [HTML 4.01 Specification]
  LinkRelationTypeSection  |
  -- | Conveys an identifier for the link's context. [RFC4287]
  LinkRelationTypeSelf  |
  -- | Indicates a URI that can be used to retrieve a service document. [RFC5023] When used in an Atom document, this relation type specifies Atom Publishing Protocol service documents by default. Requested by James Snell.
  LinkRelationTypeService  |
  -- | Identifies service description for the context that is primarily intended for consumption by machines. [RFC8631]
  LinkRelationTypeService_desc  |
  -- | Identifies service documentation for the context that is primarily intended for human consumption. [RFC8631]
  LinkRelationTypeService_doc  |
  -- | Identifies general metadata for the context that is primarily intended for consumption by machines. [RFC8631]
  LinkRelationTypeService_meta  |
  -- | refers to a capability document that defines parameters or configuration requirements for automated peering and communication channel negotiation of the Session Initiation Protocol (SIP). [draft-engi-siptrunkingcapability-link]
  LinkRelationTypeSiptrunkingcapability  |
  -- | Refers to a resource that is within a context that is sponsored (such as advertising or another compensation agreement). [Google Blog post 09-2019]
  LinkRelationTypeSponsored  |
  -- | Refers to the first resource in a collection of resources. [HTML 4.01 Specification]
  LinkRelationTypeStart  |
  -- | Identifies a resource that represents the context's status. [RFC8631]
  LinkRelationTypeStatus  |
  -- | Refers to a stylesheet. [HTML]
  LinkRelationTypeStylesheet  |
  -- | Refers to a resource serving as a subsection in a collection of resources. [HTML 4.01 Specification]
  LinkRelationTypeSubsection  |
  -- | Points to a resource containing the successor version in the version history. [RFC5829]
  LinkRelationTypeSuccessor_version  |
  -- | Identifies a resource that provides information about the context's retirement policy. [RFC8594]
  LinkRelationTypeSunset  |
  -- | Gives a tag (identified by the given address) that applies to the current document. [HTML]
  LinkRelationTypeTag  |
  -- | Refers to the terms of service associated with the link's context. [RFC6903], section 5
  LinkRelationTypeTerms_of_service  |
  -- | The Target IRI points to a TimeGate for an Original Resource. [RFC7089] A TimeGate for an Original Resource is a resource that is capable of datetime negotiation to support access to prior states of the Original Resource.
  LinkRelationTypeTimegate  |
  -- | The Target IRI points to a TimeMap for an Original Resource. [RFC7089] A TimeMap for an Original Resource is a resource from which a list of URIs of Mementos of the Original Resource is available.
  LinkRelationTypeTimemap  |
  -- | Refers to a resource identifying the abstract semantic type of which the link's context is considered to be an instance. [RFC6903], section 6
  LinkRelationTypeType  |
  -- | Refers to a resource that is within a context that is User Generated Content. [Google Blog post 09-2019]
  LinkRelationTypeUgc  |
  -- | Refers to a parent document in a hierarchy of documents. [RFC8288] This relation type registration did not indicate a reference. Requested by Noah Slater.
  LinkRelationTypeUp  |
  -- | Points to a resource containing the version history for the context. [RFC5829]
  LinkRelationTypeVersion_history  |
  -- | Identifies a resource that is the source of the information in the link's context. [RFC4287]
  LinkRelationTypeVia  |
  -- | Identifies a target URI that supports the Webmention protocol. This allows clients that mention a resource in some form of publishing process to contact that endpoint and inform it that this resource has been mentioned. [Webmention] This is a similar "Linkback" mechanism to the ones of Refback, Trackback, and Pingback. It uses a different protocol, though, and thus should be discoverable through its own link relation type.
  LinkRelationTypeWebmention  |
  -- | Points to a working copy for this resource. [RFC5829]
  LinkRelationTypeWorking_copy  |
  -- | Points to the versioned resource from which this working copy was obtained. [RFC5829]
  LinkRelationTypeWorking_copy_of 
  deriving (Eq, Ord, Read, Show)

_LinkRelationType = (Core.Name "org/iana/linkrelations.LinkRelationType")

_LinkRelationType_about = (Core.FieldName "about")

_LinkRelationType_acl = (Core.FieldName "acl")

_LinkRelationType_alternate = (Core.FieldName "alternate")

_LinkRelationType_amphtml = (Core.FieldName "amphtml")

_LinkRelationType_appendix = (Core.FieldName "appendix")

_LinkRelationType_apple_touch_icon = (Core.FieldName "apple-touch-icon")

_LinkRelationType_apple_touch_startup_image = (Core.FieldName "apple-touch-startup-image")

_LinkRelationType_archives = (Core.FieldName "archives")

_LinkRelationType_author = (Core.FieldName "author")

_LinkRelationType_blocked_by = (Core.FieldName "blocked-by")

_LinkRelationType_bookmark = (Core.FieldName "bookmark")

_LinkRelationType_canonical = (Core.FieldName "canonical")

_LinkRelationType_chapter = (Core.FieldName "chapter")

_LinkRelationType_cite_as = (Core.FieldName "cite-as")

_LinkRelationType_collection = (Core.FieldName "collection")

_LinkRelationType_contents = (Core.FieldName "contents")

_LinkRelationType_convertedfrom = (Core.FieldName "convertedfrom")

_LinkRelationType_copyright = (Core.FieldName "copyright")

_LinkRelationType_create_form = (Core.FieldName "create-form")

_LinkRelationType_current = (Core.FieldName "current")

_LinkRelationType_describedby = (Core.FieldName "describedby")

_LinkRelationType_describes = (Core.FieldName "describes")

_LinkRelationType_disclosure = (Core.FieldName "disclosure")

_LinkRelationType_dns_prefetch = (Core.FieldName "dns-prefetch")

_LinkRelationType_duplicate = (Core.FieldName "duplicate")

_LinkRelationType_edit = (Core.FieldName "edit")

_LinkRelationType_edit_form = (Core.FieldName "edit-form")

_LinkRelationType_edit_media = (Core.FieldName "edit-media")

_LinkRelationType_enclosure = (Core.FieldName "enclosure")

_LinkRelationType_external = (Core.FieldName "external")

_LinkRelationType_first = (Core.FieldName "first")

_LinkRelationType_glossary = (Core.FieldName "glossary")

_LinkRelationType_help = (Core.FieldName "help")

_LinkRelationType_hosts = (Core.FieldName "hosts")

_LinkRelationType_hub = (Core.FieldName "hub")

_LinkRelationType_icon = (Core.FieldName "icon")

_LinkRelationType_index = (Core.FieldName "index")

_LinkRelationType_intervalafter = (Core.FieldName "intervalafter")

_LinkRelationType_intervalbefore = (Core.FieldName "intervalbefore")

_LinkRelationType_intervalcontains = (Core.FieldName "intervalcontains")

_LinkRelationType_intervaldisjoint = (Core.FieldName "intervaldisjoint")

_LinkRelationType_intervalduring = (Core.FieldName "intervalduring")

_LinkRelationType_intervalequals = (Core.FieldName "intervalequals")

_LinkRelationType_intervalfinishedby = (Core.FieldName "intervalfinishedby")

_LinkRelationType_intervalfinishes = (Core.FieldName "intervalfinishes")

_LinkRelationType_intervalin = (Core.FieldName "intervalin")

_LinkRelationType_intervalmeets = (Core.FieldName "intervalmeets")

_LinkRelationType_intervalmetby = (Core.FieldName "intervalmetby")

_LinkRelationType_intervaloverlappedby = (Core.FieldName "intervaloverlappedby")

_LinkRelationType_intervaloverlaps = (Core.FieldName "intervaloverlaps")

_LinkRelationType_intervalstartedby = (Core.FieldName "intervalstartedby")

_LinkRelationType_intervalstarts = (Core.FieldName "intervalstarts")

_LinkRelationType_item = (Core.FieldName "item")

_LinkRelationType_last = (Core.FieldName "last")

_LinkRelationType_latest_version = (Core.FieldName "latest-version")

_LinkRelationType_license = (Core.FieldName "license")

_LinkRelationType_linkset = (Core.FieldName "linkset")

_LinkRelationType_lrdd = (Core.FieldName "lrdd")

_LinkRelationType_manifest = (Core.FieldName "manifest")

_LinkRelationType_mask_icon = (Core.FieldName "mask-icon")

_LinkRelationType_media_feed = (Core.FieldName "media-feed")

_LinkRelationType_memento = (Core.FieldName "memento")

_LinkRelationType_micropub = (Core.FieldName "micropub")

_LinkRelationType_modulepreload = (Core.FieldName "modulepreload")

_LinkRelationType_monitor = (Core.FieldName "monitor")

_LinkRelationType_monitor_group = (Core.FieldName "monitor-group")

_LinkRelationType_next = (Core.FieldName "next")

_LinkRelationType_next_archive = (Core.FieldName "next-archive")

_LinkRelationType_nofollow = (Core.FieldName "nofollow")

_LinkRelationType_noopener = (Core.FieldName "noopener")

_LinkRelationType_noreferrer = (Core.FieldName "noreferrer")

_LinkRelationType_opener = (Core.FieldName "opener")

_LinkRelationType_openid2_local_id = (Core.FieldName "openid2.local_id")

_LinkRelationType_openid2_provider = (Core.FieldName "openid2.provider")

_LinkRelationType_original = (Core.FieldName "original")

_LinkRelationType_p3pv1 = (Core.FieldName "p3pv1")

_LinkRelationType_payment = (Core.FieldName "payment")

_LinkRelationType_pingback = (Core.FieldName "pingback")

_LinkRelationType_preconnect = (Core.FieldName "preconnect")

_LinkRelationType_predecessor_version = (Core.FieldName "predecessor-version")

_LinkRelationType_prefetch = (Core.FieldName "prefetch")

_LinkRelationType_preload = (Core.FieldName "preload")

_LinkRelationType_prerender = (Core.FieldName "prerender")

_LinkRelationType_prev = (Core.FieldName "prev")

_LinkRelationType_preview = (Core.FieldName "preview")

_LinkRelationType_previous = (Core.FieldName "previous")

_LinkRelationType_prev_archive = (Core.FieldName "prev-archive")

_LinkRelationType_privacy_policy = (Core.FieldName "privacy-policy")

_LinkRelationType_profile = (Core.FieldName "profile")

_LinkRelationType_publication = (Core.FieldName "publication")

_LinkRelationType_related = (Core.FieldName "related")

_LinkRelationType_restconf = (Core.FieldName "restconf")

_LinkRelationType_replies = (Core.FieldName "replies")

_LinkRelationType_ruleinput = (Core.FieldName "ruleinput")

_LinkRelationType_search = (Core.FieldName "search")

_LinkRelationType_section = (Core.FieldName "section")

_LinkRelationType_self = (Core.FieldName "self")

_LinkRelationType_service = (Core.FieldName "service")

_LinkRelationType_service_desc = (Core.FieldName "service-desc")

_LinkRelationType_service_doc = (Core.FieldName "service-doc")

_LinkRelationType_service_meta = (Core.FieldName "service-meta")

_LinkRelationType_siptrunkingcapability = (Core.FieldName "siptrunkingcapability")

_LinkRelationType_sponsored = (Core.FieldName "sponsored")

_LinkRelationType_start = (Core.FieldName "start")

_LinkRelationType_status = (Core.FieldName "status")

_LinkRelationType_stylesheet = (Core.FieldName "stylesheet")

_LinkRelationType_subsection = (Core.FieldName "subsection")

_LinkRelationType_successor_version = (Core.FieldName "successor-version")

_LinkRelationType_sunset = (Core.FieldName "sunset")

_LinkRelationType_tag = (Core.FieldName "tag")

_LinkRelationType_terms_of_service = (Core.FieldName "terms-of-service")

_LinkRelationType_timegate = (Core.FieldName "timegate")

_LinkRelationType_timemap = (Core.FieldName "timemap")

_LinkRelationType_type = (Core.FieldName "type")

_LinkRelationType_ugc = (Core.FieldName "ugc")

_LinkRelationType_up = (Core.FieldName "up")

_LinkRelationType_version_history = (Core.FieldName "version-history")

_LinkRelationType_via = (Core.FieldName "via")

_LinkRelationType_webmention = (Core.FieldName "webmention")

_LinkRelationType_working_copy = (Core.FieldName "working-copy")

_LinkRelationType_working_copy_of = (Core.FieldName "working-copy-of")