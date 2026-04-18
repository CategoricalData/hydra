(ns hydra.osv.schema)

(declare hydra_osv_schema_event-variants)

(defrecord hydra_osv_schema_credited [name contact])
(defn make-hydra_osv_schema_credited [name contact] (->hydra_osv_schema_credited name contact))

(defrecord hydra_osv_schema_ecosystem [value])
(defn make-hydra_osv_schema_ecosystem [value] (->hydra_osv_schema_ecosystem value))

(defrecord hydra_osv_schema_entry [schema_version id modified published withdrawn aliases related summary details severity affected references credits])
(defn make-hydra_osv_schema_entry [schema_version id modified published withdrawn aliases related summary details severity affected references credits] (->hydra_osv_schema_entry schema_version id modified published withdrawn aliases related summary details severity affected references credits))

(def hydra_osv_schema_event-variants (list :introduced :fixed :last_affected :limit))

(defrecord hydra_osv_schema_id [value])
(defn make-hydra_osv_schema_id [value] (->hydra_osv_schema_id value))

(defrecord hydra_osv_schema_markdown [value])
(defn make-hydra_osv_schema_markdown [value] (->hydra_osv_schema_markdown value))

(defrecord hydra_osv_schema_osv_version [value])
(defn make-hydra_osv_schema_osv_version [value] (->hydra_osv_schema_osv_version value))

(defrecord hydra_osv_schema_package [ecosystem name purl])
(defn make-hydra_osv_schema_package [ecosystem name purl] (->hydra_osv_schema_package ecosystem name purl))

(defrecord hydra_osv_schema_package_versions [package ranges versions])
(defn make-hydra_osv_schema_package_versions [package ranges versions] (->hydra_osv_schema_package_versions package ranges versions))

(defrecord hydra_osv_schema_reference [type url])
(defn make-hydra_osv_schema_reference [type url] (->hydra_osv_schema_reference type url))

(defrecord hydra_osv_schema_reference_type [value])
(defn make-hydra_osv_schema_reference_type [value] (->hydra_osv_schema_reference_type value))

(defrecord hydra_osv_schema_severity [type score])
(defn make-hydra_osv_schema_severity [type score] (->hydra_osv_schema_severity type score))

(defrecord hydra_osv_schema_severity_score [value])
(defn make-hydra_osv_schema_severity_score [value] (->hydra_osv_schema_severity_score value))

(defrecord hydra_osv_schema_severity_type [value])
(defn make-hydra_osv_schema_severity_type [value] (->hydra_osv_schema_severity_type value))

(defrecord hydra_osv_schema_timestamp [value])
(defn make-hydra_osv_schema_timestamp [value] (->hydra_osv_schema_timestamp value))

(defrecord hydra_osv_schema_url [value])
(defn make-hydra_osv_schema_url [value] (->hydra_osv_schema_url value))

(defrecord hydra_osv_schema_version [value])
(defn make-hydra_osv_schema_version [value] (->hydra_osv_schema_version value))

(defrecord hydra_osv_schema_version_or_star [value])
(defn make-hydra_osv_schema_version_or_star [value] (->hydra_osv_schema_version_or_star value))

(defrecord hydra_osv_schema_version_or_zero [value])
(defn make-hydra_osv_schema_version_or_zero [value] (->hydra_osv_schema_version_or_zero value))

(defrecord hydra_osv_schema_version_range [type repo events])
(defn make-hydra_osv_schema_version_range [type repo events] (->hydra_osv_schema_version_range type repo events))

(defrecord hydra_osv_schema_version_type [value])
(defn make-hydra_osv_schema_version_type [value] (->hydra_osv_schema_version_type value))
