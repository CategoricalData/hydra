// Note: this is an automatically generated file. Do not edit.

package hydra.ext.dev.osv.schema;

import java.io.Serializable;

public class Entry implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.dev.osv.schema.Entry");
  
  public static final hydra.core.Name FIELD_NAME_SCHEMA_VERSION = new hydra.core.Name("schemaVersion");
  
  public static final hydra.core.Name FIELD_NAME_ID = new hydra.core.Name("id");
  
  public static final hydra.core.Name FIELD_NAME_MODIFIED = new hydra.core.Name("modified");
  
  public static final hydra.core.Name FIELD_NAME_PUBLISHED = new hydra.core.Name("published");
  
  public static final hydra.core.Name FIELD_NAME_WITHDRAWN = new hydra.core.Name("withdrawn");
  
  public static final hydra.core.Name FIELD_NAME_ALIASES = new hydra.core.Name("aliases");
  
  public static final hydra.core.Name FIELD_NAME_RELATED = new hydra.core.Name("related");
  
  public static final hydra.core.Name FIELD_NAME_SUMMARY = new hydra.core.Name("summary");
  
  public static final hydra.core.Name FIELD_NAME_DETAILS = new hydra.core.Name("details");
  
  public static final hydra.core.Name FIELD_NAME_SEVERITY = new hydra.core.Name("severity");
  
  public static final hydra.core.Name FIELD_NAME_AFFECTED = new hydra.core.Name("affected");
  
  public static final hydra.core.Name FIELD_NAME_REFERENCES = new hydra.core.Name("references");
  
  public static final hydra.core.Name FIELD_NAME_CREDITS = new hydra.core.Name("credits");
  
  /**
   * The default value is '1.0.0', matching version 1.0 of the OSV Schema
   */
  public final hydra.util.Opt<hydra.ext.dev.osv.schema.OsvVersion> schemaVersion;
  
  public final hydra.ext.dev.osv.schema.Id id;
  
  public final hydra.ext.dev.osv.schema.Timestamp modified;
  
  public final hydra.util.Opt<hydra.ext.dev.osv.schema.Timestamp> published;
  
  public final hydra.util.Opt<hydra.ext.dev.osv.schema.Timestamp> withdrawn;
  
  public final hydra.util.Opt<java.util.List<hydra.ext.dev.osv.schema.Id>> aliases;
  
  public final hydra.util.Opt<java.util.List<hydra.ext.dev.osv.schema.Id>> related;
  
  public final hydra.util.Opt<String> summary;
  
  public final hydra.util.Opt<hydra.ext.dev.osv.schema.Markdown> details;
  
  public final hydra.util.Opt<java.util.List<hydra.ext.dev.osv.schema.Severity>> severity;
  
  public final hydra.util.Opt<java.util.List<hydra.ext.dev.osv.schema.PackageVersions>> affected;
  
  public final hydra.util.Opt<java.util.List<hydra.ext.dev.osv.schema.Reference>> references;
  
  public final hydra.util.Opt<java.util.List<hydra.ext.dev.osv.schema.Credited>> credits;
  
  public Entry (hydra.util.Opt<hydra.ext.dev.osv.schema.OsvVersion> schemaVersion, hydra.ext.dev.osv.schema.Id id, hydra.ext.dev.osv.schema.Timestamp modified, hydra.util.Opt<hydra.ext.dev.osv.schema.Timestamp> published, hydra.util.Opt<hydra.ext.dev.osv.schema.Timestamp> withdrawn, hydra.util.Opt<java.util.List<hydra.ext.dev.osv.schema.Id>> aliases, hydra.util.Opt<java.util.List<hydra.ext.dev.osv.schema.Id>> related, hydra.util.Opt<String> summary, hydra.util.Opt<hydra.ext.dev.osv.schema.Markdown> details, hydra.util.Opt<java.util.List<hydra.ext.dev.osv.schema.Severity>> severity, hydra.util.Opt<java.util.List<hydra.ext.dev.osv.schema.PackageVersions>> affected, hydra.util.Opt<java.util.List<hydra.ext.dev.osv.schema.Reference>> references, hydra.util.Opt<java.util.List<hydra.ext.dev.osv.schema.Credited>> credits) {
    java.util.Objects.requireNonNull((schemaVersion));
    java.util.Objects.requireNonNull((id));
    java.util.Objects.requireNonNull((modified));
    java.util.Objects.requireNonNull((published));
    java.util.Objects.requireNonNull((withdrawn));
    java.util.Objects.requireNonNull((aliases));
    java.util.Objects.requireNonNull((related));
    java.util.Objects.requireNonNull((summary));
    java.util.Objects.requireNonNull((details));
    java.util.Objects.requireNonNull((severity));
    java.util.Objects.requireNonNull((affected));
    java.util.Objects.requireNonNull((references));
    java.util.Objects.requireNonNull((credits));
    this.schemaVersion = schemaVersion;
    this.id = id;
    this.modified = modified;
    this.published = published;
    this.withdrawn = withdrawn;
    this.aliases = aliases;
    this.related = related;
    this.summary = summary;
    this.details = details;
    this.severity = severity;
    this.affected = affected;
    this.references = references;
    this.credits = credits;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Entry)) {
      return false;
    }
    Entry o = (Entry) (other);
    return schemaVersion.equals(o.schemaVersion) && id.equals(o.id) && modified.equals(o.modified) && published.equals(o.published) && withdrawn.equals(o.withdrawn) && aliases.equals(o.aliases) && related.equals(o.related) && summary.equals(o.summary) && details.equals(o.details) && severity.equals(o.severity) && affected.equals(o.affected) && references.equals(o.references) && credits.equals(o.credits);
  }
  
  @Override
  public int hashCode() {
    return 2 * schemaVersion.hashCode() + 3 * id.hashCode() + 5 * modified.hashCode() + 7 * published.hashCode() + 11 * withdrawn.hashCode() + 13 * aliases.hashCode() + 17 * related.hashCode() + 19 * summary.hashCode() + 23 * details.hashCode() + 29 * severity.hashCode() + 31 * affected.hashCode() + 37 * references.hashCode() + 41 * credits.hashCode();
  }
  
  public Entry withSchemaVersion(hydra.util.Opt<hydra.ext.dev.osv.schema.OsvVersion> schemaVersion) {
    java.util.Objects.requireNonNull((schemaVersion));
    return new Entry(schemaVersion, id, modified, published, withdrawn, aliases, related, summary, details, severity, affected, references, credits);
  }
  
  public Entry withId(hydra.ext.dev.osv.schema.Id id) {
    java.util.Objects.requireNonNull((id));
    return new Entry(schemaVersion, id, modified, published, withdrawn, aliases, related, summary, details, severity, affected, references, credits);
  }
  
  public Entry withModified(hydra.ext.dev.osv.schema.Timestamp modified) {
    java.util.Objects.requireNonNull((modified));
    return new Entry(schemaVersion, id, modified, published, withdrawn, aliases, related, summary, details, severity, affected, references, credits);
  }
  
  public Entry withPublished(hydra.util.Opt<hydra.ext.dev.osv.schema.Timestamp> published) {
    java.util.Objects.requireNonNull((published));
    return new Entry(schemaVersion, id, modified, published, withdrawn, aliases, related, summary, details, severity, affected, references, credits);
  }
  
  public Entry withWithdrawn(hydra.util.Opt<hydra.ext.dev.osv.schema.Timestamp> withdrawn) {
    java.util.Objects.requireNonNull((withdrawn));
    return new Entry(schemaVersion, id, modified, published, withdrawn, aliases, related, summary, details, severity, affected, references, credits);
  }
  
  public Entry withAliases(hydra.util.Opt<java.util.List<hydra.ext.dev.osv.schema.Id>> aliases) {
    java.util.Objects.requireNonNull((aliases));
    return new Entry(schemaVersion, id, modified, published, withdrawn, aliases, related, summary, details, severity, affected, references, credits);
  }
  
  public Entry withRelated(hydra.util.Opt<java.util.List<hydra.ext.dev.osv.schema.Id>> related) {
    java.util.Objects.requireNonNull((related));
    return new Entry(schemaVersion, id, modified, published, withdrawn, aliases, related, summary, details, severity, affected, references, credits);
  }
  
  public Entry withSummary(hydra.util.Opt<String> summary) {
    java.util.Objects.requireNonNull((summary));
    return new Entry(schemaVersion, id, modified, published, withdrawn, aliases, related, summary, details, severity, affected, references, credits);
  }
  
  public Entry withDetails(hydra.util.Opt<hydra.ext.dev.osv.schema.Markdown> details) {
    java.util.Objects.requireNonNull((details));
    return new Entry(schemaVersion, id, modified, published, withdrawn, aliases, related, summary, details, severity, affected, references, credits);
  }
  
  public Entry withSeverity(hydra.util.Opt<java.util.List<hydra.ext.dev.osv.schema.Severity>> severity) {
    java.util.Objects.requireNonNull((severity));
    return new Entry(schemaVersion, id, modified, published, withdrawn, aliases, related, summary, details, severity, affected, references, credits);
  }
  
  public Entry withAffected(hydra.util.Opt<java.util.List<hydra.ext.dev.osv.schema.PackageVersions>> affected) {
    java.util.Objects.requireNonNull((affected));
    return new Entry(schemaVersion, id, modified, published, withdrawn, aliases, related, summary, details, severity, affected, references, credits);
  }
  
  public Entry withReferences(hydra.util.Opt<java.util.List<hydra.ext.dev.osv.schema.Reference>> references) {
    java.util.Objects.requireNonNull((references));
    return new Entry(schemaVersion, id, modified, published, withdrawn, aliases, related, summary, details, severity, affected, references, credits);
  }
  
  public Entry withCredits(hydra.util.Opt<java.util.List<hydra.ext.dev.osv.schema.Credited>> credits) {
    java.util.Objects.requireNonNull((credits));
    return new Entry(schemaVersion, id, modified, published, withdrawn, aliases, related, summary, details, severity, affected, references, credits);
  }
}