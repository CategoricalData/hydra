// Note: this is an automatically generated file. Do not edit.

package hydra.ext.dev.osv.schema;

import java.io.Serializable;

public class VersionRange implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.dev.osv.schema.VersionRange");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_REPO = new hydra.core.Name("repo");
  
  public static final hydra.core.Name FIELD_NAME_EVENTS = new hydra.core.Name("events");
  
  public final hydra.ext.dev.osv.schema.VersionType type;
  
  public final hydra.util.Opt<hydra.ext.dev.osv.schema.Url> repo;
  
  public final java.util.List<hydra.ext.dev.osv.schema.Event> events;
  
  public VersionRange (hydra.ext.dev.osv.schema.VersionType type, hydra.util.Opt<hydra.ext.dev.osv.schema.Url> repo, java.util.List<hydra.ext.dev.osv.schema.Event> events) {
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((repo));
    java.util.Objects.requireNonNull((events));
    this.type = type;
    this.repo = repo;
    this.events = events;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof VersionRange)) {
      return false;
    }
    VersionRange o = (VersionRange) (other);
    return type.equals(o.type) && repo.equals(o.repo) && events.equals(o.events);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * repo.hashCode() + 5 * events.hashCode();
  }
  
  public VersionRange withType(hydra.ext.dev.osv.schema.VersionType type) {
    java.util.Objects.requireNonNull((type));
    return new VersionRange(type, repo, events);
  }
  
  public VersionRange withRepo(hydra.util.Opt<hydra.ext.dev.osv.schema.Url> repo) {
    java.util.Objects.requireNonNull((repo));
    return new VersionRange(type, repo, events);
  }
  
  public VersionRange withEvents(java.util.List<hydra.ext.dev.osv.schema.Event> events) {
    java.util.Objects.requireNonNull((events));
    return new VersionRange(type, repo, events);
  }
}