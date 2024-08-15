// Note: this is an automatically generated file. Do not edit.

package hydra.langs.shacl.model;

import java.io.Serializable;

/**
 * See https://www.w3.org/TR/shacl/#ClosedPatterConstraintComponent
 */
public class Closed implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/shacl/model.Closed");
  
  public static final hydra.core.Name FIELD_NAME_IS_CLOSED = new hydra.core.Name("isClosed");
  
  public static final hydra.core.Name FIELD_NAME_IGNORED_PROPERTIES = new hydra.core.Name("ignoredProperties");
  
  public final Boolean isClosed;
  
  public final hydra.util.Opt<java.util.Set<hydra.langs.rdf.syntax.Property>> ignoredProperties;
  
  public Closed (Boolean isClosed, hydra.util.Opt<java.util.Set<hydra.langs.rdf.syntax.Property>> ignoredProperties) {
    java.util.Objects.requireNonNull((isClosed));
    java.util.Objects.requireNonNull((ignoredProperties));
    this.isClosed = isClosed;
    this.ignoredProperties = ignoredProperties;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Closed)) {
      return false;
    }
    Closed o = (Closed) (other);
    return isClosed.equals(o.isClosed) && ignoredProperties.equals(o.ignoredProperties);
  }
  
  @Override
  public int hashCode() {
    return 2 * isClosed.hashCode() + 3 * ignoredProperties.hashCode();
  }
  
  public Closed withIsClosed(Boolean isClosed) {
    java.util.Objects.requireNonNull((isClosed));
    return new Closed(isClosed, ignoredProperties);
  }
  
  public Closed withIgnoredProperties(hydra.util.Opt<java.util.Set<hydra.langs.rdf.syntax.Property>> ignoredProperties) {
    java.util.Objects.requireNonNull((ignoredProperties));
    return new Closed(isClosed, ignoredProperties);
  }
}