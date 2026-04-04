// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.shacl.model;

import java.io.Serializable;

/**
 * See https://www.w3.org/TR/shacl/#ClosedPatterConstraintComponent
 */
public class Closed implements Serializable, Comparable<Closed> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.shacl.model.Closed");

  public static final hydra.core.Name IS_CLOSED = new hydra.core.Name("isClosed");

  public static final hydra.core.Name IGNORED_PROPERTIES = new hydra.core.Name("ignoredProperties");

  public final Boolean isClosed;

  public final hydra.util.Maybe<java.util.Set<hydra.ext.org.w3.rdf.syntax.Property>> ignoredProperties;

  public Closed (Boolean isClosed, hydra.util.Maybe<java.util.Set<hydra.ext.org.w3.rdf.syntax.Property>> ignoredProperties) {
    this.isClosed = isClosed;
    this.ignoredProperties = ignoredProperties;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Closed)) {
      return false;
    }
    Closed o = (Closed) other;
    return java.util.Objects.equals(
      this.isClosed,
      o.isClosed) && java.util.Objects.equals(
      this.ignoredProperties,
      o.ignoredProperties);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(isClosed) + 3 * java.util.Objects.hashCode(ignoredProperties);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Closed other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      isClosed,
      other.isClosed);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      ignoredProperties,
      other.ignoredProperties);
  }

  public Closed withIsClosed(Boolean isClosed) {
    return new Closed(isClosed, ignoredProperties);
  }

  public Closed withIgnoredProperties(hydra.util.Maybe<java.util.Set<hydra.ext.org.w3.rdf.syntax.Property>> ignoredProperties) {
    return new Closed(isClosed, ignoredProperties);
  }
}
