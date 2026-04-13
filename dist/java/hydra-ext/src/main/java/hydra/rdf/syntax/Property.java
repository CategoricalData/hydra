// Note: this is an automatically generated file. Do not edit.

package hydra.rdf.syntax;

import java.io.Serializable;

/**
 * A type representing an RDF property, and encapsulating its domain, range, and subclass relationships
 */
public class Property implements Serializable, Comparable<Property> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.rdf.syntax.Property");

  public static final hydra.core.Name DOMAIN = new hydra.core.Name("domain");

  public static final hydra.core.Name RANGE = new hydra.core.Name("range");

  public static final hydra.core.Name SUB_PROPERTY_OF = new hydra.core.Name("subPropertyOf");

  /**
   * State that any resource that has a given property is an instance of one or more classes
   */
  public final java.util.Set<hydra.rdf.syntax.RdfsClass> domain;

  /**
   * States that the values of a property are instances of one or more classes
   */
  public final java.util.Set<hydra.rdf.syntax.RdfsClass> range;

  public final java.util.Set<hydra.rdf.syntax.Property> subPropertyOf;

  public Property (java.util.Set<hydra.rdf.syntax.RdfsClass> domain, java.util.Set<hydra.rdf.syntax.RdfsClass> range, java.util.Set<hydra.rdf.syntax.Property> subPropertyOf) {
    this.domain = domain;
    this.range = range;
    this.subPropertyOf = subPropertyOf;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Property)) {
      return false;
    }
    Property o = (Property) other;
    return java.util.Objects.equals(
      this.domain,
      o.domain) && java.util.Objects.equals(
      this.range,
      o.range) && java.util.Objects.equals(
      this.subPropertyOf,
      o.subPropertyOf);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(domain) + 3 * java.util.Objects.hashCode(range) + 5 * java.util.Objects.hashCode(subPropertyOf);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Property other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      domain,
      other.domain);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      range,
      other.range);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      subPropertyOf,
      other.subPropertyOf);
  }

  public Property withDomain(java.util.Set<hydra.rdf.syntax.RdfsClass> domain) {
    return new Property(domain, range, subPropertyOf);
  }

  public Property withRange(java.util.Set<hydra.rdf.syntax.RdfsClass> range) {
    return new Property(domain, range, subPropertyOf);
  }

  public Property withSubPropertyOf(java.util.Set<hydra.rdf.syntax.Property> subPropertyOf) {
    return new Property(domain, range, subPropertyOf);
  }
}
