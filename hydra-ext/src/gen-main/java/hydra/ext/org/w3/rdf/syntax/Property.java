// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.rdf.syntax;

import java.io.Serializable;

/**
 * A type representing an RDF property, and encapsulating its domain, range, and subclass relationships
 */
public class Property implements Serializable, Comparable<Property> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.rdf.syntax.Property");
  
  public static final hydra.core.Name DOMAIN = new hydra.core.Name("domain");
  
  public static final hydra.core.Name RANGE = new hydra.core.Name("range");
  
  public static final hydra.core.Name SUB_PROPERTY_OF = new hydra.core.Name("subPropertyOf");
  
  /**
   * State that any resource that has a given property is an instance of one or more classes
   */
  public final hydra.util.PersistentSet<hydra.ext.org.w3.rdf.syntax.RdfsClass> domain;
  
  /**
   * States that the values of a property are instances of one or more classes
   */
  public final hydra.util.PersistentSet<hydra.ext.org.w3.rdf.syntax.RdfsClass> range;
  
  public final hydra.util.PersistentSet<hydra.ext.org.w3.rdf.syntax.Property> subPropertyOf;
  
  public Property (hydra.util.PersistentSet<hydra.ext.org.w3.rdf.syntax.RdfsClass> domain, hydra.util.PersistentSet<hydra.ext.org.w3.rdf.syntax.RdfsClass> range, hydra.util.PersistentSet<hydra.ext.org.w3.rdf.syntax.Property> subPropertyOf) {
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
    cmp = Integer.compare(
      domain.hashCode(),
      other.domain.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      range.hashCode(),
      other.range.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      subPropertyOf.hashCode(),
      other.subPropertyOf.hashCode());
  }
  
  public Property withDomain(hydra.util.PersistentSet<hydra.ext.org.w3.rdf.syntax.RdfsClass> domain) {
    return new Property(domain, range, subPropertyOf);
  }
  
  public Property withRange(hydra.util.PersistentSet<hydra.ext.org.w3.rdf.syntax.RdfsClass> range) {
    return new Property(domain, range, subPropertyOf);
  }
  
  public Property withSubPropertyOf(hydra.util.PersistentSet<hydra.ext.org.w3.rdf.syntax.Property> subPropertyOf) {
    return new Property(domain, range, subPropertyOf);
  }
}
