// Note: this is an automatically generated file. Do not edit.

package hydra.ext.rdf.syntax;

import java.io.Serializable;

/**
 * A type representing an RDF property, and encapsulating its domain, range, and subclass relationships
 */
public class Property implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/rdf/syntax.Property");
  
  public static final hydra.core.Name FIELD_NAME_DOMAIN = new hydra.core.Name("domain");
  
  public static final hydra.core.Name FIELD_NAME_RANGE = new hydra.core.Name("range");
  
  public static final hydra.core.Name FIELD_NAME_SUB_PROPERTY_OF = new hydra.core.Name("subPropertyOf");
  
  /**
   * State that any resource that has a given property is an instance of one or more classes
   */
  public final java.util.Set<hydra.ext.rdf.syntax.RdfsClass> domain;
  
  /**
   * States that the values of a property are instances of one or more classes
   */
  public final java.util.Set<hydra.ext.rdf.syntax.RdfsClass> range;
  
  public final java.util.Set<hydra.ext.rdf.syntax.Property> subPropertyOf;
  
  public Property (java.util.Set<hydra.ext.rdf.syntax.RdfsClass> domain, java.util.Set<hydra.ext.rdf.syntax.RdfsClass> range, java.util.Set<hydra.ext.rdf.syntax.Property> subPropertyOf) {
    java.util.Objects.requireNonNull((domain));
    java.util.Objects.requireNonNull((range));
    java.util.Objects.requireNonNull((subPropertyOf));
    this.domain = domain;
    this.range = range;
    this.subPropertyOf = subPropertyOf;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Property)) {
      return false;
    }
    Property o = (Property) (other);
    return domain.equals(o.domain) && range.equals(o.range) && subPropertyOf.equals(o.subPropertyOf);
  }
  
  @Override
  public int hashCode() {
    return 2 * domain.hashCode() + 3 * range.hashCode() + 5 * subPropertyOf.hashCode();
  }
  
  public Property withDomain(java.util.Set<hydra.ext.rdf.syntax.RdfsClass> domain) {
    java.util.Objects.requireNonNull((domain));
    return new Property(domain, range, subPropertyOf);
  }
  
  public Property withRange(java.util.Set<hydra.ext.rdf.syntax.RdfsClass> range) {
    java.util.Objects.requireNonNull((range));
    return new Property(domain, range, subPropertyOf);
  }
  
  public Property withSubPropertyOf(java.util.Set<hydra.ext.rdf.syntax.Property> subPropertyOf) {
    java.util.Objects.requireNonNull((subPropertyOf));
    return new Property(domain, range, subPropertyOf);
  }
}
