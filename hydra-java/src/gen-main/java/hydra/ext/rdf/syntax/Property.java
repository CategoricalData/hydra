package hydra.ext.rdf.syntax;

/**
 * A type representing an RDF property, and encapsulating its domain, range, and subclass relationships
 */
public class Property {
  /**
   * State that any resource that has a given property is an instance of one or more classes
   */
  public final java.util.Set<RdfsClass> domain;
  
  /**
   * States that the values of a property are instances of one or more classes
   */
  public final java.util.Set<RdfsClass> range;
  
  public final java.util.Set<Property> subPropertyOf;
  
  public Property (java.util.Set<RdfsClass> domain, java.util.Set<RdfsClass> range, java.util.Set<Property> subPropertyOf) {
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
  
  public Property withDomain(java.util.Set<RdfsClass> domain) {
    return new Property(domain, range, subPropertyOf);
  }
  
  public Property withRange(java.util.Set<RdfsClass> range) {
    return new Property(domain, range, subPropertyOf);
  }
  
  public Property withSubPropertyOf(java.util.Set<Property> subPropertyOf) {
    return new Property(domain, range, subPropertyOf);
  }
}