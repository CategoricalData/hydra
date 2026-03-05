// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

public abstract class DatatypeRestriction_ConstrainingFacet implements Serializable, Comparable<DatatypeRestriction_ConstrainingFacet> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.DatatypeRestriction_ConstrainingFacet");
  
  public static final hydra.core.Name XML_SCHEMA = new hydra.core.Name("xmlSchema");
  
  public static final hydra.core.Name OTHER = new hydra.core.Name("other");
  
  private DatatypeRestriction_ConstrainingFacet () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(XmlSchema instance) ;
    
    R visit(Other instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(DatatypeRestriction_ConstrainingFacet instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(XmlSchema instance) {
      return otherwise(instance);
    }
    
    default R visit(Other instance) {
      return otherwise(instance);
    }
  }
  
  /**
   * Note: XML Schema constraining facets are treated as a special case in this model (not in the OWL 2 specification itself) because they are particularly common
   */
  public static final class XmlSchema extends hydra.ext.org.w3.owl.syntax.DatatypeRestriction_ConstrainingFacet implements Serializable {
    public final hydra.ext.org.w3.xml.schema.ConstrainingFacet value;
    
    public XmlSchema (hydra.ext.org.w3.xml.schema.ConstrainingFacet value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof XmlSchema)) {
        return false;
      }
      XmlSchema o = (XmlSchema) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(DatatypeRestriction_ConstrainingFacet other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      XmlSchema o = (XmlSchema) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Other extends hydra.ext.org.w3.owl.syntax.DatatypeRestriction_ConstrainingFacet implements Serializable {
    public final hydra.ext.org.w3.rdf.syntax.Iri value;
    
    public Other (hydra.ext.org.w3.rdf.syntax.Iri value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Other)) {
        return false;
      }
      Other o = (Other) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(DatatypeRestriction_ConstrainingFacet other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Other o = (Other) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
