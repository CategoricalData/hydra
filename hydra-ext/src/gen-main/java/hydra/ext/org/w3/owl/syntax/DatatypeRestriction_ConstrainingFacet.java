// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

public abstract class DatatypeRestriction_ConstrainingFacet implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/w3/owl/syntax.DatatypeRestriction.ConstrainingFacet");
  
  public static final hydra.core.Name FIELD_NAME_XML_SCHEMA = new hydra.core.Name("xmlSchema");
  
  public static final hydra.core.Name FIELD_NAME_OTHER = new hydra.core.Name("other");
  
  private DatatypeRestriction_ConstrainingFacet () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(XmlSchema instance) ;
    
    R visit(Other instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(DatatypeRestriction_ConstrainingFacet instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(XmlSchema instance) {
      return otherwise((instance));
    }
    
    default R visit(Other instance) {
      return otherwise((instance));
    }
  }
  
  /**
   * Note: XML Schema constraining facets are treated as a special case in this model (not in the OWL 2 specification itself) because they are particularly common
   */
  public static final class XmlSchema extends hydra.ext.org.w3.owl.syntax.DatatypeRestriction_ConstrainingFacet implements Serializable {
    public final hydra.ext.org.w3.xml.schema.ConstrainingFacet value;
    
    public XmlSchema (hydra.ext.org.w3.xml.schema.ConstrainingFacet value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof XmlSchema)) {
        return false;
      }
      XmlSchema o = (XmlSchema) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Other extends hydra.ext.org.w3.owl.syntax.DatatypeRestriction_ConstrainingFacet implements Serializable {
    public final hydra.ext.org.w3.rdf.syntax.Iri value;
    
    public Other (hydra.ext.org.w3.rdf.syntax.Iri value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Other)) {
        return false;
      }
      Other o = (Other) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
