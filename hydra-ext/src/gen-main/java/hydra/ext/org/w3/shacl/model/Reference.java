// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.shacl.model;

import java.io.Serializable;

/**
 * Either an instance of a type like sh:Shape or sh:NodeShape, or an IRI which refers to an instance of that type
 */
public abstract class Reference<A> implements Serializable, Comparable<Reference<A>> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.shacl.model.Reference");
  
  public static final hydra.core.Name NAMED = new hydra.core.Name("named");
  
  public static final hydra.core.Name ANONYMOUS = new hydra.core.Name("anonymous");
  
  public static final hydra.core.Name DEFINITION = new hydra.core.Name("definition");
  
  private Reference () {
  
  }
  
  public abstract <R> R accept(Visitor<A, R> visitor) ;
  
  public interface Visitor<A, R> {
    R visit(Named<A> instance) ;
    
    R visit(Anonymous<A> instance) ;
    
    R visit(Definition<A> instance) ;
  }
  
  public interface PartialVisitor<A, R> extends Visitor<A, R> {
    default R otherwise(Reference<A> instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Named<A> instance) {
      return otherwise(instance);
    }
    
    default R visit(Anonymous<A> instance) {
      return otherwise(instance);
    }
    
    default R visit(Definition<A> instance) {
      return otherwise(instance);
    }
  }
  
  public static final class Named<A> extends hydra.ext.org.w3.shacl.model.Reference<A> implements Serializable {
    public final hydra.ext.org.w3.rdf.syntax.Iri value;
    
    public Named (hydra.ext.org.w3.rdf.syntax.Iri value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Named)) {
        return false;
      }
      Named o = (Named) other;
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
    public int compareTo(Reference other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Named o = (Named) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<A, R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * An anonymous instance
   */
  public static final class Anonymous<A> extends hydra.ext.org.w3.shacl.model.Reference<A> implements Serializable {
    public final A value;
    
    public Anonymous (A value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Anonymous)) {
        return false;
      }
      Anonymous o = (Anonymous) other;
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
    public int compareTo(Reference other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Anonymous o = (Anonymous) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<A, R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * An inline definition
   */
  public static final class Definition<A> extends hydra.ext.org.w3.shacl.model.Reference<A> implements Serializable {
    public final hydra.ext.org.w3.shacl.model.Definition<A> value;
    
    public Definition (hydra.ext.org.w3.shacl.model.Definition<A> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Definition)) {
        return false;
      }
      Definition o = (Definition) other;
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
    public int compareTo(Reference other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Definition o = (Definition) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<A, R> visitor) {
      return visitor.visit(this);
    }
  }
}
