// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public abstract class Iri implements Serializable, Comparable<Iri> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.Iri");
  
  public static final hydra.core.Name IRI_REF = new hydra.core.Name("IriRef");
  
  public static final hydra.core.Name PREFIXED_NAME = new hydra.core.Name("PrefixedName");
  
  private Iri () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(IriRef instance) ;
    
    R visit(PrefixedName instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Iri instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(IriRef instance) {
      return otherwise(instance);
    }
    
    default R visit(PrefixedName instance) {
      return otherwise(instance);
    }
  }
  
  public static final class IriRef extends hydra.ext.io.shex.syntax.Iri implements Serializable {
    public final hydra.ext.io.shex.syntax.IriRef value;
    
    public IriRef (hydra.ext.io.shex.syntax.IriRef value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof IriRef)) {
        return false;
      }
      IriRef o = (IriRef) other;
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
    public int compareTo(Iri other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      IriRef o = (IriRef) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class PrefixedName extends hydra.ext.io.shex.syntax.Iri implements Serializable {
    public final hydra.ext.io.shex.syntax.PrefixedName value;
    
    public PrefixedName (hydra.ext.io.shex.syntax.PrefixedName value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PrefixedName)) {
        return false;
      }
      PrefixedName o = (PrefixedName) other;
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
    public int compareTo(Iri other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      PrefixedName o = (PrefixedName) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
