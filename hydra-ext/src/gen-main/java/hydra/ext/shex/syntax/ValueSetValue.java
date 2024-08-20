// Note: this is an automatically generated file. Do not edit.

package hydra.ext.shex.syntax;

import java.io.Serializable;

public abstract class ValueSetValue implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/shex/syntax.ValueSetValue");
  
  public static final hydra.core.Name FIELD_NAME_IRI_RANGE = new hydra.core.Name("iriRange");
  
  public static final hydra.core.Name FIELD_NAME_LITERAL = new hydra.core.Name("literal");
  
  private ValueSetValue () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(IriRange instance) ;
    
    R visit(Literal instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ValueSetValue instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(IriRange instance) {
      return otherwise((instance));
    }
    
    default R visit(Literal instance) {
      return otherwise((instance));
    }
  }
  
  public static final class IriRange extends hydra.ext.shex.syntax.ValueSetValue implements Serializable {
    public final hydra.ext.shex.syntax.IriRange value;
    
    public IriRange (hydra.ext.shex.syntax.IriRange value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof IriRange)) {
        return false;
      }
      IriRange o = (IriRange) (other);
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
  
  public static final class Literal extends hydra.ext.shex.syntax.ValueSetValue implements Serializable {
    public final hydra.ext.shex.syntax.Literal value;
    
    public Literal (hydra.ext.shex.syntax.Literal value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Literal)) {
        return false;
      }
      Literal o = (Literal) (other);
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
