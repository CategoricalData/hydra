// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public abstract class LiteralExpressionOrAttribute implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.LiteralExpressionOrAttribute");
  
  public static final hydra.core.Name FIELD_NAME_LITERAL = new hydra.core.Name("literal");
  
  public static final hydra.core.Name FIELD_NAME_ATTRIBUTE = new hydra.core.Name("attribute");
  
  private LiteralExpressionOrAttribute () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Literal instance) ;
    
    R visit(Attribute instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(LiteralExpressionOrAttribute instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Literal instance) {
      return otherwise((instance));
    }
    
    default R visit(Attribute instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Literal extends hydra.ext.python.syntax.LiteralExpressionOrAttribute implements Serializable {
    public final hydra.ext.python.syntax.LiteralExpression value;
    
    public Literal (hydra.ext.python.syntax.LiteralExpression value) {
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
  
  public static final class Attribute extends hydra.ext.python.syntax.LiteralExpressionOrAttribute implements Serializable {
    public final hydra.ext.python.syntax.Attribute value;
    
    public Attribute (hydra.ext.python.syntax.Attribute value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Attribute)) {
        return false;
      }
      Attribute o = (Attribute) (other);
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