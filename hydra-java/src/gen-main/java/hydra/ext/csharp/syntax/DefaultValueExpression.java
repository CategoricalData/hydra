// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class DefaultValueExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.DefaultValueExpression");
  
  public static final hydra.core.Name FIELD_NAME_EXPLICITLY_TYPED = new hydra.core.Name("explicitlyTyped");
  
  public static final hydra.core.Name FIELD_NAME_DEFAULT_LITERAL = new hydra.core.Name("defaultLiteral");
  
  private DefaultValueExpression () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(ExplicitlyTyped instance) ;
    
    R visit(DefaultLiteral instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(DefaultValueExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(ExplicitlyTyped instance) {
      return otherwise((instance));
    }
    
    default R visit(DefaultLiteral instance) {
      return otherwise((instance));
    }
  }
  
  public static final class ExplicitlyTyped extends hydra.ext.csharp.syntax.DefaultValueExpression implements Serializable {
    public final hydra.ext.csharp.syntax.Type value;
    
    public ExplicitlyTyped (hydra.ext.csharp.syntax.Type value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ExplicitlyTyped)) {
        return false;
      }
      ExplicitlyTyped o = (ExplicitlyTyped) (other);
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
  
  public static final class DefaultLiteral extends hydra.ext.csharp.syntax.DefaultValueExpression implements Serializable {
    public DefaultLiteral () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DefaultLiteral)) {
        return false;
      }
      DefaultLiteral o = (DefaultLiteral) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}