// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public abstract class TypeExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.TypeExpression");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public static final hydra.core.Name FIELD_NAME_STARRED_EXPRESSION = new hydra.core.Name("starredExpression");
  
  public static final hydra.core.Name FIELD_NAME_DOUBLE_STARRED_EXPRESSION = new hydra.core.Name("doubleStarredExpression");
  
  private TypeExpression () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Expression instance) ;
    
    R visit(StarredExpression instance) ;
    
    R visit(DoubleStarredExpression instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TypeExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Expression instance) {
      return otherwise((instance));
    }
    
    default R visit(StarredExpression instance) {
      return otherwise((instance));
    }
    
    default R visit(DoubleStarredExpression instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Expression extends hydra.ext.python.syntax.TypeExpression implements Serializable {
    public final hydra.ext.python.syntax.Expression value;
    
    public Expression (hydra.ext.python.syntax.Expression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Expression)) {
        return false;
      }
      Expression o = (Expression) (other);
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
  
  public static final class StarredExpression extends hydra.ext.python.syntax.TypeExpression implements Serializable {
    public final hydra.ext.python.syntax.Expression value;
    
    public StarredExpression (hydra.ext.python.syntax.Expression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof StarredExpression)) {
        return false;
      }
      StarredExpression o = (StarredExpression) (other);
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
  
  public static final class DoubleStarredExpression extends hydra.ext.python.syntax.TypeExpression implements Serializable {
    public final hydra.ext.python.syntax.Expression value;
    
    public DoubleStarredExpression (hydra.ext.python.syntax.Expression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DoubleStarredExpression)) {
        return false;
      }
      DoubleStarredExpression o = (DoubleStarredExpression) (other);
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