// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public abstract class VariableAccess implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/java/syntax.VariableAccess");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION_NAME = new hydra.core.Name("expressionName");
  
  public static final hydra.core.Name FIELD_NAME_FIELD_ACCESS = new hydra.core.Name("fieldAccess");
  
  private VariableAccess () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(ExpressionName instance) ;
    
    R visit(FieldAccess instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(VariableAccess instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(ExpressionName instance) {
      return otherwise((instance));
    }
    
    default R visit(FieldAccess instance) {
      return otherwise((instance));
    }
  }
  
  public static final class ExpressionName extends hydra.langs.java.syntax.VariableAccess implements Serializable {
    public final hydra.langs.java.syntax.ExpressionName value;
    
    public ExpressionName (hydra.langs.java.syntax.ExpressionName value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ExpressionName)) {
        return false;
      }
      ExpressionName o = (ExpressionName) (other);
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
  
  public static final class FieldAccess extends hydra.langs.java.syntax.VariableAccess implements Serializable {
    public final hydra.langs.java.syntax.FieldAccess value;
    
    public FieldAccess (hydra.langs.java.syntax.FieldAccess value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof FieldAccess)) {
        return false;
      }
      FieldAccess o = (FieldAccess) (other);
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