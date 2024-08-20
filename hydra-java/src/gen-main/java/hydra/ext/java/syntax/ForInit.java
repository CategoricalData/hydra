// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public abstract class ForInit implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/java/syntax.ForInit");
  
  public static final hydra.core.Name FIELD_NAME_STATEMENTS = new hydra.core.Name("statements");
  
  public static final hydra.core.Name FIELD_NAME_LOCAL_VARIABLE = new hydra.core.Name("localVariable");
  
  private ForInit () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Statements instance) ;
    
    R visit(LocalVariable instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ForInit instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Statements instance) {
      return otherwise((instance));
    }
    
    default R visit(LocalVariable instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Statements extends hydra.ext.java.syntax.ForInit implements Serializable {
    public final java.util.List<hydra.ext.java.syntax.StatementExpression> value;
    
    public Statements (java.util.List<hydra.ext.java.syntax.StatementExpression> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Statements)) {
        return false;
      }
      Statements o = (Statements) (other);
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
  
  public static final class LocalVariable extends hydra.ext.java.syntax.ForInit implements Serializable {
    public final hydra.ext.java.syntax.LocalVariableDeclaration value;
    
    public LocalVariable (hydra.ext.java.syntax.LocalVariableDeclaration value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LocalVariable)) {
        return false;
      }
      LocalVariable o = (LocalVariable) (other);
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
