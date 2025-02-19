// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class ForInitializer implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.ForInitializer");
  
  public static final hydra.core.Name FIELD_NAME_VARIABLE = new hydra.core.Name("variable");
  
  public static final hydra.core.Name FIELD_NAME_STATEMENTS = new hydra.core.Name("statements");
  
  private ForInitializer () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Variable instance) ;
    
    R visit(Statements instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ForInitializer instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Variable instance) {
      return otherwise((instance));
    }
    
    default R visit(Statements instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Variable extends hydra.ext.csharp.syntax.ForInitializer implements Serializable {
    public final hydra.ext.csharp.syntax.LocalVariableDeclaration value;
    
    public Variable (hydra.ext.csharp.syntax.LocalVariableDeclaration value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Variable)) {
        return false;
      }
      Variable o = (Variable) (other);
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
  
  public static final class Statements extends hydra.ext.csharp.syntax.ForInitializer implements Serializable {
    public final hydra.ext.csharp.syntax.StatementExpressionList value;
    
    public Statements (hydra.ext.csharp.syntax.StatementExpressionList value) {
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
}