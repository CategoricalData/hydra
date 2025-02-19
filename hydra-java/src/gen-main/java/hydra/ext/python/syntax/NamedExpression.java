// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public abstract class NamedExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.NamedExpression");
  
  public static final hydra.core.Name FIELD_NAME_ASSIGNMENT = new hydra.core.Name("assignment");
  
  public static final hydra.core.Name FIELD_NAME_SIMPLE = new hydra.core.Name("simple");
  
  private NamedExpression () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Assignment instance) ;
    
    R visit(Simple instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(NamedExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Assignment instance) {
      return otherwise((instance));
    }
    
    default R visit(Simple instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Assignment extends hydra.ext.python.syntax.NamedExpression implements Serializable {
    public final hydra.ext.python.syntax.AssignmentExpression value;
    
    public Assignment (hydra.ext.python.syntax.AssignmentExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Assignment)) {
        return false;
      }
      Assignment o = (Assignment) (other);
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
  
  public static final class Simple extends hydra.ext.python.syntax.NamedExpression implements Serializable {
    public final hydra.ext.python.syntax.Expression value;
    
    public Simple (hydra.ext.python.syntax.Expression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Simple)) {
        return false;
      }
      Simple o = (Simple) (other);
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