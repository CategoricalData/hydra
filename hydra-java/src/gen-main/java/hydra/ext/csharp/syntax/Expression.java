// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class Expression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.Expression");
  
  public static final hydra.core.Name FIELD_NAME_NON_ASSIGNMENT = new hydra.core.Name("nonAssignment");
  
  public static final hydra.core.Name FIELD_NAME_ASSIGNMENT = new hydra.core.Name("assignment");
  
  private Expression () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(NonAssignment instance) ;
    
    R visit(Assignment instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Expression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(NonAssignment instance) {
      return otherwise((instance));
    }
    
    default R visit(Assignment instance) {
      return otherwise((instance));
    }
  }
  
  public static final class NonAssignment extends hydra.ext.csharp.syntax.Expression implements Serializable {
    public final hydra.ext.csharp.syntax.NonAssignmentExpression value;
    
    public NonAssignment (hydra.ext.csharp.syntax.NonAssignmentExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NonAssignment)) {
        return false;
      }
      NonAssignment o = (NonAssignment) (other);
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
  
  public static final class Assignment extends hydra.ext.csharp.syntax.Expression implements Serializable {
    public final hydra.ext.csharp.syntax.Assignment value;
    
    public Assignment (hydra.ext.csharp.syntax.Assignment value) {
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
}