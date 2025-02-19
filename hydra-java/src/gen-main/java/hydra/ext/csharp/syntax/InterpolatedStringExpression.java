// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class InterpolatedStringExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.InterpolatedStringExpression");
  
  public static final hydra.core.Name FIELD_NAME_REGULAR = new hydra.core.Name("regular");
  
  public static final hydra.core.Name FIELD_NAME_VERBATIM = new hydra.core.Name("verbatim");
  
  private InterpolatedStringExpression () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Regular instance) ;
    
    R visit(Verbatim instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(InterpolatedStringExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Regular instance) {
      return otherwise((instance));
    }
    
    default R visit(Verbatim instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Regular extends hydra.ext.csharp.syntax.InterpolatedStringExpression implements Serializable {
    public final hydra.ext.csharp.syntax.InterpolatedRegularStringExpression value;
    
    public Regular (hydra.ext.csharp.syntax.InterpolatedRegularStringExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Regular)) {
        return false;
      }
      Regular o = (Regular) (other);
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
  
  public static final class Verbatim extends hydra.ext.csharp.syntax.InterpolatedStringExpression implements Serializable {
    public final hydra.ext.csharp.syntax.InterpolatedVerbatimStringExpression value;
    
    public Verbatim (hydra.ext.csharp.syntax.InterpolatedVerbatimStringExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Verbatim)) {
        return false;
      }
      Verbatim o = (Verbatim) (other);
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