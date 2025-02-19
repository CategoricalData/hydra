// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public abstract class SliceOrStarredExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.SliceOrStarredExpression");
  
  public static final hydra.core.Name FIELD_NAME_SLICE = new hydra.core.Name("slice");
  
  public static final hydra.core.Name FIELD_NAME_STARRED = new hydra.core.Name("starred");
  
  private SliceOrStarredExpression () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Slice instance) ;
    
    R visit(Starred instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SliceOrStarredExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Slice instance) {
      return otherwise((instance));
    }
    
    default R visit(Starred instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Slice extends hydra.ext.python.syntax.SliceOrStarredExpression implements Serializable {
    public final hydra.ext.python.syntax.Slice value;
    
    public Slice (hydra.ext.python.syntax.Slice value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Slice)) {
        return false;
      }
      Slice o = (Slice) (other);
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
  
  public static final class Starred extends hydra.ext.python.syntax.SliceOrStarredExpression implements Serializable {
    public final hydra.ext.python.syntax.StarredExpression value;
    
    public Starred (hydra.ext.python.syntax.StarredExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Starred)) {
        return false;
      }
      Starred o = (Starred) (other);
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