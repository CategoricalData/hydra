// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public abstract class SingleSubscriptAttributeTarget implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.SingleSubscriptAttributeTarget");
  
  public static final hydra.core.Name FIELD_NAME_PRIMARY_AND_NAME = new hydra.core.Name("primaryAndName");
  
  public static final hydra.core.Name FIELD_NAME_PRIMARY_AND_SLICES = new hydra.core.Name("primaryAndSlices");
  
  private SingleSubscriptAttributeTarget () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(PrimaryAndName instance) ;
    
    R visit(PrimaryAndSlices instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SingleSubscriptAttributeTarget instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(PrimaryAndName instance) {
      return otherwise((instance));
    }
    
    default R visit(PrimaryAndSlices instance) {
      return otherwise((instance));
    }
  }
  
  public static final class PrimaryAndName extends hydra.ext.python.syntax.SingleSubscriptAttributeTarget implements Serializable {
    public final hydra.ext.python.syntax.TPrimaryAndName value;
    
    public PrimaryAndName (hydra.ext.python.syntax.TPrimaryAndName value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PrimaryAndName)) {
        return false;
      }
      PrimaryAndName o = (PrimaryAndName) (other);
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
  
  public static final class PrimaryAndSlices extends hydra.ext.python.syntax.SingleSubscriptAttributeTarget implements Serializable {
    public final hydra.ext.python.syntax.TPrimaryAndSlices value;
    
    public PrimaryAndSlices (hydra.ext.python.syntax.TPrimaryAndSlices value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PrimaryAndSlices)) {
        return false;
      }
      PrimaryAndSlices o = (PrimaryAndSlices) (other);
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