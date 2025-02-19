// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class VarianceAnnotation implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.VarianceAnnotation");
  
  public static final hydra.core.Name FIELD_NAME_IN = new hydra.core.Name("in");
  
  public static final hydra.core.Name FIELD_NAME_OUT = new hydra.core.Name("out");
  
  private VarianceAnnotation () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(In instance) ;
    
    R visit(Out instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(VarianceAnnotation instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(In instance) {
      return otherwise((instance));
    }
    
    default R visit(Out instance) {
      return otherwise((instance));
    }
  }
  
  public static final class In extends hydra.ext.csharp.syntax.VarianceAnnotation implements Serializable {
    public In () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof In)) {
        return false;
      }
      In o = (In) (other);
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
  
  public static final class Out extends hydra.ext.csharp.syntax.VarianceAnnotation implements Serializable {
    public Out () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Out)) {
        return false;
      }
      Out o = (Out) (other);
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