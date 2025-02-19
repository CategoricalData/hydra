// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public abstract class LambdaStarEtc implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.LambdaStarEtc");
  
  public static final hydra.core.Name FIELD_NAME_STAR = new hydra.core.Name("star");
  
  public static final hydra.core.Name FIELD_NAME_PARAM_NO_DEFAULT = new hydra.core.Name("paramNoDefault");
  
  public static final hydra.core.Name FIELD_NAME_PARAM_MAYBE_DEFAULT = new hydra.core.Name("paramMaybeDefault");
  
  public static final hydra.core.Name FIELD_NAME_KWDS = new hydra.core.Name("kwds");
  
  private LambdaStarEtc () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Star instance) ;
    
    R visit(ParamNoDefault instance) ;
    
    R visit(ParamMaybeDefault instance) ;
    
    R visit(Kwds instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(LambdaStarEtc instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Star instance) {
      return otherwise((instance));
    }
    
    default R visit(ParamNoDefault instance) {
      return otherwise((instance));
    }
    
    default R visit(ParamMaybeDefault instance) {
      return otherwise((instance));
    }
    
    default R visit(Kwds instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Star extends hydra.ext.python.syntax.LambdaStarEtc implements Serializable {
    public final Boolean value;
    
    public Star (Boolean value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Star)) {
        return false;
      }
      Star o = (Star) (other);
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
  
  public static final class ParamNoDefault extends hydra.ext.python.syntax.LambdaStarEtc implements Serializable {
    public final hydra.ext.python.syntax.LambdaParamNoDefault value;
    
    public ParamNoDefault (hydra.ext.python.syntax.LambdaParamNoDefault value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ParamNoDefault)) {
        return false;
      }
      ParamNoDefault o = (ParamNoDefault) (other);
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
  
  public static final class ParamMaybeDefault extends hydra.ext.python.syntax.LambdaStarEtc implements Serializable {
    public final java.util.List<hydra.ext.python.syntax.LambdaParamMaybeDefault> value;
    
    public ParamMaybeDefault (java.util.List<hydra.ext.python.syntax.LambdaParamMaybeDefault> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ParamMaybeDefault)) {
        return false;
      }
      ParamMaybeDefault o = (ParamMaybeDefault) (other);
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
  
  public static final class Kwds extends hydra.ext.python.syntax.LambdaStarEtc implements Serializable {
    public final hydra.ext.python.syntax.LambdaKwds value;
    
    public Kwds (hydra.ext.python.syntax.LambdaKwds value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Kwds)) {
        return false;
      }
      Kwds o = (Kwds) (other);
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