// Note: this is an automatically generated file. Do not edit.

package hydra.ext.com.microsoft.kusto.kql;

import java.io.Serializable;

public abstract class Function implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.com.microsoft.kusto.kql.Function");
  
  public static final hydra.core.Name FIELD_NAME_BUILT_IN = new hydra.core.Name("builtIn");
  
  public static final hydra.core.Name FIELD_NAME_CUSTOM = new hydra.core.Name("custom");
  
  private Function () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(BuiltIn instance) ;
    
    R visit(Custom instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Function instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(BuiltIn instance) {
      return otherwise((instance));
    }
    
    default R visit(Custom instance) {
      return otherwise((instance));
    }
  }
  
  public static final class BuiltIn extends hydra.ext.com.microsoft.kusto.kql.Function implements Serializable {
    public final hydra.ext.com.microsoft.kusto.kql.BuiltInFunction value;
    
    public BuiltIn (hydra.ext.com.microsoft.kusto.kql.BuiltInFunction value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof BuiltIn)) {
        return false;
      }
      BuiltIn o = (BuiltIn) (other);
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
  
  public static final class Custom extends hydra.ext.com.microsoft.kusto.kql.Function implements Serializable {
    public final hydra.ext.com.microsoft.kusto.kql.FunctionName value;
    
    public Custom (hydra.ext.com.microsoft.kusto.kql.FunctionName value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Custom)) {
        return false;
      }
      Custom o = (Custom) (other);
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