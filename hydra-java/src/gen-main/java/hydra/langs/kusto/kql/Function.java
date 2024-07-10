// Note: this is an automatically generated file. Do not edit.

package hydra.langs.kusto.kql;

import java.io.Serializable;

public abstract class Function implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/kusto/kql.Function");
  
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
  
  public static final class BuiltIn extends hydra.langs.kusto.kql.Function implements Serializable {
    public final hydra.langs.kusto.kql.BuiltInFunction value;
    
    public BuiltIn (hydra.langs.kusto.kql.BuiltInFunction value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
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
  
  public static final class Custom extends hydra.langs.kusto.kql.Function implements Serializable {
    public final hydra.langs.kusto.kql.FunctionName value;
    
    public Custom (hydra.langs.kusto.kql.FunctionName value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
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