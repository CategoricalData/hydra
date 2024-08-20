// Note: this is an automatically generated file. Do not edit.

package hydra.ext.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class DedupArgs implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/tinkerpop/gremlin.DedupArgs");
  
  public static final hydra.core.Name FIELD_NAME_SCOPE_STRING = new hydra.core.Name("scopeString");
  
  public static final hydra.core.Name FIELD_NAME_STRING = new hydra.core.Name("string");
  
  private DedupArgs () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(ScopeString instance) ;
    
    R visit(String_ instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(DedupArgs instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(ScopeString instance) {
      return otherwise((instance));
    }
    
    default R visit(String_ instance) {
      return otherwise((instance));
    }
  }
  
  public static final class ScopeString extends hydra.ext.tinkerpop.gremlin.DedupArgs implements Serializable {
    public final hydra.ext.tinkerpop.gremlin.ScopeStringArgument value;
    
    public ScopeString (hydra.ext.tinkerpop.gremlin.ScopeStringArgument value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ScopeString)) {
        return false;
      }
      ScopeString o = (ScopeString) (other);
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
  
  public static final class String_ extends hydra.ext.tinkerpop.gremlin.DedupArgs implements Serializable {
    public final java.util.List<hydra.ext.tinkerpop.gremlin.StringNullableArgument> value;
    
    public String_ (java.util.List<hydra.ext.tinkerpop.gremlin.StringNullableArgument> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof String_)) {
        return false;
      }
      String_ o = (String_) (other);
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
