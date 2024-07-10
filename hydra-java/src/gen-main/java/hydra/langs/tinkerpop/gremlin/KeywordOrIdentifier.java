// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class KeywordOrIdentifier implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.KeywordOrIdentifier");
  
  private KeywordOrIdentifier () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Keyword instance) ;
    
    R visit(Identifier instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(KeywordOrIdentifier instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Keyword instance) {
      return otherwise((instance));
    }
    
    default R visit(Identifier instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Keyword extends hydra.langs.tinkerpop.gremlin.KeywordOrIdentifier implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.Keyword value;
    
    public Keyword (hydra.langs.tinkerpop.gremlin.Keyword value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Keyword)) {
        return false;
      }
      Keyword o = (Keyword) (other);
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
  
  public static final class Identifier extends hydra.langs.tinkerpop.gremlin.KeywordOrIdentifier implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.Identifier value;
    
    public Identifier (hydra.langs.tinkerpop.gremlin.Identifier value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Identifier)) {
        return false;
      }
      Identifier o = (Identifier) (other);
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