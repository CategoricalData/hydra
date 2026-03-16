// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class KeywordOrIdentifier implements Serializable, Comparable<KeywordOrIdentifier> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.KeywordOrIdentifier");
  
  public static final hydra.core.Name KEYWORD = new hydra.core.Name("keyword");
  
  public static final hydra.core.Name IDENTIFIER = new hydra.core.Name("identifier");
  
  private KeywordOrIdentifier () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Keyword instance) ;
    
    R visit(Identifier instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(KeywordOrIdentifier instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Keyword instance) {
      return otherwise(instance);
    }
    
    default R visit(Identifier instance) {
      return otherwise(instance);
    }
  }
  
  public static final class Keyword extends hydra.ext.org.apache.tinkerpop.gremlin.KeywordOrIdentifier implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.Keyword value;
    
    public Keyword (hydra.ext.org.apache.tinkerpop.gremlin.Keyword value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Keyword)) {
        return false;
      }
      Keyword o = (Keyword) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(KeywordOrIdentifier other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Keyword o = (Keyword) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Identifier extends hydra.ext.org.apache.tinkerpop.gremlin.KeywordOrIdentifier implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.Identifier value;
    
    public Identifier (hydra.ext.org.apache.tinkerpop.gremlin.Identifier value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Identifier)) {
        return false;
      }
      Identifier o = (Identifier) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(KeywordOrIdentifier other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Identifier o = (Identifier) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
