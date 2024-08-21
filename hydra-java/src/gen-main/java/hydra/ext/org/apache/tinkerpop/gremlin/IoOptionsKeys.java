// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class IoOptionsKeys implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/apache/tinkerpop/gremlin.IoOptionsKeys");
  
  public static final hydra.core.Name FIELD_NAME_READER = new hydra.core.Name("reader");
  
  public static final hydra.core.Name FIELD_NAME_WRITER = new hydra.core.Name("writer");
  
  private IoOptionsKeys () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Reader instance) ;
    
    R visit(Writer instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(IoOptionsKeys instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Reader instance) {
      return otherwise((instance));
    }
    
    default R visit(Writer instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Reader extends hydra.ext.org.apache.tinkerpop.gremlin.IoOptionsKeys implements Serializable {
    public Reader () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Reader)) {
        return false;
      }
      Reader o = (Reader) (other);
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
  
  public static final class Writer extends hydra.ext.org.apache.tinkerpop.gremlin.IoOptionsKeys implements Serializable {
    public Writer () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Writer)) {
        return false;
      }
      Writer o = (Writer) (other);
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