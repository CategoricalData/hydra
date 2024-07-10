// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class IoOptionsKeys implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.IoOptionsKeys");
  
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
  
  public static final class Reader extends hydra.langs.tinkerpop.gremlin.IoOptionsKeys implements Serializable {
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
  
  public static final class Writer extends hydra.langs.tinkerpop.gremlin.IoOptionsKeys implements Serializable {
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