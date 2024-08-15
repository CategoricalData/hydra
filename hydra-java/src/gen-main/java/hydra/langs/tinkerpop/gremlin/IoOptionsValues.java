// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class IoOptionsValues implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.IoOptionsValues");
  
  public static final hydra.core.Name FIELD_NAME_GRYO = new hydra.core.Name("gryo");
  
  public static final hydra.core.Name FIELD_NAME_GRAPHSON = new hydra.core.Name("graphson");
  
  public static final hydra.core.Name FIELD_NAME_GRAPHML = new hydra.core.Name("graphml");
  
  private IoOptionsValues () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Gryo instance) ;
    
    R visit(Graphson instance) ;
    
    R visit(Graphml instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(IoOptionsValues instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Gryo instance) {
      return otherwise((instance));
    }
    
    default R visit(Graphson instance) {
      return otherwise((instance));
    }
    
    default R visit(Graphml instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Gryo extends hydra.langs.tinkerpop.gremlin.IoOptionsValues implements Serializable {
    public Gryo () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Gryo)) {
        return false;
      }
      Gryo o = (Gryo) (other);
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
  
  public static final class Graphson extends hydra.langs.tinkerpop.gremlin.IoOptionsValues implements Serializable {
    public Graphson () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Graphson)) {
        return false;
      }
      Graphson o = (Graphson) (other);
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
  
  public static final class Graphml extends hydra.langs.tinkerpop.gremlin.IoOptionsValues implements Serializable {
    public Graphml () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Graphml)) {
        return false;
      }
      Graphml o = (Graphml) (other);
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