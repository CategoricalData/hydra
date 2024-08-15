// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class Keyword implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.Keyword");
  
  public static final hydra.core.Name FIELD_NAME_EDGES = new hydra.core.Name("edges");
  
  public static final hydra.core.Name FIELD_NAME_KEYS = new hydra.core.Name("keys");
  
  public static final hydra.core.Name FIELD_NAME_NEW = new hydra.core.Name("new");
  
  public static final hydra.core.Name FIELD_NAME_VALUES = new hydra.core.Name("values");
  
  private Keyword () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Edges instance) ;
    
    R visit(Keys instance) ;
    
    R visit(New instance) ;
    
    R visit(Values instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Keyword instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Edges instance) {
      return otherwise((instance));
    }
    
    default R visit(Keys instance) {
      return otherwise((instance));
    }
    
    default R visit(New instance) {
      return otherwise((instance));
    }
    
    default R visit(Values instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Edges extends hydra.langs.tinkerpop.gremlin.Keyword implements Serializable {
    public Edges () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Edges)) {
        return false;
      }
      Edges o = (Edges) (other);
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
  
  public static final class Keys extends hydra.langs.tinkerpop.gremlin.Keyword implements Serializable {
    public Keys () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Keys)) {
        return false;
      }
      Keys o = (Keys) (other);
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
  
  public static final class New extends hydra.langs.tinkerpop.gremlin.Keyword implements Serializable {
    public New () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof New)) {
        return false;
      }
      New o = (New) (other);
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
  
  public static final class Values extends hydra.langs.tinkerpop.gremlin.Keyword implements Serializable {
    public Values () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Values)) {
        return false;
      }
      Values o = (Values) (other);
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