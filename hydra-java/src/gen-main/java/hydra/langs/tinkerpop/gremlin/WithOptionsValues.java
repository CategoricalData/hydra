// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class WithOptionsValues implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.WithOptionsValues");
  
  private WithOptionsValues () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Tokens instance) ;
    
    R visit(None instance) ;
    
    R visit(Ids instance) ;
    
    R visit(Labels instance) ;
    
    R visit(Keys instance) ;
    
    R visit(Values instance) ;
    
    R visit(All instance) ;
    
    R visit(List instance) ;
    
    R visit(Map instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(WithOptionsValues instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Tokens instance) {
      return otherwise((instance));
    }
    
    default R visit(None instance) {
      return otherwise((instance));
    }
    
    default R visit(Ids instance) {
      return otherwise((instance));
    }
    
    default R visit(Labels instance) {
      return otherwise((instance));
    }
    
    default R visit(Keys instance) {
      return otherwise((instance));
    }
    
    default R visit(Values instance) {
      return otherwise((instance));
    }
    
    default R visit(All instance) {
      return otherwise((instance));
    }
    
    default R visit(List instance) {
      return otherwise((instance));
    }
    
    default R visit(Map instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Tokens extends hydra.langs.tinkerpop.gremlin.WithOptionsValues implements Serializable {
    public Tokens () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Tokens)) {
        return false;
      }
      Tokens o = (Tokens) (other);
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
  
  public static final class None extends hydra.langs.tinkerpop.gremlin.WithOptionsValues implements Serializable {
    public None () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof None)) {
        return false;
      }
      None o = (None) (other);
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
  
  public static final class Ids extends hydra.langs.tinkerpop.gremlin.WithOptionsValues implements Serializable {
    public Ids () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Ids)) {
        return false;
      }
      Ids o = (Ids) (other);
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
  
  public static final class Labels extends hydra.langs.tinkerpop.gremlin.WithOptionsValues implements Serializable {
    public Labels () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Labels)) {
        return false;
      }
      Labels o = (Labels) (other);
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
  
  public static final class Keys extends hydra.langs.tinkerpop.gremlin.WithOptionsValues implements Serializable {
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
  
  public static final class Values extends hydra.langs.tinkerpop.gremlin.WithOptionsValues implements Serializable {
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
  
  public static final class All extends hydra.langs.tinkerpop.gremlin.WithOptionsValues implements Serializable {
    public All () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof All)) {
        return false;
      }
      All o = (All) (other);
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
  
  public static final class List extends hydra.langs.tinkerpop.gremlin.WithOptionsValues implements Serializable {
    public List () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof List)) {
        return false;
      }
      List o = (List) (other);
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
  
  public static final class Map extends hydra.langs.tinkerpop.gremlin.WithOptionsValues implements Serializable {
    public Map () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Map)) {
        return false;
      }
      Map o = (Map) (other);
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