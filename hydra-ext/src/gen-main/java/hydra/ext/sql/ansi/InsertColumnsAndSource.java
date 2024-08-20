// Note: this is an automatically generated file. Do not edit.

package hydra.ext.sql.ansi;

import java.io.Serializable;

public abstract class InsertColumnsAndSource implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/sql/ansi.InsertColumnsAndSource");
  
  public static final hydra.core.Name FIELD_NAME_SUBQUERY = new hydra.core.Name("subquery");
  
  public static final hydra.core.Name FIELD_NAME_CONSTRUCTOR = new hydra.core.Name("constructor");
  
  public static final hydra.core.Name FIELD_NAME_DEFAULT = new hydra.core.Name("default");
  
  private InsertColumnsAndSource () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Subquery instance) ;
    
    R visit(Constructor instance) ;
    
    R visit(Default instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(InsertColumnsAndSource instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Subquery instance) {
      return otherwise((instance));
    }
    
    default R visit(Constructor instance) {
      return otherwise((instance));
    }
    
    default R visit(Default instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Subquery extends hydra.ext.sql.ansi.InsertColumnsAndSource implements Serializable {
    public final hydra.ext.sql.ansi.FromSubquery value;
    
    public Subquery (hydra.ext.sql.ansi.FromSubquery value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Subquery)) {
        return false;
      }
      Subquery o = (Subquery) (other);
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
  
  public static final class Constructor extends hydra.ext.sql.ansi.InsertColumnsAndSource implements Serializable {
    public final hydra.ext.sql.ansi.FromConstructor value;
    
    public Constructor (hydra.ext.sql.ansi.FromConstructor value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Constructor)) {
        return false;
      }
      Constructor o = (Constructor) (other);
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
  
  public static final class Default extends hydra.ext.sql.ansi.InsertColumnsAndSource implements Serializable {
    public final hydra.ext.sql.ansi.FromDefault value;
    
    public Default (hydra.ext.sql.ansi.FromDefault value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Default)) {
        return false;
      }
      Default o = (Default) (other);
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