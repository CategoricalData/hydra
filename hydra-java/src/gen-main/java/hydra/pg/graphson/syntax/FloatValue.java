// Note: this is an automatically generated file. Do not edit.

package hydra.pg.graphson.syntax;

import java.io.Serializable;

public abstract class FloatValue implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.pg.graphson.syntax.FloatValue");
  
  public static final hydra.core.Name FIELD_NAME_FINITE = new hydra.core.Name("finite");
  
  public static final hydra.core.Name FIELD_NAME_INFINITY = new hydra.core.Name("infinity");
  
  public static final hydra.core.Name FIELD_NAME_NEGATIVE_INFINITY = new hydra.core.Name("negativeInfinity");
  
  public static final hydra.core.Name FIELD_NAME_NOT_A_NUMBER = new hydra.core.Name("notANumber");
  
  private FloatValue () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Finite instance) ;
    
    R visit(Infinity instance) ;
    
    R visit(NegativeInfinity instance) ;
    
    R visit(NotANumber instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(FloatValue instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Finite instance) {
      return otherwise((instance));
    }
    
    default R visit(Infinity instance) {
      return otherwise((instance));
    }
    
    default R visit(NegativeInfinity instance) {
      return otherwise((instance));
    }
    
    default R visit(NotANumber instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Finite extends hydra.pg.graphson.syntax.FloatValue implements Serializable {
    public final Float value;
    
    public Finite (Float value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Finite)) {
        return false;
      }
      Finite o = (Finite) (other);
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
  
  public static final class Infinity extends hydra.pg.graphson.syntax.FloatValue implements Serializable {
    public Infinity () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Infinity)) {
        return false;
      }
      Infinity o = (Infinity) (other);
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
  
  public static final class NegativeInfinity extends hydra.pg.graphson.syntax.FloatValue implements Serializable {
    public NegativeInfinity () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NegativeInfinity)) {
        return false;
      }
      NegativeInfinity o = (NegativeInfinity) (other);
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
  
  public static final class NotANumber extends hydra.pg.graphson.syntax.FloatValue implements Serializable {
    public NotANumber () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NotANumber)) {
        return false;
      }
      NotANumber o = (NotANumber) (other);
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