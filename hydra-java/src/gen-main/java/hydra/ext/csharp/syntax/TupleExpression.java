// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class TupleExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.TupleExpression");
  
  public static final hydra.core.Name FIELD_NAME_ELEMENTS = new hydra.core.Name("elements");
  
  public static final hydra.core.Name FIELD_NAME_DECONSTRUCTION = new hydra.core.Name("deconstruction");
  
  private TupleExpression () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Elements_ instance) ;
    
    R visit(Deconstruction instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TupleExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Elements_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Deconstruction instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Elements_ extends hydra.ext.csharp.syntax.TupleExpression implements Serializable {
    public final java.util.List<hydra.ext.csharp.syntax.TupleElement> value;
    
    public Elements_ (java.util.List<hydra.ext.csharp.syntax.TupleElement> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Elements_)) {
        return false;
      }
      Elements_ o = (Elements_) (other);
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
  
  public static final class Deconstruction extends hydra.ext.csharp.syntax.TupleExpression implements Serializable {
    public final hydra.ext.csharp.syntax.DeconstructionTuple value;
    
    public Deconstruction (hydra.ext.csharp.syntax.DeconstructionTuple value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Deconstruction)) {
        return false;
      }
      Deconstruction o = (Deconstruction) (other);
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