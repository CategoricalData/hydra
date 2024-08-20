// Note: this is an automatically generated file. Do not edit.

package hydra.ext.shex.syntax;

import java.io.Serializable;

public abstract class ShapeAtom implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/shex/syntax.ShapeAtom");
  
  public static final hydra.core.Name FIELD_NAME_SEQUENCE = new hydra.core.Name("sequence");
  
  public static final hydra.core.Name FIELD_NAME_SHAPE_OR_REF = new hydra.core.Name("shapeOrRef");
  
  public static final hydra.core.Name FIELD_NAME_SEQUENCE2 = new hydra.core.Name("sequence2");
  
  public static final hydra.core.Name FIELD_NAME_PERIOD = new hydra.core.Name("period");
  
  private ShapeAtom () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Sequence instance) ;
    
    R visit(ShapeOrRef instance) ;
    
    R visit(Sequence2 instance) ;
    
    R visit(Period instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ShapeAtom instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Sequence instance) {
      return otherwise((instance));
    }
    
    default R visit(ShapeOrRef instance) {
      return otherwise((instance));
    }
    
    default R visit(Sequence2 instance) {
      return otherwise((instance));
    }
    
    default R visit(Period instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Sequence extends hydra.ext.shex.syntax.ShapeAtom implements Serializable {
    public final hydra.ext.shex.syntax.ShapeAtom_Sequence value;
    
    public Sequence (hydra.ext.shex.syntax.ShapeAtom_Sequence value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Sequence)) {
        return false;
      }
      Sequence o = (Sequence) (other);
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
  
  public static final class ShapeOrRef extends hydra.ext.shex.syntax.ShapeAtom implements Serializable {
    public final hydra.ext.shex.syntax.ShapeOrRef value;
    
    public ShapeOrRef (hydra.ext.shex.syntax.ShapeOrRef value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ShapeOrRef)) {
        return false;
      }
      ShapeOrRef o = (ShapeOrRef) (other);
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
  
  public static final class Sequence2 extends hydra.ext.shex.syntax.ShapeAtom implements Serializable {
    public final hydra.ext.shex.syntax.ShapeExpression value;
    
    public Sequence2 (hydra.ext.shex.syntax.ShapeExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Sequence2)) {
        return false;
      }
      Sequence2 o = (Sequence2) (other);
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
  
  public static final class Period extends hydra.ext.shex.syntax.ShapeAtom implements Serializable {
    public Period () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Period)) {
        return false;
      }
      Period o = (Period) (other);
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