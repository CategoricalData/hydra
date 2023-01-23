package hydra.ext.shex.syntax;

public abstract class InlineShapeAtom {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.InlineShapeAtom");
  
  private InlineShapeAtom () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Sequence instance) ;
    
    R visit(Sequence2 instance) ;
    
    R visit(Sequence3 instance) ;
    
    R visit(Period instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(InlineShapeAtom instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Sequence instance) {
      return otherwise((instance));
    }
    
    default R visit(Sequence2 instance) {
      return otherwise((instance));
    }
    
    default R visit(Sequence3 instance) {
      return otherwise((instance));
    }
    
    default R visit(Period instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Sequence extends hydra.ext.shex.syntax.InlineShapeAtom {
    public final hydra.ext.shex.syntax.InlineShapeAtom_Sequence value;
    
    public Sequence (hydra.ext.shex.syntax.InlineShapeAtom_Sequence value) {
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
  
  public static final class Sequence2 extends hydra.ext.shex.syntax.InlineShapeAtom {
    public final hydra.ext.shex.syntax.InlineShapeAtom_Sequence2 value;
    
    public Sequence2 (hydra.ext.shex.syntax.InlineShapeAtom_Sequence2 value) {
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
  
  public static final class Sequence3 extends hydra.ext.shex.syntax.InlineShapeAtom {
    public final hydra.ext.shex.syntax.ShapeExpression value;
    
    public Sequence3 (hydra.ext.shex.syntax.ShapeExpression value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Sequence3)) {
        return false;
      }
      Sequence3 o = (Sequence3) (other);
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
  
  public static final class Period extends hydra.ext.shex.syntax.InlineShapeAtom {
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