package hydra.ext.shex.syntax;

public abstract class InlineShapeOrRef {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.InlineShapeOrRef");
  
  private InlineShapeOrRef () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(InlineShapeDefinition instance) ;
    
    R visit(AtpNameLn instance) ;
    
    R visit(AtpNameNs instance) ;
    
    R visit(Sequence instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(InlineShapeOrRef instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(InlineShapeDefinition instance) {
      return otherwise((instance));
    }
    
    default R visit(AtpNameLn instance) {
      return otherwise((instance));
    }
    
    default R visit(AtpNameNs instance) {
      return otherwise((instance));
    }
    
    default R visit(Sequence instance) {
      return otherwise((instance));
    }
  }
  
  public static final class InlineShapeDefinition extends hydra.ext.shex.syntax.InlineShapeOrRef {
    public final hydra.ext.shex.syntax.InlineShapeDefinition value;
    
    public InlineShapeDefinition (hydra.ext.shex.syntax.InlineShapeDefinition value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof InlineShapeDefinition)) {
        return false;
      }
      InlineShapeDefinition o = (InlineShapeDefinition) (other);
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
  
  public static final class AtpNameLn extends hydra.ext.shex.syntax.InlineShapeOrRef {
    public final hydra.ext.shex.syntax.AtpNameLn value;
    
    public AtpNameLn (hydra.ext.shex.syntax.AtpNameLn value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AtpNameLn)) {
        return false;
      }
      AtpNameLn o = (AtpNameLn) (other);
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
  
  public static final class AtpNameNs extends hydra.ext.shex.syntax.InlineShapeOrRef {
    public final hydra.ext.shex.syntax.AtpNameNs value;
    
    public AtpNameNs (hydra.ext.shex.syntax.AtpNameNs value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AtpNameNs)) {
        return false;
      }
      AtpNameNs o = (AtpNameNs) (other);
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
  
  public static final class Sequence extends hydra.ext.shex.syntax.InlineShapeOrRef {
    public final hydra.ext.shex.syntax.ShapeExprLabel value;
    
    public Sequence (hydra.ext.shex.syntax.ShapeExprLabel value) {
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
}