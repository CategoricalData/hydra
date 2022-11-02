package hydra.ext.shex.syntax;

public abstract class UnaryTripleExpr {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.UnaryTripleExpr");
  
  private UnaryTripleExpr () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Sequence instance) ;
    
    R visit(Include instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(UnaryTripleExpr instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Sequence instance) {
      return otherwise((instance));
    }
    
    default R visit(Include instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Sequence extends hydra.ext.shex.syntax.UnaryTripleExpr {
    public final hydra.ext.shex.syntax.UnaryTripleExpr_Sequence value;
    
    public Sequence (hydra.ext.shex.syntax.UnaryTripleExpr_Sequence value) {
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
  
  public static final class Include extends hydra.ext.shex.syntax.UnaryTripleExpr {
    public final hydra.ext.shex.syntax.Include value;
    
    public Include (hydra.ext.shex.syntax.Include value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Include)) {
        return false;
      }
      Include o = (Include) (other);
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