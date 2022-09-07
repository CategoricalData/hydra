package hydra.ext.java.syntax;

public abstract class LambdaBody {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.LambdaBody");
  
  private LambdaBody () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Expression instance) ;
    
    R visit(Block instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(LambdaBody instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Expression instance) {
      return otherwise((instance));
    }
    
    default R visit(Block instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Expression extends hydra.ext.java.syntax.LambdaBody {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.Expression");
    
    public final hydra.ext.java.syntax.Expression value;
    
    public Expression (hydra.ext.java.syntax.Expression value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Expression)) {
        return false;
      }
      Expression o = (Expression) (other);
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
  
  public static final class Block extends hydra.ext.java.syntax.LambdaBody {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.Block");
    
    public final hydra.ext.java.syntax.Block value;
    
    public Block (hydra.ext.java.syntax.Block value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Block)) {
        return false;
      }
      Block o = (Block) (other);
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