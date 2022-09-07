package hydra.ext.java.syntax;

public abstract class AssertStatement {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.AssertStatement");
  
  private AssertStatement () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Single instance) ;
    
    R visit(Pair instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(AssertStatement instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Single instance) {
      return otherwise((instance));
    }
    
    default R visit(Pair instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Single extends hydra.ext.java.syntax.AssertStatement {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.Single");
    
    public final hydra.ext.java.syntax.Expression value;
    
    public Single (hydra.ext.java.syntax.Expression value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Single)) {
        return false;
      }
      Single o = (Single) (other);
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
  
  public static final class Pair extends hydra.ext.java.syntax.AssertStatement {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.Pair");
    
    public final hydra.ext.java.syntax.AssertStatement_Pair value;
    
    public Pair (hydra.ext.java.syntax.AssertStatement_Pair value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Pair)) {
        return false;
      }
      Pair o = (Pair) (other);
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