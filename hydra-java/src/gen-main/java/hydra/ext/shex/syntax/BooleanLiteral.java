package hydra.ext.shex.syntax;

public abstract class BooleanLiteral {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.BooleanLiteral");
  
  private BooleanLiteral () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(True instance) ;
    
    R visit(False instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(BooleanLiteral instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(True instance) {
      return otherwise((instance));
    }
    
    default R visit(False instance) {
      return otherwise((instance));
    }
  }
  
  public static final class True extends hydra.ext.shex.syntax.BooleanLiteral {
    public True () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof True)) {
        return false;
      }
      True o = (True) (other);
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
  
  public static final class False extends hydra.ext.shex.syntax.BooleanLiteral {
    public False () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof False)) {
        return false;
      }
      False o = (False) (other);
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