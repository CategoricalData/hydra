package hydra.util.codetree.ast;

/**
 * An abstract expression
 */
public abstract class Expr {
  private Expr () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Const instance) ;
    
    R visit(Op instance) ;
    
    R visit(Brackets instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Expr instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Const instance) {
      return otherwise((instance));
    }
    
    default R visit(Op instance) {
      return otherwise((instance));
    }
    
    default R visit(Brackets instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Const extends Expr {
    public final Symbol value;
    
    public Const (Symbol value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Const)) {
        return false;
      }
      Const o = (Const) (other);
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
  
  public static final class Op extends Expr {
    public final OpExpr value;
    
    public Op (OpExpr value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Op)) {
        return false;
      }
      Op o = (Op) (other);
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
  
  public static final class Brackets extends Expr {
    public final BracketExpr value;
    
    public Brackets (BracketExpr value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Brackets)) {
        return false;
      }
      Brackets o = (Brackets) (other);
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