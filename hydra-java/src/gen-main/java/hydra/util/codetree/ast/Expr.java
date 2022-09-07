package hydra.util.codetree.ast;

/**
 * An abstract expression
 */
public abstract class Expr {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/util/codetree/ast.Expr");
  
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
  
  public static final class Const extends hydra.util.codetree.ast.Expr {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/util/codetree/ast.Const");
    
    public final hydra.util.codetree.ast.Symbol value;
    
    public Const (hydra.util.codetree.ast.Symbol value) {
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
  
  public static final class Op extends hydra.util.codetree.ast.Expr {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/util/codetree/ast.Op");
    
    public final hydra.util.codetree.ast.OpExpr value;
    
    public Op (hydra.util.codetree.ast.OpExpr value) {
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
  
  public static final class Brackets extends hydra.util.codetree.ast.Expr {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/util/codetree/ast.Brackets");
    
    public final hydra.util.codetree.ast.BracketExpr value;
    
    public Brackets (hydra.util.codetree.ast.BracketExpr value) {
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