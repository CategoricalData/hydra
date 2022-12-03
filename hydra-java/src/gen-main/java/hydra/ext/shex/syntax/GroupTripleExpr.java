package hydra.ext.shex.syntax;

public abstract class GroupTripleExpr {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.GroupTripleExpr");
  
  private GroupTripleExpr () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(SingleElementGroup instance) ;
    
    R visit(MultiElementGroup instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(GroupTripleExpr instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(SingleElementGroup instance) {
      return otherwise((instance));
    }
    
    default R visit(MultiElementGroup instance) {
      return otherwise((instance));
    }
  }
  
  public static final class SingleElementGroup extends hydra.ext.shex.syntax.GroupTripleExpr {
    public final hydra.ext.shex.syntax.SingleElementGroup value;
    
    public SingleElementGroup (hydra.ext.shex.syntax.SingleElementGroup value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SingleElementGroup)) {
        return false;
      }
      SingleElementGroup o = (SingleElementGroup) (other);
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
  
  public static final class MultiElementGroup extends hydra.ext.shex.syntax.GroupTripleExpr {
    public final hydra.ext.shex.syntax.MultiElementGroup value;
    
    public MultiElementGroup (hydra.ext.shex.syntax.MultiElementGroup value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MultiElementGroup)) {
        return false;
      }
      MultiElementGroup o = (MultiElementGroup) (other);
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