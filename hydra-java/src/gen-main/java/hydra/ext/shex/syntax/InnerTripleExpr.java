package hydra.ext.shex.syntax;

public abstract class InnerTripleExpr {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.InnerTripleExpr");
  
  private InnerTripleExpr () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(MultiElementGroup instance) ;
    
    R visit(MultiElementOneOf instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(InnerTripleExpr instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(MultiElementGroup instance) {
      return otherwise((instance));
    }
    
    default R visit(MultiElementOneOf instance) {
      return otherwise((instance));
    }
  }
  
  public static final class MultiElementGroup extends hydra.ext.shex.syntax.InnerTripleExpr {
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
  
  public static final class MultiElementOneOf extends hydra.ext.shex.syntax.InnerTripleExpr {
    public final hydra.ext.shex.syntax.MultiElementOneOf value;
    
    public MultiElementOneOf (hydra.ext.shex.syntax.MultiElementOneOf value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MultiElementOneOf)) {
        return false;
      }
      MultiElementOneOf o = (MultiElementOneOf) (other);
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