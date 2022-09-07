package hydra.ext.coq.syntax;

public abstract class CofixQual {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/coq/syntax.CofixQual");
  
  private CofixQual () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(In instance) ;
    
    R visit(With instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(CofixQual instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(In instance) {
      return otherwise((instance));
    }
    
    default R visit(With instance) {
      return otherwise((instance));
    }
  }
  
  public static final class In extends hydra.ext.coq.syntax.CofixQual {
    public final hydra.ext.coq.syntax.Term value;
    
    public In (hydra.ext.coq.syntax.Term value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof In)) {
        return false;
      }
      In o = (In) (other);
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
  
  public static final class With extends hydra.ext.coq.syntax.CofixQual {
    public final hydra.ext.coq.syntax.CofixWith value;
    
    public With (hydra.ext.coq.syntax.CofixWith value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof With)) {
        return false;
      }
      With o = (With) (other);
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