package hydra.ext.coq.syntax;

public abstract class Term100 {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/coq/syntax.Term100");
  
  private Term100 () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Cast instance) ;
    
    R visit(Term10 instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Term100 instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Cast instance) {
      return otherwise((instance));
    }
    
    default R visit(Term10 instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Cast extends hydra.ext.coq.syntax.Term100 {
    public final hydra.ext.coq.syntax.TypeCast value;
    
    public Cast (hydra.ext.coq.syntax.TypeCast value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Cast)) {
        return false;
      }
      Cast o = (Cast) (other);
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
  
  public static final class Term10 extends hydra.ext.coq.syntax.Term100 {
    public final hydra.ext.coq.syntax.Term10 value;
    
    public Term10 (hydra.ext.coq.syntax.Term10 value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Term10)) {
        return false;
      }
      Term10 o = (Term10) (other);
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