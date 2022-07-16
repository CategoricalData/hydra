package hydra.ext.coq.syntax;

public abstract class OneTerm {
  private OneTerm () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Explicit instance) ;
    
    R visit(Term1 instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(OneTerm instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Explicit instance) {
      return otherwise((instance));
    }
    
    default R visit(Term1 instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Explicit extends OneTerm {
    public final QualidAnnotated value;
    
    public Explicit (QualidAnnotated value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Explicit)) {
        return false;
      }
      Explicit o = (Explicit) (other);
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
  
  public static final class Term1 extends OneTerm {
    public final Term1 value;
    
    public Term1 (Term1 value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Term1)) {
        return false;
      }
      Term1 o = (Term1) (other);
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