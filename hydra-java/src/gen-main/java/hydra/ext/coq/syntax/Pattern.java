package hydra.ext.coq.syntax;

public abstract class Pattern {
  private Pattern () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Pattern_ instance) ;
    
    R visit(Term instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Pattern instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Pattern_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Term instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Pattern_ extends hydra.ext.coq.syntax.Pattern {
    public final hydra.ext.coq.syntax.Pattern10 value;
    
    public Pattern_ (hydra.ext.coq.syntax.Pattern10 value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Pattern_)) {
        return false;
      }
      Pattern_ o = (Pattern_) (other);
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
  
  public static final class Term extends hydra.ext.coq.syntax.Pattern {
    public final java.util.Optional<hydra.ext.coq.syntax.Term> value;
    
    public Term (java.util.Optional<hydra.ext.coq.syntax.Term> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Term)) {
        return false;
      }
      Term o = (Term) (other);
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