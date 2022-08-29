package hydra.ext.coq.syntax;

public abstract class Arg {
  private Arg () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Ident instance) ;
    
    R visit(Natural instance) ;
    
    R visit(Term instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Arg instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Ident instance) {
      return otherwise((instance));
    }
    
    default R visit(Natural instance) {
      return otherwise((instance));
    }
    
    default R visit(Term instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Ident extends hydra.ext.coq.syntax.Arg {
    public final hydra.ext.coq.syntax.IdentArg value;
    
    public Ident (hydra.ext.coq.syntax.IdentArg value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Ident)) {
        return false;
      }
      Ident o = (Ident) (other);
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
  
  public static final class Natural extends hydra.ext.coq.syntax.Arg {
    public final hydra.ext.coq.syntax.NaturalArg value;
    
    public Natural (hydra.ext.coq.syntax.NaturalArg value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Natural)) {
        return false;
      }
      Natural o = (Natural) (other);
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
  
  public static final class Term extends hydra.ext.coq.syntax.Arg {
    public final hydra.ext.coq.syntax.Term1 value;
    
    public Term (hydra.ext.coq.syntax.Term1 value) {
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