package hydra.ext.coq.syntax;

public abstract class Binder {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/coq/syntax.Binder");
  
  private Binder () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Name instance) ;
    
    R visit(Type instance) ;
    
    R visit(Term instance) ;
    
    R visit(Implicit instance) ;
    
    R visit(Generalizing instance) ;
    
    R visit(Pattern instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Binder instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Name instance) {
      return otherwise((instance));
    }
    
    default R visit(Type instance) {
      return otherwise((instance));
    }
    
    default R visit(Term instance) {
      return otherwise((instance));
    }
    
    default R visit(Implicit instance) {
      return otherwise((instance));
    }
    
    default R visit(Generalizing instance) {
      return otherwise((instance));
    }
    
    default R visit(Pattern instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Name extends hydra.ext.coq.syntax.Binder {
    public final hydra.ext.coq.syntax.Name value;
    
    public Name (hydra.ext.coq.syntax.Name value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Name)) {
        return false;
      }
      Name o = (Name) (other);
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
  
  public static final class Type extends hydra.ext.coq.syntax.Binder {
    public final hydra.ext.coq.syntax.TypeBinders value;
    
    public Type (hydra.ext.coq.syntax.TypeBinders value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Type)) {
        return false;
      }
      Type o = (Type) (other);
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
  
  public static final class Term extends hydra.ext.coq.syntax.Binder {
    public final hydra.ext.coq.syntax.LetBinder value;
    
    public Term (hydra.ext.coq.syntax.LetBinder value) {
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
  
  public static final class Implicit extends hydra.ext.coq.syntax.Binder {
    public final hydra.ext.coq.syntax.ImplicitBinders value;
    
    public Implicit (hydra.ext.coq.syntax.ImplicitBinders value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Implicit)) {
        return false;
      }
      Implicit o = (Implicit) (other);
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
  
  public static final class Generalizing extends hydra.ext.coq.syntax.Binder {
    public final hydra.ext.coq.syntax.GeneralizingBinder value;
    
    public Generalizing (hydra.ext.coq.syntax.GeneralizingBinder value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Generalizing)) {
        return false;
      }
      Generalizing o = (Generalizing) (other);
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
  
  public static final class Pattern extends hydra.ext.coq.syntax.Binder {
    public final hydra.ext.coq.syntax.Pattern0 value;
    
    public Pattern (hydra.ext.coq.syntax.Pattern0 value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Pattern)) {
        return false;
      }
      Pattern o = (Pattern) (other);
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