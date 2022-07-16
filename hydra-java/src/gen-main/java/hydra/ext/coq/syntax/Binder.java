package hydra.ext.coq.syntax;

public abstract class Binder {
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
  
  public static final class Name extends Binder {
    public final Name value;
    
    public Name (Name value) {
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
  
  public static final class Type extends Binder {
    public final TypeBinders value;
    
    public Type (TypeBinders value) {
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
  
  public static final class Term extends Binder {
    public final LetBinder value;
    
    public Term (LetBinder value) {
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
  
  public static final class Implicit extends Binder {
    public final ImplicitBinders value;
    
    public Implicit (ImplicitBinders value) {
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
  
  public static final class Generalizing extends Binder {
    public final GeneralizingBinder value;
    
    public Generalizing (GeneralizingBinder value) {
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
  
  public static final class Pattern extends Binder {
    public final Pattern0 value;
    
    public Pattern (Pattern0 value) {
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