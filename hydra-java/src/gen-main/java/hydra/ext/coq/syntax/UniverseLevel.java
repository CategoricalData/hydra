package hydra.ext.coq.syntax;

public abstract class UniverseLevel {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/coq/syntax.UniverseLevel");
  
  private UniverseLevel () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Set instance) ;
    
    R visit(Prop instance) ;
    
    R visit(Type instance) ;
    
    R visit(Ignored instance) ;
    
    R visit(Qualid instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(UniverseLevel instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Set instance) {
      return otherwise((instance));
    }
    
    default R visit(Prop instance) {
      return otherwise((instance));
    }
    
    default R visit(Type instance) {
      return otherwise((instance));
    }
    
    default R visit(Ignored instance) {
      return otherwise((instance));
    }
    
    default R visit(Qualid instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Set extends hydra.ext.coq.syntax.UniverseLevel {
    public Set () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Set)) {
        return false;
      }
      Set o = (Set) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Prop extends hydra.ext.coq.syntax.UniverseLevel {
    public Prop () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Prop)) {
        return false;
      }
      Prop o = (Prop) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Type extends hydra.ext.coq.syntax.UniverseLevel {
    public Type () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Type)) {
        return false;
      }
      Type o = (Type) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Ignored extends hydra.ext.coq.syntax.UniverseLevel {
    public Ignored () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Ignored)) {
        return false;
      }
      Ignored o = (Ignored) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Qualid extends hydra.ext.coq.syntax.UniverseLevel {
    public final hydra.ext.coq.syntax.Qualid value;
    
    public Qualid (hydra.ext.coq.syntax.Qualid value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Qualid)) {
        return false;
      }
      Qualid o = (Qualid) (other);
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