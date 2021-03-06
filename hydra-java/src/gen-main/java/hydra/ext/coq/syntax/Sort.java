package hydra.ext.coq.syntax;

/**
 * The types of types are called sorts.
 */
public abstract class Sort {
  private Sort () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Set instance) ;
    
    R visit(Prop instance) ;
    
    R visit(SProp instance) ;
    
    R visit(Type instance) ;
    
    R visit(TypeWithAnyUniverse instance) ;
    
    R visit(TypeWithUniverse instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Sort instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Set instance) {
      return otherwise((instance));
    }
    
    default R visit(Prop instance) {
      return otherwise((instance));
    }
    
    default R visit(SProp instance) {
      return otherwise((instance));
    }
    
    default R visit(Type instance) {
      return otherwise((instance));
    }
    
    default R visit(TypeWithAnyUniverse instance) {
      return otherwise((instance));
    }
    
    default R visit(TypeWithUniverse instance) {
      return otherwise((instance));
    }
  }
  
  /**
   * The sort π²πΎπ intends to be the type of small sets.
   */
  public static final class Set extends Sort {
    /**
     * The sort π²πΎπ intends to be the type of small sets.
     */
    public final java.lang.Void value;
    
    public Set (java.lang.Void value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Set)) {
        return false;
      }
      Set o = (Set) (other);
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
  
  /**
   * The sort π―πππ intends to be the type of logical propositions.
   */
  public static final class Prop extends Sort {
    /**
     * The sort π―πππ intends to be the type of logical propositions.
     */
    public final java.lang.Void value;
    
    public Prop (java.lang.Void value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Prop)) {
        return false;
      }
      Prop o = (Prop) (other);
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
  
  /**
   * The sort π²π―πππ is like π―πππ but the propositions in π²π―πππ are known to have irrelevant proofs (all proofs are equal).
   */
  public static final class SProp extends Sort {
    /**
     * The sort π²π―πππ is like π―πππ but the propositions in π²π―πππ are known to have irrelevant proofs (all proofs are equal).
     */
    public final java.lang.Void value;
    
    public SProp (java.lang.Void value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SProp)) {
        return false;
      }
      SProp o = (SProp) (other);
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
  
  public static final class Type extends Sort {
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
  
  public static final class TypeWithAnyUniverse extends Sort {
    public TypeWithAnyUniverse () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TypeWithAnyUniverse)) {
        return false;
      }
      TypeWithAnyUniverse o = (TypeWithAnyUniverse) (other);
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
  
  public static final class TypeWithUniverse extends Sort {
    public final Universe value;
    
    public TypeWithUniverse (Universe value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TypeWithUniverse)) {
        return false;
      }
      TypeWithUniverse o = (TypeWithUniverse) (other);
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