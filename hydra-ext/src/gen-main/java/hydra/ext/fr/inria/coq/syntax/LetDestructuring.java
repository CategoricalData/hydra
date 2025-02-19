// Note: this is an automatically generated file. Do not edit.

package hydra.ext.fr.inria.coq.syntax;

import java.io.Serializable;

public abstract class LetDestructuring implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.fr.inria.coq.syntax.LetDestructuring");
  
  public static final hydra.core.Name FIELD_NAME_VARIANT1 = new hydra.core.Name("variant1");
  
  public static final hydra.core.Name FIELD_NAME_VARIANT2 = new hydra.core.Name("variant2");
  
  public static final hydra.core.Name FIELD_NAME_VARIANT3 = new hydra.core.Name("variant3");
  
  private LetDestructuring () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Variant1 instance) ;
    
    R visit(Variant2 instance) ;
    
    R visit(Variant3 instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(LetDestructuring instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Variant1 instance) {
      return otherwise((instance));
    }
    
    default R visit(Variant2 instance) {
      return otherwise((instance));
    }
    
    default R visit(Variant3 instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Variant1 extends hydra.ext.fr.inria.coq.syntax.LetDestructuring implements Serializable {
    public final hydra.ext.fr.inria.coq.syntax.LetDestructuring_Variant1 value;
    
    public Variant1 (hydra.ext.fr.inria.coq.syntax.LetDestructuring_Variant1 value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Variant1)) {
        return false;
      }
      Variant1 o = (Variant1) (other);
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
  
  public static final class Variant2 extends hydra.ext.fr.inria.coq.syntax.LetDestructuring implements Serializable {
    public final hydra.ext.fr.inria.coq.syntax.LetDestructuring_Variant2 value;
    
    public Variant2 (hydra.ext.fr.inria.coq.syntax.LetDestructuring_Variant2 value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Variant2)) {
        return false;
      }
      Variant2 o = (Variant2) (other);
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
  
  public static final class Variant3 extends hydra.ext.fr.inria.coq.syntax.LetDestructuring implements Serializable {
    public final hydra.ext.fr.inria.coq.syntax.LetDestructuring_Variant3 value;
    
    public Variant3 (hydra.ext.fr.inria.coq.syntax.LetDestructuring_Variant3 value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Variant3)) {
        return false;
      }
      Variant3 o = (Variant3) (other);
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