// Note: this is an automatically generated file. Do not edit.

package hydra.ext.fr.inria.coq.syntax;

import java.io.Serializable;

public abstract class GeneralizingBinder implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/fr/inria/coq/syntax.GeneralizingBinder");
  
  public static final hydra.core.Name FIELD_NAME_EXPLICIT = new hydra.core.Name("explicit");
  
  public static final hydra.core.Name FIELD_NAME_IMPLICIT_MAXIMALLY_INSERTED = new hydra.core.Name("implicitMaximallyInserted");
  
  public static final hydra.core.Name FIELD_NAME_IMPLICIT_NON_MAXIMALLY_INSERTED = new hydra.core.Name("implicitNonMaximallyInserted");
  
  private GeneralizingBinder () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Explicit instance) ;
    
    R visit(ImplicitMaximallyInserted instance) ;
    
    R visit(ImplicitNonMaximallyInserted instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(GeneralizingBinder instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Explicit instance) {
      return otherwise((instance));
    }
    
    default R visit(ImplicitMaximallyInserted instance) {
      return otherwise((instance));
    }
    
    default R visit(ImplicitNonMaximallyInserted instance) {
      return otherwise((instance));
    }
  }
  
  /**
   * Terms surrounded by `( ) introduce their free variables as explicit arguments
   */
  public static final class Explicit extends hydra.ext.fr.inria.coq.syntax.GeneralizingBinder implements Serializable {
    public final hydra.ext.fr.inria.coq.syntax.TypeclassConstraint value;
    
    public Explicit (hydra.ext.fr.inria.coq.syntax.TypeclassConstraint value) {
      java.util.Objects.requireNonNull((value));
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
  
  /**
   * Terms surrounded by `{ } introduce their free variables as maximally inserted implicit arguments
   */
  public static final class ImplicitMaximallyInserted extends hydra.ext.fr.inria.coq.syntax.GeneralizingBinder implements Serializable {
    public final hydra.ext.fr.inria.coq.syntax.TypeclassConstraint value;
    
    public ImplicitMaximallyInserted (hydra.ext.fr.inria.coq.syntax.TypeclassConstraint value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ImplicitMaximallyInserted)) {
        return false;
      }
      ImplicitMaximallyInserted o = (ImplicitMaximallyInserted) (other);
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
   * Terms surrounded by `[ ] introduce them as non-maximally inserted implicit arguments
   */
  public static final class ImplicitNonMaximallyInserted extends hydra.ext.fr.inria.coq.syntax.GeneralizingBinder implements Serializable {
    public final hydra.ext.fr.inria.coq.syntax.TypeclassConstraint value;
    
    public ImplicitNonMaximallyInserted (hydra.ext.fr.inria.coq.syntax.TypeclassConstraint value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ImplicitNonMaximallyInserted)) {
        return false;
      }
      ImplicitNonMaximallyInserted o = (ImplicitNonMaximallyInserted) (other);
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