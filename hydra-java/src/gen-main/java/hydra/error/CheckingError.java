// Note: this is an automatically generated file. Do not edit.

package hydra.error;

import java.io.Serializable;

/**
 * An error that occurred during type checking
 */
public abstract class CheckingError implements Serializable, Comparable<CheckingError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.CheckingError");
  
  public static final hydra.core.Name INCORRECT_UNIFICATION = new hydra.core.Name("incorrectUnification");
  
  public static final hydra.core.Name NOT_A_FORALL_TYPE = new hydra.core.Name("notAForallType");
  
  public static final hydra.core.Name NOT_A_FUNCTION_TYPE = new hydra.core.Name("notAFunctionType");
  
  public static final hydra.core.Name TYPE_ARITY_MISMATCH = new hydra.core.Name("typeArityMismatch");
  
  public static final hydra.core.Name TYPE_MISMATCH = new hydra.core.Name("typeMismatch");
  
  public static final hydra.core.Name UNBOUND_TYPE_VARIABLES = new hydra.core.Name("unboundTypeVariables");
  
  public static final hydra.core.Name UNEQUAL_TYPES = new hydra.core.Name("unequalTypes");
  
  public static final hydra.core.Name UNSUPPORTED_TERM_VARIANT = new hydra.core.Name("unsupportedTermVariant");
  
  public static final hydra.core.Name UNTYPED_LAMBDA = new hydra.core.Name("untypedLambda");
  
  public static final hydra.core.Name UNTYPED_LET_BINDING = new hydra.core.Name("untypedLetBinding");
  
  private CheckingError () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(IncorrectUnification instance) ;
    
    R visit(NotAForallType instance) ;
    
    R visit(NotAFunctionType instance) ;
    
    R visit(TypeArityMismatch instance) ;
    
    R visit(TypeMismatch instance) ;
    
    R visit(UnboundTypeVariables instance) ;
    
    R visit(UnequalTypes instance) ;
    
    R visit(UnsupportedTermVariant instance) ;
    
    R visit(UntypedLambda instance) ;
    
    R visit(UntypedLetBinding instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(CheckingError instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(IncorrectUnification instance) {
      return otherwise(instance);
    }
    
    default R visit(NotAForallType instance) {
      return otherwise(instance);
    }
    
    default R visit(NotAFunctionType instance) {
      return otherwise(instance);
    }
    
    default R visit(TypeArityMismatch instance) {
      return otherwise(instance);
    }
    
    default R visit(TypeMismatch instance) {
      return otherwise(instance);
    }
    
    default R visit(UnboundTypeVariables instance) {
      return otherwise(instance);
    }
    
    default R visit(UnequalTypes instance) {
      return otherwise(instance);
    }
    
    default R visit(UnsupportedTermVariant instance) {
      return otherwise(instance);
    }
    
    default R visit(UntypedLambda instance) {
      return otherwise(instance);
    }
    
    default R visit(UntypedLetBinding instance) {
      return otherwise(instance);
    }
  }
  
  /**
   * A post-unification consistency check failure
   */
  public static final class IncorrectUnification extends hydra.error.CheckingError implements Serializable {
    public final hydra.error.IncorrectUnificationError value;
    
    public IncorrectUnification (hydra.error.IncorrectUnificationError value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof IncorrectUnification)) {
        return false;
      }
      IncorrectUnification o = (IncorrectUnification) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(CheckingError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      IncorrectUnification o = (IncorrectUnification) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A type that is not a forall type when one was expected
   */
  public static final class NotAForallType extends hydra.error.CheckingError implements Serializable {
    public final hydra.error.NotAForallTypeError value;
    
    public NotAForallType (hydra.error.NotAForallTypeError value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NotAForallType)) {
        return false;
      }
      NotAForallType o = (NotAForallType) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(CheckingError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      NotAForallType o = (NotAForallType) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A type that is not a function type when one was expected
   */
  public static final class NotAFunctionType extends hydra.error.CheckingError implements Serializable {
    public final hydra.error.NotAFunctionTypeError value;
    
    public NotAFunctionType (hydra.error.NotAFunctionTypeError value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NotAFunctionType)) {
        return false;
      }
      NotAFunctionType o = (NotAFunctionType) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(CheckingError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      NotAFunctionType o = (NotAFunctionType) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A type constructor applied to the wrong number of arguments
   */
  public static final class TypeArityMismatch extends hydra.error.CheckingError implements Serializable {
    public final hydra.error.TypeArityMismatchError value;
    
    public TypeArityMismatch (hydra.error.TypeArityMismatchError value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TypeArityMismatch)) {
        return false;
      }
      TypeArityMismatch o = (TypeArityMismatch) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(CheckingError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      TypeArityMismatch o = (TypeArityMismatch) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A type mismatch between expected and actual types
   */
  public static final class TypeMismatch extends hydra.error.CheckingError implements Serializable {
    public final hydra.error.TypeMismatchError value;
    
    public TypeMismatch (hydra.error.TypeMismatchError value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TypeMismatch)) {
        return false;
      }
      TypeMismatch o = (TypeMismatch) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(CheckingError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      TypeMismatch o = (TypeMismatch) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * Type variables that are not bound in scope
   */
  public static final class UnboundTypeVariables extends hydra.error.CheckingError implements Serializable {
    public final hydra.error.UnboundTypeVariablesError value;
    
    public UnboundTypeVariables (hydra.error.UnboundTypeVariablesError value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof UnboundTypeVariables)) {
        return false;
      }
      UnboundTypeVariables o = (UnboundTypeVariables) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(CheckingError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      UnboundTypeVariables o = (UnboundTypeVariables) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * Multiple types that should be equal but are not
   */
  public static final class UnequalTypes extends hydra.error.CheckingError implements Serializable {
    public final hydra.error.UnequalTypesError value;
    
    public UnequalTypes (hydra.error.UnequalTypesError value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof UnequalTypes)) {
        return false;
      }
      UnequalTypes o = (UnequalTypes) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(CheckingError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      UnequalTypes o = (UnequalTypes) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A term variant that the type checker does not support
   */
  public static final class UnsupportedTermVariant extends hydra.error.CheckingError implements Serializable {
    public final hydra.error.UnsupportedTermVariantError value;
    
    public UnsupportedTermVariant (hydra.error.UnsupportedTermVariantError value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof UnsupportedTermVariant)) {
        return false;
      }
      UnsupportedTermVariant o = (UnsupportedTermVariant) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(CheckingError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      UnsupportedTermVariant o = (UnsupportedTermVariant) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A lambda expression without a type annotation on its parameter
   */
  public static final class UntypedLambda extends hydra.error.CheckingError implements Serializable {
    public final hydra.error.UntypedLambdaError value;
    
    public UntypedLambda (hydra.error.UntypedLambdaError value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof UntypedLambda)) {
        return false;
      }
      UntypedLambda o = (UntypedLambda) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(CheckingError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      UntypedLambda o = (UntypedLambda) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A let binding without a type annotation
   */
  public static final class UntypedLetBinding extends hydra.error.CheckingError implements Serializable {
    public final hydra.error.UntypedLetBindingError value;
    
    public UntypedLetBinding (hydra.error.UntypedLetBindingError value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof UntypedLetBinding)) {
        return false;
      }
      UntypedLetBinding o = (UntypedLetBinding) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(CheckingError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      UntypedLetBinding o = (UntypedLetBinding) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
