// Note: this is an automatically generated file. Do not edit.

package hydra.error.core;

import java.io.Serializable;

/**
 * An error indicating that a term is invalid
 */
public abstract class InvalidTermError implements Serializable, Comparable<InvalidTermError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.core.InvalidTermError");

  public static final hydra.core.Name CONSTANT_CONDITION = new hydra.core.Name("constantCondition");

  public static final hydra.core.Name DUPLICATE_BINDING = new hydra.core.Name("duplicateBinding");

  public static final hydra.core.Name DUPLICATE_FIELD = new hydra.core.Name("duplicateField");

  public static final hydra.core.Name EMPTY_CASE_STATEMENT = new hydra.core.Name("emptyCaseStatement");

  public static final hydra.core.Name EMPTY_LET_BINDINGS = new hydra.core.Name("emptyLetBindings");

  public static final hydra.core.Name EMPTY_TERM_ANNOTATION = new hydra.core.Name("emptyTermAnnotation");

  public static final hydra.core.Name EMPTY_TYPE_NAME_IN_TERM = new hydra.core.Name("emptyTypeNameInTerm");

  public static final hydra.core.Name INVALID_LAMBDA_PARAMETER_NAME = new hydra.core.Name("invalidLambdaParameterName");

  public static final hydra.core.Name INVALID_LET_BINDING_NAME = new hydra.core.Name("invalidLetBindingName");

  public static final hydra.core.Name INVALID_TYPE_LAMBDA_PARAMETER_NAME = new hydra.core.Name("invalidTypeLambdaParameterName");

  public static final hydra.core.Name NESTED_TERM_ANNOTATION = new hydra.core.Name("nestedTermAnnotation");

  public static final hydra.core.Name REDUNDANT_WRAP_UNWRAP = new hydra.core.Name("redundantWrapUnwrap");

  public static final hydra.core.Name SELF_APPLICATION = new hydra.core.Name("selfApplication");

  public static final hydra.core.Name TERM_VARIABLE_SHADOWING = new hydra.core.Name("termVariableShadowing");

  public static final hydra.core.Name TYPE_VARIABLE_SHADOWING_IN_TYPE_LAMBDA = new hydra.core.Name("typeVariableShadowingInTypeLambda");

  public static final hydra.core.Name UNDEFINED_TERM_VARIABLE = new hydra.core.Name("undefinedTermVariable");

  public static final hydra.core.Name UNDEFINED_TYPE_VARIABLE_IN_BINDING_TYPE = new hydra.core.Name("undefinedTypeVariableInBindingType");

  public static final hydra.core.Name UNDEFINED_TYPE_VARIABLE_IN_LAMBDA_DOMAIN = new hydra.core.Name("undefinedTypeVariableInLambdaDomain");

  public static final hydra.core.Name UNDEFINED_TYPE_VARIABLE_IN_TYPE_APPLICATION = new hydra.core.Name("undefinedTypeVariableInTypeApplication");

  public static final hydra.core.Name UNKNOWN_PRIMITIVE_NAME = new hydra.core.Name("unknownPrimitiveName");

  public static final hydra.core.Name UNNECESSARY_IDENTITY_APPLICATION = new hydra.core.Name("unnecessaryIdentityApplication");

  public static final hydra.core.Name UNTYPED_TERM_VARIABLE = new hydra.core.Name("untypedTermVariable");

  private InvalidTermError () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(ConstantCondition instance) ;

    R visit(DuplicateBinding instance) ;

    R visit(DuplicateField instance) ;

    R visit(EmptyCaseStatement instance) ;

    R visit(EmptyLetBindings instance) ;

    R visit(EmptyTermAnnotation instance) ;

    R visit(EmptyTypeNameInTerm instance) ;

    R visit(InvalidLambdaParameterName instance) ;

    R visit(InvalidLetBindingName instance) ;

    R visit(InvalidTypeLambdaParameterName instance) ;

    R visit(NestedTermAnnotation instance) ;

    R visit(RedundantWrapUnwrap instance) ;

    R visit(SelfApplication instance) ;

    R visit(TermVariableShadowing instance) ;

    R visit(TypeVariableShadowingInTypeLambda instance) ;

    R visit(UndefinedTermVariable instance) ;

    R visit(UndefinedTypeVariableInBindingType instance) ;

    R visit(UndefinedTypeVariableInLambdaDomain instance) ;

    R visit(UndefinedTypeVariableInTypeApplication instance) ;

    R visit(UnknownPrimitiveName instance) ;

    R visit(UnnecessaryIdentityApplication instance) ;

    R visit(UntypedTermVariable instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(InvalidTermError instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(ConstantCondition instance) {
      return otherwise(instance);
    }

    default R visit(DuplicateBinding instance) {
      return otherwise(instance);
    }

    default R visit(DuplicateField instance) {
      return otherwise(instance);
    }

    default R visit(EmptyCaseStatement instance) {
      return otherwise(instance);
    }

    default R visit(EmptyLetBindings instance) {
      return otherwise(instance);
    }

    default R visit(EmptyTermAnnotation instance) {
      return otherwise(instance);
    }

    default R visit(EmptyTypeNameInTerm instance) {
      return otherwise(instance);
    }

    default R visit(InvalidLambdaParameterName instance) {
      return otherwise(instance);
    }

    default R visit(InvalidLetBindingName instance) {
      return otherwise(instance);
    }

    default R visit(InvalidTypeLambdaParameterName instance) {
      return otherwise(instance);
    }

    default R visit(NestedTermAnnotation instance) {
      return otherwise(instance);
    }

    default R visit(RedundantWrapUnwrap instance) {
      return otherwise(instance);
    }

    default R visit(SelfApplication instance) {
      return otherwise(instance);
    }

    default R visit(TermVariableShadowing instance) {
      return otherwise(instance);
    }

    default R visit(TypeVariableShadowingInTypeLambda instance) {
      return otherwise(instance);
    }

    default R visit(UndefinedTermVariable instance) {
      return otherwise(instance);
    }

    default R visit(UndefinedTypeVariableInBindingType instance) {
      return otherwise(instance);
    }

    default R visit(UndefinedTypeVariableInLambdaDomain instance) {
      return otherwise(instance);
    }

    default R visit(UndefinedTypeVariableInTypeApplication instance) {
      return otherwise(instance);
    }

    default R visit(UnknownPrimitiveName instance) {
      return otherwise(instance);
    }

    default R visit(UnnecessaryIdentityApplication instance) {
      return otherwise(instance);
    }

    default R visit(UntypedTermVariable instance) {
      return otherwise(instance);
    }
  }

  /**
   * An ifElse with a literal boolean condition (optional)
   */
  public static final class ConstantCondition extends hydra.error.core.InvalidTermError implements Serializable {
    public final hydra.error.core.ConstantConditionError value;

    public ConstantCondition (hydra.error.core.ConstantConditionError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ConstantCondition)) {
        return false;
      }
      ConstantCondition o = (ConstantCondition) other;
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
    public int compareTo(InvalidTermError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ConstantCondition o = (ConstantCondition) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A duplicate binding name in a let expression
   */
  public static final class DuplicateBinding extends hydra.error.core.InvalidTermError implements Serializable {
    public final hydra.error.core.DuplicateBindingError value;

    public DuplicateBinding (hydra.error.core.DuplicateBindingError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DuplicateBinding)) {
        return false;
      }
      DuplicateBinding o = (DuplicateBinding) other;
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
    public int compareTo(InvalidTermError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DuplicateBinding o = (DuplicateBinding) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A duplicate field name in a record or case statement
   */
  public static final class DuplicateField extends hydra.error.core.InvalidTermError implements Serializable {
    public final hydra.error.core.DuplicateFieldError value;

    public DuplicateField (hydra.error.core.DuplicateFieldError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DuplicateField)) {
        return false;
      }
      DuplicateField o = (DuplicateField) other;
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
    public int compareTo(InvalidTermError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DuplicateField o = (DuplicateField) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A case statement with no cases and no default (optional)
   */
  public static final class EmptyCaseStatement extends hydra.error.core.InvalidTermError implements Serializable {
    public final hydra.error.core.EmptyCaseStatementError value;

    public EmptyCaseStatement (hydra.error.core.EmptyCaseStatementError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof EmptyCaseStatement)) {
        return false;
      }
      EmptyCaseStatement o = (EmptyCaseStatement) other;
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
    public int compareTo(InvalidTermError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      EmptyCaseStatement o = (EmptyCaseStatement) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A let expression with no bindings (optional)
   */
  public static final class EmptyLetBindings extends hydra.error.core.InvalidTermError implements Serializable {
    public final hydra.error.core.EmptyLetBindingsError value;

    public EmptyLetBindings (hydra.error.core.EmptyLetBindingsError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof EmptyLetBindings)) {
        return false;
      }
      EmptyLetBindings o = (EmptyLetBindings) other;
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
    public int compareTo(InvalidTermError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      EmptyLetBindings o = (EmptyLetBindings) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A term annotation with an empty annotation map (optional)
   */
  public static final class EmptyTermAnnotation extends hydra.error.core.InvalidTermError implements Serializable {
    public final hydra.error.core.EmptyTermAnnotationError value;

    public EmptyTermAnnotation (hydra.error.core.EmptyTermAnnotationError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof EmptyTermAnnotation)) {
        return false;
      }
      EmptyTermAnnotation o = (EmptyTermAnnotation) other;
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
    public int compareTo(InvalidTermError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      EmptyTermAnnotation o = (EmptyTermAnnotation) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A term with an empty type name (optional)
   */
  public static final class EmptyTypeNameInTerm extends hydra.error.core.InvalidTermError implements Serializable {
    public final hydra.error.core.EmptyTypeNameInTermError value;

    public EmptyTypeNameInTerm (hydra.error.core.EmptyTypeNameInTermError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof EmptyTypeNameInTerm)) {
        return false;
      }
      EmptyTypeNameInTerm o = (EmptyTypeNameInTerm) other;
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
    public int compareTo(InvalidTermError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      EmptyTypeNameInTerm o = (EmptyTypeNameInTerm) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A lambda parameter name violating naming conventions (optional)
   */
  public static final class InvalidLambdaParameterName extends hydra.error.core.InvalidTermError implements Serializable {
    public final hydra.error.core.InvalidLambdaParameterNameError value;

    public InvalidLambdaParameterName (hydra.error.core.InvalidLambdaParameterNameError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof InvalidLambdaParameterName)) {
        return false;
      }
      InvalidLambdaParameterName o = (InvalidLambdaParameterName) other;
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
    public int compareTo(InvalidTermError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      InvalidLambdaParameterName o = (InvalidLambdaParameterName) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A let binding name violating naming conventions (optional)
   */
  public static final class InvalidLetBindingName extends hydra.error.core.InvalidTermError implements Serializable {
    public final hydra.error.core.InvalidLetBindingNameError value;

    public InvalidLetBindingName (hydra.error.core.InvalidLetBindingNameError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof InvalidLetBindingName)) {
        return false;
      }
      InvalidLetBindingName o = (InvalidLetBindingName) other;
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
    public int compareTo(InvalidTermError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      InvalidLetBindingName o = (InvalidLetBindingName) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A type lambda parameter name violating naming conventions (optional)
   */
  public static final class InvalidTypeLambdaParameterName extends hydra.error.core.InvalidTermError implements Serializable {
    public final hydra.error.core.InvalidTypeLambdaParameterNameError value;

    public InvalidTypeLambdaParameterName (hydra.error.core.InvalidTypeLambdaParameterNameError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof InvalidTypeLambdaParameterName)) {
        return false;
      }
      InvalidTypeLambdaParameterName o = (InvalidTypeLambdaParameterName) other;
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
    public int compareTo(InvalidTermError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      InvalidTypeLambdaParameterName o = (InvalidTypeLambdaParameterName) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * Nested term annotations that should be merged (optional)
   */
  public static final class NestedTermAnnotation extends hydra.error.core.InvalidTermError implements Serializable {
    public final hydra.error.core.NestedTermAnnotationError value;

    public NestedTermAnnotation (hydra.error.core.NestedTermAnnotationError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NestedTermAnnotation)) {
        return false;
      }
      NestedTermAnnotation o = (NestedTermAnnotation) other;
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
    public int compareTo(InvalidTermError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      NestedTermAnnotation o = (NestedTermAnnotation) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A no-op unwrap-of-wrap round-trip (optional)
   */
  public static final class RedundantWrapUnwrap extends hydra.error.core.InvalidTermError implements Serializable {
    public final hydra.error.core.RedundantWrapUnwrapError value;

    public RedundantWrapUnwrap (hydra.error.core.RedundantWrapUnwrapError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof RedundantWrapUnwrap)) {
        return false;
      }
      RedundantWrapUnwrap o = (RedundantWrapUnwrap) other;
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
    public int compareTo(InvalidTermError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      RedundantWrapUnwrap o = (RedundantWrapUnwrap) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A variable applied to itself (optional)
   */
  public static final class SelfApplication extends hydra.error.core.InvalidTermError implements Serializable {
    public final hydra.error.core.SelfApplicationError value;

    public SelfApplication (hydra.error.core.SelfApplicationError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SelfApplication)) {
        return false;
      }
      SelfApplication o = (SelfApplication) other;
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
    public int compareTo(InvalidTermError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      SelfApplication o = (SelfApplication) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A binding that shadows a variable already in scope (optional)
   */
  public static final class TermVariableShadowing extends hydra.error.core.InvalidTermError implements Serializable {
    public final hydra.error.core.TermVariableShadowingError value;

    public TermVariableShadowing (hydra.error.core.TermVariableShadowingError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TermVariableShadowing)) {
        return false;
      }
      TermVariableShadowing o = (TermVariableShadowing) other;
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
    public int compareTo(InvalidTermError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      TermVariableShadowing o = (TermVariableShadowing) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A type lambda parameter that shadows a type variable in scope (optional)
   */
  public static final class TypeVariableShadowingInTypeLambda extends hydra.error.core.InvalidTermError implements Serializable {
    public final hydra.error.core.TypeVariableShadowingInTypeLambdaError value;

    public TypeVariableShadowingInTypeLambda (hydra.error.core.TypeVariableShadowingInTypeLambdaError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TypeVariableShadowingInTypeLambda)) {
        return false;
      }
      TypeVariableShadowingInTypeLambda o = (TypeVariableShadowingInTypeLambda) other;
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
    public int compareTo(InvalidTermError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      TypeVariableShadowingInTypeLambda o = (TypeVariableShadowingInTypeLambda) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A variable reference to an unbound term name
   */
  public static final class UndefinedTermVariable extends hydra.error.core.InvalidTermError implements Serializable {
    public final hydra.error.core.UndefinedTermVariableError value;

    public UndefinedTermVariable (hydra.error.core.UndefinedTermVariableError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof UndefinedTermVariable)) {
        return false;
      }
      UndefinedTermVariable o = (UndefinedTermVariable) other;
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
    public int compareTo(InvalidTermError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      UndefinedTermVariable o = (UndefinedTermVariable) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * An unbound type variable in a let binding's type scheme
   */
  public static final class UndefinedTypeVariableInBindingType extends hydra.error.core.InvalidTermError implements Serializable {
    public final hydra.error.core.UndefinedTypeVariableInBindingTypeError value;

    public UndefinedTypeVariableInBindingType (hydra.error.core.UndefinedTypeVariableInBindingTypeError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof UndefinedTypeVariableInBindingType)) {
        return false;
      }
      UndefinedTypeVariableInBindingType o = (UndefinedTypeVariableInBindingType) other;
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
    public int compareTo(InvalidTermError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      UndefinedTypeVariableInBindingType o = (UndefinedTypeVariableInBindingType) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * An unbound type variable in a lambda domain annotation
   */
  public static final class UndefinedTypeVariableInLambdaDomain extends hydra.error.core.InvalidTermError implements Serializable {
    public final hydra.error.core.UndefinedTypeVariableInLambdaDomainError value;

    public UndefinedTypeVariableInLambdaDomain (hydra.error.core.UndefinedTypeVariableInLambdaDomainError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof UndefinedTypeVariableInLambdaDomain)) {
        return false;
      }
      UndefinedTypeVariableInLambdaDomain o = (UndefinedTypeVariableInLambdaDomain) other;
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
    public int compareTo(InvalidTermError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      UndefinedTypeVariableInLambdaDomain o = (UndefinedTypeVariableInLambdaDomain) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * An unbound type variable in a type application term
   */
  public static final class UndefinedTypeVariableInTypeApplication extends hydra.error.core.InvalidTermError implements Serializable {
    public final hydra.error.core.UndefinedTypeVariableInTypeApplicationError value;

    public UndefinedTypeVariableInTypeApplication (hydra.error.core.UndefinedTypeVariableInTypeApplicationError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof UndefinedTypeVariableInTypeApplication)) {
        return false;
      }
      UndefinedTypeVariableInTypeApplication o = (UndefinedTypeVariableInTypeApplication) other;
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
    public int compareTo(InvalidTermError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      UndefinedTypeVariableInTypeApplication o = (UndefinedTypeVariableInTypeApplication) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A reference to an unknown primitive function
   */
  public static final class UnknownPrimitiveName extends hydra.error.core.InvalidTermError implements Serializable {
    public final hydra.error.core.UnknownPrimitiveNameError value;

    public UnknownPrimitiveName (hydra.error.core.UnknownPrimitiveNameError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof UnknownPrimitiveName)) {
        return false;
      }
      UnknownPrimitiveName o = (UnknownPrimitiveName) other;
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
    public int compareTo(InvalidTermError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      UnknownPrimitiveName o = (UnknownPrimitiveName) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * An identity lambda applied to an argument (optional)
   */
  public static final class UnnecessaryIdentityApplication extends hydra.error.core.InvalidTermError implements Serializable {
    public final hydra.error.core.UnnecessaryIdentityApplicationError value;

    public UnnecessaryIdentityApplication (hydra.error.core.UnnecessaryIdentityApplicationError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof UnnecessaryIdentityApplication)) {
        return false;
      }
      UnnecessaryIdentityApplication o = (UnnecessaryIdentityApplication) other;
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
    public int compareTo(InvalidTermError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      UnnecessaryIdentityApplication o = (UnnecessaryIdentityApplication) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A term variable whose type is not known
   */
  public static final class UntypedTermVariable extends hydra.error.core.InvalidTermError implements Serializable {
    public final hydra.error.core.UntypedTermVariableError value;

    public UntypedTermVariable (hydra.error.core.UntypedTermVariableError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof UntypedTermVariable)) {
        return false;
      }
      UntypedTermVariable o = (UntypedTermVariable) other;
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
    public int compareTo(InvalidTermError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      UntypedTermVariable o = (UntypedTermVariable) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
