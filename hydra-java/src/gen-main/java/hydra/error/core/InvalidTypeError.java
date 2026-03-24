// Note: this is an automatically generated file. Do not edit.

package hydra.error.core;

import java.io.Serializable;

/**
 * An error indicating that a type is invalid
 */
public abstract class InvalidTypeError implements Serializable, Comparable<InvalidTypeError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.core.InvalidTypeError");

  public static final hydra.core.Name DUPLICATE_RECORD_TYPE_FIELD_NAMES = new hydra.core.Name("duplicateRecordTypeFieldNames");

  public static final hydra.core.Name DUPLICATE_UNION_TYPE_FIELD_NAMES = new hydra.core.Name("duplicateUnionTypeFieldNames");

  public static final hydra.core.Name EMPTY_RECORD_TYPE = new hydra.core.Name("emptyRecordType");

  public static final hydra.core.Name EMPTY_TYPE_ANNOTATION = new hydra.core.Name("emptyTypeAnnotation");

  public static final hydra.core.Name EMPTY_UNION_TYPE = new hydra.core.Name("emptyUnionType");

  public static final hydra.core.Name INVALID_FORALL_PARAMETER_NAME = new hydra.core.Name("invalidForallParameterName");

  public static final hydra.core.Name INVALID_TYPE_SCHEME_VARIABLE_NAME = new hydra.core.Name("invalidTypeSchemeVariableName");

  public static final hydra.core.Name NESTED_TYPE_ANNOTATION = new hydra.core.Name("nestedTypeAnnotation");

  public static final hydra.core.Name NON_COMPARABLE_MAP_KEY_TYPE = new hydra.core.Name("nonComparableMapKeyType");

  public static final hydra.core.Name NON_COMPARABLE_SET_ELEMENT_TYPE = new hydra.core.Name("nonComparableSetElementType");

  public static final hydra.core.Name SINGLE_VARIANT_UNION = new hydra.core.Name("singleVariantUnion");

  public static final hydra.core.Name TYPE_VARIABLE_SHADOWING_IN_FORALL = new hydra.core.Name("typeVariableShadowingInForall");

  public static final hydra.core.Name UNDEFINED_TYPE_VARIABLE = new hydra.core.Name("undefinedTypeVariable");

  public static final hydra.core.Name VOID_IN_NON_BOTTOM_POSITION = new hydra.core.Name("voidInNonBottomPosition");

  private InvalidTypeError () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(DuplicateRecordTypeFieldNames instance) ;

    R visit(DuplicateUnionTypeFieldNames instance) ;

    R visit(EmptyRecordType instance) ;

    R visit(EmptyTypeAnnotation instance) ;

    R visit(EmptyUnionType instance) ;

    R visit(InvalidForallParameterName instance) ;

    R visit(InvalidTypeSchemeVariableName instance) ;

    R visit(NestedTypeAnnotation instance) ;

    R visit(NonComparableMapKeyType instance) ;

    R visit(NonComparableSetElementType instance) ;

    R visit(SingleVariantUnion instance) ;

    R visit(TypeVariableShadowingInForall instance) ;

    R visit(UndefinedTypeVariable instance) ;

    R visit(VoidInNonBottomPosition instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(InvalidTypeError instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(DuplicateRecordTypeFieldNames instance) {
      return otherwise(instance);
    }

    default R visit(DuplicateUnionTypeFieldNames instance) {
      return otherwise(instance);
    }

    default R visit(EmptyRecordType instance) {
      return otherwise(instance);
    }

    default R visit(EmptyTypeAnnotation instance) {
      return otherwise(instance);
    }

    default R visit(EmptyUnionType instance) {
      return otherwise(instance);
    }

    default R visit(InvalidForallParameterName instance) {
      return otherwise(instance);
    }

    default R visit(InvalidTypeSchemeVariableName instance) {
      return otherwise(instance);
    }

    default R visit(NestedTypeAnnotation instance) {
      return otherwise(instance);
    }

    default R visit(NonComparableMapKeyType instance) {
      return otherwise(instance);
    }

    default R visit(NonComparableSetElementType instance) {
      return otherwise(instance);
    }

    default R visit(SingleVariantUnion instance) {
      return otherwise(instance);
    }

    default R visit(TypeVariableShadowingInForall instance) {
      return otherwise(instance);
    }

    default R visit(UndefinedTypeVariable instance) {
      return otherwise(instance);
    }

    default R visit(VoidInNonBottomPosition instance) {
      return otherwise(instance);
    }
  }

  /**
   * A record type with duplicate field names
   */
  public static final class DuplicateRecordTypeFieldNames extends hydra.error.core.InvalidTypeError implements Serializable {
    public final hydra.error.core.DuplicateRecordTypeFieldNamesError value;

    public DuplicateRecordTypeFieldNames (hydra.error.core.DuplicateRecordTypeFieldNamesError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DuplicateRecordTypeFieldNames)) {
        return false;
      }
      DuplicateRecordTypeFieldNames o = (DuplicateRecordTypeFieldNames) other;
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
    public int compareTo(InvalidTypeError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DuplicateRecordTypeFieldNames o = (DuplicateRecordTypeFieldNames) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A union type with duplicate field names
   */
  public static final class DuplicateUnionTypeFieldNames extends hydra.error.core.InvalidTypeError implements Serializable {
    public final hydra.error.core.DuplicateUnionTypeFieldNamesError value;

    public DuplicateUnionTypeFieldNames (hydra.error.core.DuplicateUnionTypeFieldNamesError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DuplicateUnionTypeFieldNames)) {
        return false;
      }
      DuplicateUnionTypeFieldNames o = (DuplicateUnionTypeFieldNames) other;
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
    public int compareTo(InvalidTypeError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DuplicateUnionTypeFieldNames o = (DuplicateUnionTypeFieldNames) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A record type with no fields (optional)
   */
  public static final class EmptyRecordType extends hydra.error.core.InvalidTypeError implements Serializable {
    public final hydra.error.core.EmptyRecordTypeError value;

    public EmptyRecordType (hydra.error.core.EmptyRecordTypeError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof EmptyRecordType)) {
        return false;
      }
      EmptyRecordType o = (EmptyRecordType) other;
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
    public int compareTo(InvalidTypeError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      EmptyRecordType o = (EmptyRecordType) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A type annotation with an empty annotation map (optional)
   */
  public static final class EmptyTypeAnnotation extends hydra.error.core.InvalidTypeError implements Serializable {
    public final hydra.error.core.EmptyTypeAnnotationError value;

    public EmptyTypeAnnotation (hydra.error.core.EmptyTypeAnnotationError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof EmptyTypeAnnotation)) {
        return false;
      }
      EmptyTypeAnnotation o = (EmptyTypeAnnotation) other;
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
    public int compareTo(InvalidTypeError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      EmptyTypeAnnotation o = (EmptyTypeAnnotation) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A union type with no alternatives (optional)
   */
  public static final class EmptyUnionType extends hydra.error.core.InvalidTypeError implements Serializable {
    public final hydra.error.core.EmptyUnionTypeError value;

    public EmptyUnionType (hydra.error.core.EmptyUnionTypeError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof EmptyUnionType)) {
        return false;
      }
      EmptyUnionType o = (EmptyUnionType) other;
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
    public int compareTo(InvalidTypeError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      EmptyUnionType o = (EmptyUnionType) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A forall parameter name violating naming conventions (optional)
   */
  public static final class InvalidForallParameterName extends hydra.error.core.InvalidTypeError implements Serializable {
    public final hydra.error.core.InvalidForallParameterNameError value;

    public InvalidForallParameterName (hydra.error.core.InvalidForallParameterNameError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof InvalidForallParameterName)) {
        return false;
      }
      InvalidForallParameterName o = (InvalidForallParameterName) other;
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
    public int compareTo(InvalidTypeError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      InvalidForallParameterName o = (InvalidForallParameterName) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A type scheme variable name violating naming conventions (optional)
   */
  public static final class InvalidTypeSchemeVariableName extends hydra.error.core.InvalidTypeError implements Serializable {
    public final hydra.error.core.InvalidTypeSchemeVariableNameError value;

    public InvalidTypeSchemeVariableName (hydra.error.core.InvalidTypeSchemeVariableNameError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof InvalidTypeSchemeVariableName)) {
        return false;
      }
      InvalidTypeSchemeVariableName o = (InvalidTypeSchemeVariableName) other;
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
    public int compareTo(InvalidTypeError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      InvalidTypeSchemeVariableName o = (InvalidTypeSchemeVariableName) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * Nested type annotations that should be merged (optional)
   */
  public static final class NestedTypeAnnotation extends hydra.error.core.InvalidTypeError implements Serializable {
    public final hydra.error.core.NestedTypeAnnotationError value;

    public NestedTypeAnnotation (hydra.error.core.NestedTypeAnnotationError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NestedTypeAnnotation)) {
        return false;
      }
      NestedTypeAnnotation o = (NestedTypeAnnotation) other;
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
    public int compareTo(InvalidTypeError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      NestedTypeAnnotation o = (NestedTypeAnnotation) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A map with a non-comparable key type
   */
  public static final class NonComparableMapKeyType extends hydra.error.core.InvalidTypeError implements Serializable {
    public final hydra.error.core.NonComparableMapKeyTypeError value;

    public NonComparableMapKeyType (hydra.error.core.NonComparableMapKeyTypeError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NonComparableMapKeyType)) {
        return false;
      }
      NonComparableMapKeyType o = (NonComparableMapKeyType) other;
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
    public int compareTo(InvalidTypeError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      NonComparableMapKeyType o = (NonComparableMapKeyType) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A set with a non-comparable element type
   */
  public static final class NonComparableSetElementType extends hydra.error.core.InvalidTypeError implements Serializable {
    public final hydra.error.core.NonComparableSetElementTypeError value;

    public NonComparableSetElementType (hydra.error.core.NonComparableSetElementTypeError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NonComparableSetElementType)) {
        return false;
      }
      NonComparableSetElementType o = (NonComparableSetElementType) other;
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
    public int compareTo(InvalidTypeError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      NonComparableSetElementType o = (NonComparableSetElementType) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A union type with only one variant (optional)
   */
  public static final class SingleVariantUnion extends hydra.error.core.InvalidTypeError implements Serializable {
    public final hydra.error.core.SingleVariantUnionError value;

    public SingleVariantUnion (hydra.error.core.SingleVariantUnionError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SingleVariantUnion)) {
        return false;
      }
      SingleVariantUnion o = (SingleVariantUnion) other;
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
    public int compareTo(InvalidTypeError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      SingleVariantUnion o = (SingleVariantUnion) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A forall parameter that shadows a type variable in scope (optional)
   */
  public static final class TypeVariableShadowingInForall extends hydra.error.core.InvalidTypeError implements Serializable {
    public final hydra.error.core.TypeVariableShadowingInForallError value;

    public TypeVariableShadowingInForall (hydra.error.core.TypeVariableShadowingInForallError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TypeVariableShadowingInForall)) {
        return false;
      }
      TypeVariableShadowingInForall o = (TypeVariableShadowingInForall) other;
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
    public int compareTo(InvalidTypeError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      TypeVariableShadowingInForall o = (TypeVariableShadowingInForall) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A type variable reference to an unbound name
   */
  public static final class UndefinedTypeVariable extends hydra.error.core.InvalidTypeError implements Serializable {
    public final hydra.error.core.UndefinedTypeVariableError value;

    public UndefinedTypeVariable (hydra.error.core.UndefinedTypeVariableError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof UndefinedTypeVariable)) {
        return false;
      }
      UndefinedTypeVariable o = (UndefinedTypeVariable) other;
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
    public int compareTo(InvalidTypeError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      UndefinedTypeVariable o = (UndefinedTypeVariable) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * TypeVoid in a position where no value can be constructed (optional)
   */
  public static final class VoidInNonBottomPosition extends hydra.error.core.InvalidTypeError implements Serializable {
    public final hydra.error.core.VoidInNonBottomPositionError value;

    public VoidInNonBottomPosition (hydra.error.core.VoidInNonBottomPositionError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof VoidInNonBottomPosition)) {
        return false;
      }
      VoidInNonBottomPosition o = (VoidInNonBottomPosition) other;
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
    public int compareTo(InvalidTypeError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      VoidInNonBottomPosition o = (VoidInNonBottomPosition) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
