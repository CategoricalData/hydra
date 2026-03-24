// Note: this is an automatically generated file. Do not edit.

package hydra.paths;

import java.io.Serializable;

/**
 * A function which maps from a type to a particular immediate subtype
 */
public abstract class SubtypeStep implements Serializable, Comparable<SubtypeStep> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.paths.SubtypeStep");

  public static final hydra.core.Name ANNOTATED_BODY = new hydra.core.Name("annotatedBody");

  public static final hydra.core.Name APPLICATION_FUNCTION = new hydra.core.Name("applicationFunction");

  public static final hydra.core.Name APPLICATION_ARGUMENT = new hydra.core.Name("applicationArgument");

  public static final hydra.core.Name EITHER_LEFT = new hydra.core.Name("eitherLeft");

  public static final hydra.core.Name EITHER_RIGHT = new hydra.core.Name("eitherRight");

  public static final hydra.core.Name FORALL_BODY = new hydra.core.Name("forallBody");

  public static final hydra.core.Name FUNCTION_DOMAIN = new hydra.core.Name("functionDomain");

  public static final hydra.core.Name FUNCTION_CODOMAIN = new hydra.core.Name("functionCodomain");

  public static final hydra.core.Name LIST_ELEMENT = new hydra.core.Name("listElement");

  public static final hydra.core.Name MAP_KEYS = new hydra.core.Name("mapKeys");

  public static final hydra.core.Name MAP_VALUES = new hydra.core.Name("mapValues");

  public static final hydra.core.Name MAYBE_ELEMENT = new hydra.core.Name("maybeElement");

  public static final hydra.core.Name PAIR_FIRST = new hydra.core.Name("pairFirst");

  public static final hydra.core.Name PAIR_SECOND = new hydra.core.Name("pairSecond");

  public static final hydra.core.Name RECORD_FIELD = new hydra.core.Name("recordField");

  public static final hydra.core.Name SET_ELEMENT = new hydra.core.Name("setElement");

  public static final hydra.core.Name UNION_FIELD = new hydra.core.Name("unionField");

  public static final hydra.core.Name WRAPPED_TYPE = new hydra.core.Name("wrappedType");

  private SubtypeStep () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(AnnotatedBody instance) ;

    R visit(ApplicationFunction instance) ;

    R visit(ApplicationArgument instance) ;

    R visit(EitherLeft instance) ;

    R visit(EitherRight instance) ;

    R visit(ForallBody instance) ;

    R visit(FunctionDomain instance) ;

    R visit(FunctionCodomain instance) ;

    R visit(ListElement instance) ;

    R visit(MapKeys instance) ;

    R visit(MapValues instance) ;

    R visit(MaybeElement instance) ;

    R visit(PairFirst instance) ;

    R visit(PairSecond instance) ;

    R visit(RecordField instance) ;

    R visit(SetElement instance) ;

    R visit(UnionField instance) ;

    R visit(WrappedType instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SubtypeStep instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(AnnotatedBody instance) {
      return otherwise(instance);
    }

    default R visit(ApplicationFunction instance) {
      return otherwise(instance);
    }

    default R visit(ApplicationArgument instance) {
      return otherwise(instance);
    }

    default R visit(EitherLeft instance) {
      return otherwise(instance);
    }

    default R visit(EitherRight instance) {
      return otherwise(instance);
    }

    default R visit(ForallBody instance) {
      return otherwise(instance);
    }

    default R visit(FunctionDomain instance) {
      return otherwise(instance);
    }

    default R visit(FunctionCodomain instance) {
      return otherwise(instance);
    }

    default R visit(ListElement instance) {
      return otherwise(instance);
    }

    default R visit(MapKeys instance) {
      return otherwise(instance);
    }

    default R visit(MapValues instance) {
      return otherwise(instance);
    }

    default R visit(MaybeElement instance) {
      return otherwise(instance);
    }

    default R visit(PairFirst instance) {
      return otherwise(instance);
    }

    default R visit(PairSecond instance) {
      return otherwise(instance);
    }

    default R visit(RecordField instance) {
      return otherwise(instance);
    }

    default R visit(SetElement instance) {
      return otherwise(instance);
    }

    default R visit(UnionField instance) {
      return otherwise(instance);
    }

    default R visit(WrappedType instance) {
      return otherwise(instance);
    }
  }

  /**
   * Access the body of an annotated type
   */
  public static final class AnnotatedBody extends hydra.paths.SubtypeStep implements Serializable {
    public AnnotatedBody () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AnnotatedBody)) {
        return false;
      }
      AnnotatedBody o = (AnnotatedBody) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(SubtypeStep other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * Access the function of an application type
   */
  public static final class ApplicationFunction extends hydra.paths.SubtypeStep implements Serializable {
    public ApplicationFunction () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ApplicationFunction)) {
        return false;
      }
      ApplicationFunction o = (ApplicationFunction) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(SubtypeStep other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * Access the argument of an application type
   */
  public static final class ApplicationArgument extends hydra.paths.SubtypeStep implements Serializable {
    public ApplicationArgument () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ApplicationArgument)) {
        return false;
      }
      ApplicationArgument o = (ApplicationArgument) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(SubtypeStep other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * Access the left type of an either type
   */
  public static final class EitherLeft extends hydra.paths.SubtypeStep implements Serializable {
    public EitherLeft () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof EitherLeft)) {
        return false;
      }
      EitherLeft o = (EitherLeft) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(SubtypeStep other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * Access the right type of an either type
   */
  public static final class EitherRight extends hydra.paths.SubtypeStep implements Serializable {
    public EitherRight () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof EitherRight)) {
        return false;
      }
      EitherRight o = (EitherRight) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(SubtypeStep other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * Access the body of a universally quantified type
   */
  public static final class ForallBody extends hydra.paths.SubtypeStep implements Serializable {
    public ForallBody () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ForallBody)) {
        return false;
      }
      ForallBody o = (ForallBody) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(SubtypeStep other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * Access the domain type of a function type
   */
  public static final class FunctionDomain extends hydra.paths.SubtypeStep implements Serializable {
    public FunctionDomain () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof FunctionDomain)) {
        return false;
      }
      FunctionDomain o = (FunctionDomain) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(SubtypeStep other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * Access the codomain type of a function type
   */
  public static final class FunctionCodomain extends hydra.paths.SubtypeStep implements Serializable {
    public FunctionCodomain () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof FunctionCodomain)) {
        return false;
      }
      FunctionCodomain o = (FunctionCodomain) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(SubtypeStep other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * Access the element type of a list type
   */
  public static final class ListElement extends hydra.paths.SubtypeStep implements Serializable {
    public ListElement () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ListElement)) {
        return false;
      }
      ListElement o = (ListElement) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(SubtypeStep other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * Access the key type of a map type
   */
  public static final class MapKeys extends hydra.paths.SubtypeStep implements Serializable {
    public MapKeys () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MapKeys)) {
        return false;
      }
      MapKeys o = (MapKeys) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(SubtypeStep other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * Access the value type of a map type
   */
  public static final class MapValues extends hydra.paths.SubtypeStep implements Serializable {
    public MapValues () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MapValues)) {
        return false;
      }
      MapValues o = (MapValues) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(SubtypeStep other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * Access the element type of an optional type
   */
  public static final class MaybeElement extends hydra.paths.SubtypeStep implements Serializable {
    public MaybeElement () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MaybeElement)) {
        return false;
      }
      MaybeElement o = (MaybeElement) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(SubtypeStep other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * Access the first type of a pair type
   */
  public static final class PairFirst extends hydra.paths.SubtypeStep implements Serializable {
    public PairFirst () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PairFirst)) {
        return false;
      }
      PairFirst o = (PairFirst) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(SubtypeStep other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * Access the second type of a pair type
   */
  public static final class PairSecond extends hydra.paths.SubtypeStep implements Serializable {
    public PairSecond () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PairSecond)) {
        return false;
      }
      PairSecond o = (PairSecond) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(SubtypeStep other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * Access a field type of a record type by field name
   */
  public static final class RecordField extends hydra.paths.SubtypeStep implements Serializable {
    public final hydra.core.Name value;

    public RecordField (hydra.core.Name value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof RecordField)) {
        return false;
      }
      RecordField o = (RecordField) other;
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
    public int compareTo(SubtypeStep other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      RecordField o = (RecordField) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * Access the element type of a set type
   */
  public static final class SetElement extends hydra.paths.SubtypeStep implements Serializable {
    public SetElement () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SetElement)) {
        return false;
      }
      SetElement o = (SetElement) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(SubtypeStep other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * Access a field type of a union type by field name
   */
  public static final class UnionField extends hydra.paths.SubtypeStep implements Serializable {
    public final hydra.core.Name value;

    public UnionField (hydra.core.Name value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof UnionField)) {
        return false;
      }
      UnionField o = (UnionField) other;
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
    public int compareTo(SubtypeStep other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      UnionField o = (UnionField) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * Access the type inside a wrapped type
   */
  public static final class WrappedType extends hydra.paths.SubtypeStep implements Serializable {
    public WrappedType () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof WrappedType)) {
        return false;
      }
      WrappedType o = (WrappedType) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(SubtypeStep other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
