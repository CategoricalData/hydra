// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class UnsignedValueSpecification implements Serializable, Comparable<UnsignedValueSpecification> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.UnsignedValueSpecification");

  public static final hydra.core.Name UNSIGNED_LITERAL = new hydra.core.Name("unsignedLiteral");

  public static final hydra.core.Name GENERAL_VALUE_SPECIFICATION = new hydra.core.Name("generalValueSpecification");

  private UnsignedValueSpecification () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(UnsignedLiteral instance) ;

    R visit(GeneralValueSpecification instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(UnsignedValueSpecification instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(UnsignedLiteral instance) {
      return otherwise(instance);
    }

    default R visit(GeneralValueSpecification instance) {
      return otherwise(instance);
    }
  }

  public static final class UnsignedLiteral extends openGql.grammar.UnsignedValueSpecification implements Serializable {
    public final openGql.grammar.UnsignedLiteral value;

    public UnsignedLiteral (openGql.grammar.UnsignedLiteral value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof UnsignedLiteral)) {
        return false;
      }
      UnsignedLiteral o = (UnsignedLiteral) other;
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
    public int compareTo(UnsignedValueSpecification other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      UnsignedLiteral o = (UnsignedLiteral) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class GeneralValueSpecification extends openGql.grammar.UnsignedValueSpecification implements Serializable {
    public final openGql.grammar.GeneralValueSpecification value;

    public GeneralValueSpecification (openGql.grammar.GeneralValueSpecification value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof GeneralValueSpecification)) {
        return false;
      }
      GeneralValueSpecification o = (GeneralValueSpecification) other;
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
    public int compareTo(UnsignedValueSpecification other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      GeneralValueSpecification o = (GeneralValueSpecification) other;
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
