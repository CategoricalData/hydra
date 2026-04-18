// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class ExactNumericLiteral implements Serializable, Comparable<ExactNumericLiteral> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.ExactNumericLiteral");

  public static final hydra.core.Name SCIENTIFIC_WITH_SUFFIX = new hydra.core.Name("scientificWithSuffix");

  public static final hydra.core.Name COMMON_WITH_SUFFIX = new hydra.core.Name("commonWithSuffix");

  public static final hydra.core.Name COMMON_WITHOUT_SUFFIX = new hydra.core.Name("commonWithoutSuffix");

  public static final hydra.core.Name INTEGER_WITH_SUFFIX = new hydra.core.Name("integerWithSuffix");

  public static final hydra.core.Name UNSIGNED_INTEGER = new hydra.core.Name("unsignedInteger");

  private ExactNumericLiteral () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(ScientificWithSuffix instance) ;

    R visit(CommonWithSuffix instance) ;

    R visit(CommonWithoutSuffix instance) ;

    R visit(IntegerWithSuffix instance) ;

    R visit(UnsignedInteger instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ExactNumericLiteral instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(ScientificWithSuffix instance) {
      return otherwise(instance);
    }

    default R visit(CommonWithSuffix instance) {
      return otherwise(instance);
    }

    default R visit(CommonWithoutSuffix instance) {
      return otherwise(instance);
    }

    default R visit(IntegerWithSuffix instance) {
      return otherwise(instance);
    }

    default R visit(UnsignedInteger instance) {
      return otherwise(instance);
    }
  }

  public static final class ScientificWithSuffix extends openGql.grammar.ExactNumericLiteral implements Serializable {
    public final String value;

    public ScientificWithSuffix (String value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ScientificWithSuffix)) {
        return false;
      }
      ScientificWithSuffix o = (ScientificWithSuffix) other;
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
    public int compareTo(ExactNumericLiteral other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ScientificWithSuffix o = (ScientificWithSuffix) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class CommonWithSuffix extends openGql.grammar.ExactNumericLiteral implements Serializable {
    public final String value;

    public CommonWithSuffix (String value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof CommonWithSuffix)) {
        return false;
      }
      CommonWithSuffix o = (CommonWithSuffix) other;
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
    public int compareTo(ExactNumericLiteral other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      CommonWithSuffix o = (CommonWithSuffix) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class CommonWithoutSuffix extends openGql.grammar.ExactNumericLiteral implements Serializable {
    public final String value;

    public CommonWithoutSuffix (String value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof CommonWithoutSuffix)) {
        return false;
      }
      CommonWithoutSuffix o = (CommonWithoutSuffix) other;
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
    public int compareTo(ExactNumericLiteral other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      CommonWithoutSuffix o = (CommonWithoutSuffix) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class IntegerWithSuffix extends openGql.grammar.ExactNumericLiteral implements Serializable {
    public final String value;

    public IntegerWithSuffix (String value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof IntegerWithSuffix)) {
        return false;
      }
      IntegerWithSuffix o = (IntegerWithSuffix) other;
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
    public int compareTo(ExactNumericLiteral other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      IntegerWithSuffix o = (IntegerWithSuffix) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class UnsignedInteger extends openGql.grammar.ExactNumericLiteral implements Serializable {
    public final openGql.grammar.UnsignedInteger value;

    public UnsignedInteger (openGql.grammar.UnsignedInteger value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof UnsignedInteger)) {
        return false;
      }
      UnsignedInteger o = (UnsignedInteger) other;
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
    public int compareTo(ExactNumericLiteral other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      UnsignedInteger o = (UnsignedInteger) other;
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
