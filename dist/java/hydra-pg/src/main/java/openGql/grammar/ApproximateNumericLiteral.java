// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class ApproximateNumericLiteral implements Serializable, Comparable<ApproximateNumericLiteral> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.ApproximateNumericLiteral");

  public static final hydra.core.Name SCIENTIFIC_WITH_SUFFIX = new hydra.core.Name("scientificWithSuffix");

  public static final hydra.core.Name SCIENTIFIC_WITHOUT_SUFFIX = new hydra.core.Name("scientificWithoutSuffix");

  public static final hydra.core.Name COMMON_WITH_SUFFIX = new hydra.core.Name("commonWithSuffix");

  public static final hydra.core.Name INTEGER_WITH_SUFFIX = new hydra.core.Name("integerWithSuffix");

  private ApproximateNumericLiteral () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(ScientificWithSuffix instance) ;

    R visit(ScientificWithoutSuffix instance) ;

    R visit(CommonWithSuffix instance) ;

    R visit(IntegerWithSuffix instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ApproximateNumericLiteral instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(ScientificWithSuffix instance) {
      return otherwise(instance);
    }

    default R visit(ScientificWithoutSuffix instance) {
      return otherwise(instance);
    }

    default R visit(CommonWithSuffix instance) {
      return otherwise(instance);
    }

    default R visit(IntegerWithSuffix instance) {
      return otherwise(instance);
    }
  }

  public static final class ScientificWithSuffix extends openGql.grammar.ApproximateNumericLiteral implements Serializable {
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
    public int compareTo(ApproximateNumericLiteral other) {
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

  public static final class ScientificWithoutSuffix extends openGql.grammar.ApproximateNumericLiteral implements Serializable {
    public final String value;

    public ScientificWithoutSuffix (String value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ScientificWithoutSuffix)) {
        return false;
      }
      ScientificWithoutSuffix o = (ScientificWithoutSuffix) other;
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
    public int compareTo(ApproximateNumericLiteral other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ScientificWithoutSuffix o = (ScientificWithoutSuffix) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class CommonWithSuffix extends openGql.grammar.ApproximateNumericLiteral implements Serializable {
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
    public int compareTo(ApproximateNumericLiteral other) {
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

  public static final class IntegerWithSuffix extends openGql.grammar.ApproximateNumericLiteral implements Serializable {
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
    public int compareTo(ApproximateNumericLiteral other) {
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
}
