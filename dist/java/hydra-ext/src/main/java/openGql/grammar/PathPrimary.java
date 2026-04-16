// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class PathPrimary implements Serializable, Comparable<PathPrimary> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.PathPrimary");

  public static final hydra.core.Name ELEMENT_PATTERN = new hydra.core.Name("elementPattern");

  public static final hydra.core.Name PARENTHESIZED_EXPRESSION = new hydra.core.Name("parenthesizedExpression");

  public static final hydra.core.Name SIMPLIFIED_EXPRESSION = new hydra.core.Name("simplifiedExpression");

  private PathPrimary () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(ElementPattern instance) ;

    R visit(ParenthesizedExpression instance) ;

    R visit(SimplifiedExpression instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(PathPrimary instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(ElementPattern instance) {
      return otherwise(instance);
    }

    default R visit(ParenthesizedExpression instance) {
      return otherwise(instance);
    }

    default R visit(SimplifiedExpression instance) {
      return otherwise(instance);
    }
  }

  public static final class ElementPattern extends openGql.grammar.PathPrimary implements Serializable {
    public final openGql.grammar.ElementPattern value;

    public ElementPattern (openGql.grammar.ElementPattern value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ElementPattern)) {
        return false;
      }
      ElementPattern o = (ElementPattern) other;
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
    public int compareTo(PathPrimary other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ElementPattern o = (ElementPattern) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class ParenthesizedExpression extends openGql.grammar.PathPrimary implements Serializable {
    public final openGql.grammar.ParenthesizedPathPatternExpression value;

    public ParenthesizedExpression (openGql.grammar.ParenthesizedPathPatternExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ParenthesizedExpression)) {
        return false;
      }
      ParenthesizedExpression o = (ParenthesizedExpression) other;
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
    public int compareTo(PathPrimary other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ParenthesizedExpression o = (ParenthesizedExpression) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class SimplifiedExpression extends openGql.grammar.PathPrimary implements Serializable {
    public final openGql.grammar.SimplifiedPathPatternExpression value;

    public SimplifiedExpression (openGql.grammar.SimplifiedPathPatternExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SimplifiedExpression)) {
        return false;
      }
      SimplifiedExpression o = (SimplifiedExpression) other;
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
    public int compareTo(PathPrimary other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      SimplifiedExpression o = (SimplifiedExpression) other;
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
