// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class GraphTypeSource implements Serializable, Comparable<GraphTypeSource> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.GraphTypeSource");

  public static final hydra.core.Name COPY_OF = new hydra.core.Name("copyOf");

  public static final hydra.core.Name LIKE_GRAPH = new hydra.core.Name("likeGraph");

  public static final hydra.core.Name NESTED_SPECIFICATION = new hydra.core.Name("nestedSpecification");

  private GraphTypeSource () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(CopyOf instance) ;

    R visit(LikeGraph instance) ;

    R visit(NestedSpecification instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(GraphTypeSource instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(CopyOf instance) {
      return otherwise(instance);
    }

    default R visit(LikeGraph instance) {
      return otherwise(instance);
    }

    default R visit(NestedSpecification instance) {
      return otherwise(instance);
    }
  }

  public static final class CopyOf extends openGql.grammar.GraphTypeSource implements Serializable {
    public final openGql.grammar.GraphTypeReference value;

    public CopyOf (openGql.grammar.GraphTypeReference value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof CopyOf)) {
        return false;
      }
      CopyOf o = (CopyOf) other;
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
    public int compareTo(GraphTypeSource other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      CopyOf o = (CopyOf) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class LikeGraph extends openGql.grammar.GraphTypeSource implements Serializable {
    public final openGql.grammar.GraphExpression value;

    public LikeGraph (openGql.grammar.GraphExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LikeGraph)) {
        return false;
      }
      LikeGraph o = (LikeGraph) other;
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
    public int compareTo(GraphTypeSource other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      LikeGraph o = (LikeGraph) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class NestedSpecification extends openGql.grammar.GraphTypeSource implements Serializable {
    public final java.util.List<openGql.grammar.ElementTypeSpecification> value;

    public NestedSpecification (java.util.List<openGql.grammar.ElementTypeSpecification> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NestedSpecification)) {
        return false;
      }
      NestedSpecification o = (NestedSpecification) other;
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
    public int compareTo(GraphTypeSource other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      NestedSpecification o = (NestedSpecification) other;
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
