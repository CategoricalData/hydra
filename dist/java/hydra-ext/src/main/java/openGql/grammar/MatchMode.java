// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class MatchMode implements Serializable, Comparable<MatchMode> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.MatchMode");

  public static final hydra.core.Name REPEATABLE_ELEMENTS = new hydra.core.Name("repeatableElements");

  public static final hydra.core.Name DIFFERENT_EDGES = new hydra.core.Name("differentEdges");

  private MatchMode () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(RepeatableElements instance) ;

    R visit(DifferentEdges instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(MatchMode instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(RepeatableElements instance) {
      return otherwise(instance);
    }

    default R visit(DifferentEdges instance) {
      return otherwise(instance);
    }
  }

  public static final class RepeatableElements extends openGql.grammar.MatchMode implements Serializable {
    public final openGql.grammar.ElementBindingsOrElements value;

    public RepeatableElements (openGql.grammar.ElementBindingsOrElements value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof RepeatableElements)) {
        return false;
      }
      RepeatableElements o = (RepeatableElements) other;
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
    public int compareTo(MatchMode other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      RepeatableElements o = (RepeatableElements) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class DifferentEdges extends openGql.grammar.MatchMode implements Serializable {
    public final openGql.grammar.EdgeBindingsOrEdges value;

    public DifferentEdges (openGql.grammar.EdgeBindingsOrEdges value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DifferentEdges)) {
        return false;
      }
      DifferentEdges o = (DifferentEdges) other;
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
    public int compareTo(MatchMode other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DifferentEdges o = (DifferentEdges) other;
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
