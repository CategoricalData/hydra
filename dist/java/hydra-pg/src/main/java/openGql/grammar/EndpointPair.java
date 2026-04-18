// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class EndpointPair implements Serializable, Comparable<EndpointPair> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.EndpointPair");

  public static final hydra.core.Name DIRECTED_PAIR = new hydra.core.Name("directedPair");

  public static final hydra.core.Name UNDIRECTED_PAIR = new hydra.core.Name("undirectedPair");

  private EndpointPair () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(DirectedPair instance) ;

    R visit(UndirectedPair instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(EndpointPair instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(DirectedPair instance) {
      return otherwise(instance);
    }

    default R visit(UndirectedPair instance) {
      return otherwise(instance);
    }
  }

  public static final class DirectedPair extends openGql.grammar.EndpointPair implements Serializable {
    public final openGql.grammar.EndpointPairDirected value;

    public DirectedPair (openGql.grammar.EndpointPairDirected value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DirectedPair)) {
        return false;
      }
      DirectedPair o = (DirectedPair) other;
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
    public int compareTo(EndpointPair other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DirectedPair o = (DirectedPair) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class UndirectedPair extends openGql.grammar.EndpointPair implements Serializable {
    public final openGql.grammar.EndpointPairUndirected value;

    public UndirectedPair (openGql.grammar.EndpointPairUndirected value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof UndirectedPair)) {
        return false;
      }
      UndirectedPair o = (UndirectedPair) other;
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
    public int compareTo(EndpointPair other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      UndirectedPair o = (UndirectedPair) other;
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
