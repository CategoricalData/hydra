// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public abstract class PatternElement implements Serializable, Comparable<PatternElement> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.PatternElement");

  public static final hydra.core.Name CHAINED = new hydra.core.Name("chained");

  public static final hydra.core.Name PARENTHESIZED = new hydra.core.Name("parenthesized");

  private PatternElement () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Chained instance) ;

    R visit(Parenthesized instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(PatternElement instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Chained instance) {
      return otherwise(instance);
    }

    default R visit(Parenthesized instance) {
      return otherwise(instance);
    }
  }

  public static final class Chained extends hydra.ext.cypher.openCypher.PatternElement implements Serializable {
    public final hydra.ext.cypher.openCypher.NodePatternChain value;

    public Chained (hydra.ext.cypher.openCypher.NodePatternChain value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Chained)) {
        return false;
      }
      Chained o = (Chained) other;
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
    public int compareTo(PatternElement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Chained o = (Chained) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Parenthesized extends hydra.ext.cypher.openCypher.PatternElement implements Serializable {
    public final hydra.ext.cypher.openCypher.PatternElement value;

    public Parenthesized (hydra.ext.cypher.openCypher.PatternElement value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Parenthesized)) {
        return false;
      }
      Parenthesized o = (Parenthesized) other;
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
    public int compareTo(PatternElement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Parenthesized o = (Parenthesized) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
