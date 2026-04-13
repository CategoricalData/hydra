// Note: this is an automatically generated file. Do not edit.

package hydra.cypher.openCypher;

import java.io.Serializable;

public abstract class StarOrYieldItems implements Serializable, Comparable<StarOrYieldItems> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.cypher.openCypher.StarOrYieldItems");

  public static final hydra.core.Name STAR = new hydra.core.Name("star");

  public static final hydra.core.Name ITEMS = new hydra.core.Name("items");

  private StarOrYieldItems () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Star instance) ;

    R visit(Items instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(StarOrYieldItems instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Star instance) {
      return otherwise(instance);
    }

    default R visit(Items instance) {
      return otherwise(instance);
    }
  }

  public static final class Star extends hydra.cypher.openCypher.StarOrYieldItems implements Serializable {
    public Star () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Star)) {
        return false;
      }
      Star o = (Star) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(StarOrYieldItems other) {
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

  public static final class Items extends hydra.cypher.openCypher.StarOrYieldItems implements Serializable {
    public final hydra.cypher.openCypher.YieldItems value;

    public Items (hydra.cypher.openCypher.YieldItems value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Items)) {
        return false;
      }
      Items o = (Items) other;
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
    public int compareTo(StarOrYieldItems other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Items o = (Items) other;
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
