// Note: this is an automatically generated file. Do not edit.

package hydra.haskell.syntax;

import java.io.Serializable;

/**
 * A data constructor
 */
public abstract class Constructor implements Serializable, Comparable<Constructor> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.haskell.syntax.Constructor");

  public static final hydra.core.Name ORDINARY = new hydra.core.Name("ordinary");

  public static final hydra.core.Name RECORD = new hydra.core.Name("record");

  private Constructor () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Ordinary instance) ;

    R visit(Record instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Constructor instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Ordinary instance) {
      return otherwise(instance);
    }

    default R visit(Record instance) {
      return otherwise(instance);
    }
  }

  /**
   * An ordinary (positional) constructor
   */
  public static final class Ordinary extends hydra.haskell.syntax.Constructor implements Serializable {
    public final hydra.haskell.syntax.OrdinaryConstructor value;

    public Ordinary (hydra.haskell.syntax.OrdinaryConstructor value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Ordinary)) {
        return false;
      }
      Ordinary o = (Ordinary) other;
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
    public int compareTo(Constructor other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Ordinary o = (Ordinary) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A record constructor
   */
  public static final class Record extends hydra.haskell.syntax.Constructor implements Serializable {
    public final hydra.haskell.syntax.RecordConstructor value;

    public Record (hydra.haskell.syntax.RecordConstructor value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Record)) {
        return false;
      }
      Record o = (Record) other;
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
    public int compareTo(Constructor other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Record o = (Record) other;
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
