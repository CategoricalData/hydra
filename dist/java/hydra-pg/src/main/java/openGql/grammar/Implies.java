// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class Implies implements Serializable, Comparable<Implies> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.Implies");

  public static final hydra.core.Name RIGHT_DOUBLE_ARROW = new hydra.core.Name("rightDoubleArrow");

  public static final hydra.core.Name IMPLIES = new hydra.core.Name("implies");

  private Implies () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(RightDoubleArrow instance) ;

    R visit(Implies_ instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Implies instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(RightDoubleArrow instance) {
      return otherwise(instance);
    }

    default R visit(Implies_ instance) {
      return otherwise(instance);
    }
  }

  public static final class RightDoubleArrow extends openGql.grammar.Implies implements Serializable {
    public RightDoubleArrow () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof RightDoubleArrow)) {
        return false;
      }
      RightDoubleArrow o = (RightDoubleArrow) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Implies other) {
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

  public static final class Implies_ extends openGql.grammar.Implies implements Serializable {
    public Implies_ () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Implies_)) {
        return false;
      }
      Implies_ o = (Implies_) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Implies other) {
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
}
