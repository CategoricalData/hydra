// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class DetachOption implements Serializable, Comparable<DetachOption> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.DetachOption");

  public static final hydra.core.Name DETACH = new hydra.core.Name("detach");

  public static final hydra.core.Name NO_DETACH = new hydra.core.Name("noDetach");

  private DetachOption () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Detach instance) ;

    R visit(NoDetach instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(DetachOption instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Detach instance) {
      return otherwise(instance);
    }

    default R visit(NoDetach instance) {
      return otherwise(instance);
    }
  }

  public static final class Detach extends openGql.grammar.DetachOption implements Serializable {
    public Detach () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Detach)) {
        return false;
      }
      Detach o = (Detach) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(DetachOption other) {
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

  public static final class NoDetach extends openGql.grammar.DetachOption implements Serializable {
    public NoDetach () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NoDetach)) {
        return false;
      }
      NoDetach o = (NoDetach) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(DetachOption other) {
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
