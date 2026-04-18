// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

/**
 * Local or Global qualifier for commands
 */
public abstract class Locality implements Serializable, Comparable<Locality> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.Locality");

  public static final hydra.core.Name LOCAL = new hydra.core.Name("local");

  public static final hydra.core.Name GLOBAL = new hydra.core.Name("global");

  private Locality () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Local instance) ;

    R visit(Global instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Locality instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Local instance) {
      return otherwise(instance);
    }

    default R visit(Global instance) {
      return otherwise(instance);
    }
  }

  public static final class Local extends hydra.coq.syntax.Locality implements Serializable {
    public Local () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Local)) {
        return false;
      }
      Local o = (Local) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Locality other) {
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

  public static final class Global extends hydra.coq.syntax.Locality implements Serializable {
    public Global () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Global)) {
        return false;
      }
      Global o = (Global) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Locality other) {
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
