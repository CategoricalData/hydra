// Note: this is an automatically generated file. Do not edit.

package hydra.python.syntax;

import java.io.Serializable;

public abstract class IfTail implements Serializable, Comparable<IfTail> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.python.syntax.IfTail");

  public static final hydra.core.Name ELIF = new hydra.core.Name("elif");

  public static final hydra.core.Name ELSE = new hydra.core.Name("else");

  private IfTail () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Elif instance) ;

    R visit(Else instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(IfTail instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Elif instance) {
      return otherwise(instance);
    }

    default R visit(Else instance) {
      return otherwise(instance);
    }
  }

  public static final class Elif extends hydra.python.syntax.IfTail implements Serializable {
    public final hydra.python.syntax.IfStatement value;

    public Elif (hydra.python.syntax.IfStatement value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Elif)) {
        return false;
      }
      Elif o = (Elif) other;
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
    public int compareTo(IfTail other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Elif o = (Elif) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Else extends hydra.python.syntax.IfTail implements Serializable {
    public final hydra.python.syntax.Block value;

    public Else (hydra.python.syntax.Block value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Else)) {
        return false;
      }
      Else o = (Else) other;
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
    public int compareTo(IfTail other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Else o = (Else) other;
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
