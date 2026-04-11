// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public abstract class InlineShapeAtom implements Serializable, Comparable<InlineShapeAtom> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.InlineShapeAtom");

  public static final hydra.core.Name SEQUENCE = new hydra.core.Name("sequence");

  public static final hydra.core.Name SEQUENCE2 = new hydra.core.Name("sequence2");

  public static final hydra.core.Name SEQUENCE3 = new hydra.core.Name("sequence3");

  public static final hydra.core.Name PERIOD = new hydra.core.Name("Period");

  private InlineShapeAtom () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Sequence instance) ;

    R visit(Sequence2 instance) ;

    R visit(Sequence3 instance) ;

    R visit(Period instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(InlineShapeAtom instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Sequence instance) {
      return otherwise(instance);
    }

    default R visit(Sequence2 instance) {
      return otherwise(instance);
    }

    default R visit(Sequence3 instance) {
      return otherwise(instance);
    }

    default R visit(Period instance) {
      return otherwise(instance);
    }
  }

  public static final class Sequence extends hydra.ext.io.shex.syntax.InlineShapeAtom implements Serializable {
    public final hydra.ext.io.shex.syntax.InlineShapeAtom_Sequence value;

    public Sequence (hydra.ext.io.shex.syntax.InlineShapeAtom_Sequence value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Sequence)) {
        return false;
      }
      Sequence o = (Sequence) other;
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
    public int compareTo(InlineShapeAtom other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Sequence o = (Sequence) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Sequence2 extends hydra.ext.io.shex.syntax.InlineShapeAtom implements Serializable {
    public final hydra.ext.io.shex.syntax.InlineShapeAtom_Sequence2 value;

    public Sequence2 (hydra.ext.io.shex.syntax.InlineShapeAtom_Sequence2 value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Sequence2)) {
        return false;
      }
      Sequence2 o = (Sequence2) other;
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
    public int compareTo(InlineShapeAtom other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Sequence2 o = (Sequence2) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Sequence3 extends hydra.ext.io.shex.syntax.InlineShapeAtom implements Serializable {
    public final hydra.ext.io.shex.syntax.ShapeExpression value;

    public Sequence3 (hydra.ext.io.shex.syntax.ShapeExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Sequence3)) {
        return false;
      }
      Sequence3 o = (Sequence3) other;
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
    public int compareTo(InlineShapeAtom other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Sequence3 o = (Sequence3) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Period extends hydra.ext.io.shex.syntax.InlineShapeAtom implements Serializable {
    public Period () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Period)) {
        return false;
      }
      Period o = (Period) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(InlineShapeAtom other) {
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
