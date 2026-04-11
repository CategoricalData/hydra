// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public abstract class InlineShapeOrRef implements Serializable, Comparable<InlineShapeOrRef> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.InlineShapeOrRef");

  public static final hydra.core.Name INLINE_SHAPE_DEFINITION = new hydra.core.Name("InlineShapeDefinition");

  public static final hydra.core.Name ATP_NAME_LN = new hydra.core.Name("AtpNameLn");

  public static final hydra.core.Name ATP_NAME_NS = new hydra.core.Name("AtpNameNs");

  public static final hydra.core.Name SEQUENCE = new hydra.core.Name("sequence");

  private InlineShapeOrRef () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(InlineShapeDefinition instance) ;

    R visit(AtpNameLn instance) ;

    R visit(AtpNameNs instance) ;

    R visit(Sequence instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(InlineShapeOrRef instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(InlineShapeDefinition instance) {
      return otherwise(instance);
    }

    default R visit(AtpNameLn instance) {
      return otherwise(instance);
    }

    default R visit(AtpNameNs instance) {
      return otherwise(instance);
    }

    default R visit(Sequence instance) {
      return otherwise(instance);
    }
  }

  public static final class InlineShapeDefinition extends hydra.ext.io.shex.syntax.InlineShapeOrRef implements Serializable {
    public final hydra.ext.io.shex.syntax.InlineShapeDefinition value;

    public InlineShapeDefinition (hydra.ext.io.shex.syntax.InlineShapeDefinition value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof InlineShapeDefinition)) {
        return false;
      }
      InlineShapeDefinition o = (InlineShapeDefinition) other;
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
    public int compareTo(InlineShapeOrRef other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      InlineShapeDefinition o = (InlineShapeDefinition) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class AtpNameLn extends hydra.ext.io.shex.syntax.InlineShapeOrRef implements Serializable {
    public final hydra.ext.io.shex.syntax.AtpNameLn value;

    public AtpNameLn (hydra.ext.io.shex.syntax.AtpNameLn value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AtpNameLn)) {
        return false;
      }
      AtpNameLn o = (AtpNameLn) other;
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
    public int compareTo(InlineShapeOrRef other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      AtpNameLn o = (AtpNameLn) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class AtpNameNs extends hydra.ext.io.shex.syntax.InlineShapeOrRef implements Serializable {
    public final hydra.ext.io.shex.syntax.AtpNameNs value;

    public AtpNameNs (hydra.ext.io.shex.syntax.AtpNameNs value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AtpNameNs)) {
        return false;
      }
      AtpNameNs o = (AtpNameNs) other;
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
    public int compareTo(InlineShapeOrRef other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      AtpNameNs o = (AtpNameNs) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Sequence extends hydra.ext.io.shex.syntax.InlineShapeOrRef implements Serializable {
    public final hydra.ext.io.shex.syntax.ShapeExprLabel value;

    public Sequence (hydra.ext.io.shex.syntax.ShapeExprLabel value) {
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
    public int compareTo(InlineShapeOrRef other) {
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
}
