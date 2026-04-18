// Note: this is an automatically generated file. Do not edit.

package hydra.shex.syntax;

import java.io.Serializable;

public abstract class ShapeOrRef implements Serializable, Comparable<ShapeOrRef> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.shex.syntax.ShapeOrRef");

  public static final hydra.core.Name SHAPE_DEFINITION = new hydra.core.Name("ShapeDefinition");

  public static final hydra.core.Name ATP_NAME_LN = new hydra.core.Name("AtpNameLn");

  public static final hydra.core.Name ATP_NAME_NS = new hydra.core.Name("AtpNameNs");

  public static final hydra.core.Name SEQUENCE = new hydra.core.Name("sequence");

  private ShapeOrRef () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(ShapeDefinition instance) ;

    R visit(AtpNameLn instance) ;

    R visit(AtpNameNs instance) ;

    R visit(Sequence instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ShapeOrRef instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(ShapeDefinition instance) {
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

  public static final class ShapeDefinition extends hydra.shex.syntax.ShapeOrRef implements Serializable {
    public final hydra.shex.syntax.ShapeDefinition value;

    public ShapeDefinition (hydra.shex.syntax.ShapeDefinition value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ShapeDefinition)) {
        return false;
      }
      ShapeDefinition o = (ShapeDefinition) other;
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
    public int compareTo(ShapeOrRef other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ShapeDefinition o = (ShapeDefinition) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class AtpNameLn extends hydra.shex.syntax.ShapeOrRef implements Serializable {
    public final hydra.shex.syntax.AtpNameLn value;

    public AtpNameLn (hydra.shex.syntax.AtpNameLn value) {
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
    public int compareTo(ShapeOrRef other) {
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

  public static final class AtpNameNs extends hydra.shex.syntax.ShapeOrRef implements Serializable {
    public final hydra.shex.syntax.AtpNameNs value;

    public AtpNameNs (hydra.shex.syntax.AtpNameNs value) {
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
    public int compareTo(ShapeOrRef other) {
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

  public static final class Sequence extends hydra.shex.syntax.ShapeOrRef implements Serializable {
    public final hydra.shex.syntax.ShapeExprLabel value;

    public Sequence (hydra.shex.syntax.ShapeExprLabel value) {
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
    public int compareTo(ShapeOrRef other) {
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
