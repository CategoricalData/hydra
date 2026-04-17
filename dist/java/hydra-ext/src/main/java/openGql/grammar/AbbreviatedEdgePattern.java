// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class AbbreviatedEdgePattern implements Serializable, Comparable<AbbreviatedEdgePattern> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.AbbreviatedEdgePattern");

  public static final hydra.core.Name LEFT_ARROW = new hydra.core.Name("leftArrow");

  public static final hydra.core.Name TILDE = new hydra.core.Name("tilde");

  public static final hydra.core.Name RIGHT_ARROW = new hydra.core.Name("rightArrow");

  public static final hydra.core.Name LEFT_ARROW_TILDE = new hydra.core.Name("leftArrowTilde");

  public static final hydra.core.Name TILDE_RIGHT_ARROW = new hydra.core.Name("tildeRightArrow");

  public static final hydra.core.Name LEFT_MINUS_RIGHT = new hydra.core.Name("leftMinusRight");

  public static final hydra.core.Name MINUS_SIGN = new hydra.core.Name("minusSign");

  private AbbreviatedEdgePattern () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(LeftArrow instance) ;

    R visit(Tilde instance) ;

    R visit(RightArrow instance) ;

    R visit(LeftArrowTilde instance) ;

    R visit(TildeRightArrow instance) ;

    R visit(LeftMinusRight instance) ;

    R visit(MinusSign instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(AbbreviatedEdgePattern instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(LeftArrow instance) {
      return otherwise(instance);
    }

    default R visit(Tilde instance) {
      return otherwise(instance);
    }

    default R visit(RightArrow instance) {
      return otherwise(instance);
    }

    default R visit(LeftArrowTilde instance) {
      return otherwise(instance);
    }

    default R visit(TildeRightArrow instance) {
      return otherwise(instance);
    }

    default R visit(LeftMinusRight instance) {
      return otherwise(instance);
    }

    default R visit(MinusSign instance) {
      return otherwise(instance);
    }
  }

  public static final class LeftArrow extends openGql.grammar.AbbreviatedEdgePattern implements Serializable {
    public LeftArrow () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LeftArrow)) {
        return false;
      }
      LeftArrow o = (LeftArrow) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(AbbreviatedEdgePattern other) {
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

  public static final class Tilde extends openGql.grammar.AbbreviatedEdgePattern implements Serializable {
    public Tilde () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Tilde)) {
        return false;
      }
      Tilde o = (Tilde) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(AbbreviatedEdgePattern other) {
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

  public static final class RightArrow extends openGql.grammar.AbbreviatedEdgePattern implements Serializable {
    public RightArrow () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof RightArrow)) {
        return false;
      }
      RightArrow o = (RightArrow) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(AbbreviatedEdgePattern other) {
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

  public static final class LeftArrowTilde extends openGql.grammar.AbbreviatedEdgePattern implements Serializable {
    public LeftArrowTilde () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LeftArrowTilde)) {
        return false;
      }
      LeftArrowTilde o = (LeftArrowTilde) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(AbbreviatedEdgePattern other) {
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

  public static final class TildeRightArrow extends openGql.grammar.AbbreviatedEdgePattern implements Serializable {
    public TildeRightArrow () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TildeRightArrow)) {
        return false;
      }
      TildeRightArrow o = (TildeRightArrow) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(AbbreviatedEdgePattern other) {
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

  public static final class LeftMinusRight extends openGql.grammar.AbbreviatedEdgePattern implements Serializable {
    public LeftMinusRight () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LeftMinusRight)) {
        return false;
      }
      LeftMinusRight o = (LeftMinusRight) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(AbbreviatedEdgePattern other) {
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

  public static final class MinusSign extends openGql.grammar.AbbreviatedEdgePattern implements Serializable {
    public MinusSign () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MinusSign)) {
        return false;
      }
      MinusSign o = (MinusSign) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(AbbreviatedEdgePattern other) {
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
