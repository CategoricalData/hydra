// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class TrimType implements Serializable, Comparable<TrimType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.TrimType");

  public static final hydra.core.Name BTRIM = new hydra.core.Name("btrim");

  public static final hydra.core.Name LTRIM = new hydra.core.Name("ltrim");

  public static final hydra.core.Name RTRIM = new hydra.core.Name("rtrim");

  private TrimType () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Btrim instance) ;

    R visit(Ltrim instance) ;

    R visit(Rtrim instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TrimType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Btrim instance) {
      return otherwise(instance);
    }

    default R visit(Ltrim instance) {
      return otherwise(instance);
    }

    default R visit(Rtrim instance) {
      return otherwise(instance);
    }
  }

  public static final class Btrim extends openGql.grammar.TrimType implements Serializable {
    public Btrim () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Btrim)) {
        return false;
      }
      Btrim o = (Btrim) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TrimType other) {
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

  public static final class Ltrim extends openGql.grammar.TrimType implements Serializable {
    public Ltrim () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Ltrim)) {
        return false;
      }
      Ltrim o = (Ltrim) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TrimType other) {
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

  public static final class Rtrim extends openGql.grammar.TrimType implements Serializable {
    public Rtrim () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Rtrim)) {
        return false;
      }
      Rtrim o = (Rtrim) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TrimType other) {
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
