// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class GraphPatternQuantifier implements Serializable, Comparable<GraphPatternQuantifier> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.GraphPatternQuantifier");

  public static final hydra.core.Name ASTERISK = new hydra.core.Name("asterisk");

  public static final hydra.core.Name PLUS_SIGN = new hydra.core.Name("plusSign");

  public static final hydra.core.Name FIXED = new hydra.core.Name("fixed");

  public static final hydra.core.Name GENERAL = new hydra.core.Name("general");

  private GraphPatternQuantifier () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Asterisk instance) ;

    R visit(PlusSign instance) ;

    R visit(Fixed instance) ;

    R visit(General instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(GraphPatternQuantifier instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Asterisk instance) {
      return otherwise(instance);
    }

    default R visit(PlusSign instance) {
      return otherwise(instance);
    }

    default R visit(Fixed instance) {
      return otherwise(instance);
    }

    default R visit(General instance) {
      return otherwise(instance);
    }
  }

  public static final class Asterisk extends openGql.grammar.GraphPatternQuantifier implements Serializable {
    public Asterisk () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Asterisk)) {
        return false;
      }
      Asterisk o = (Asterisk) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(GraphPatternQuantifier other) {
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

  public static final class PlusSign extends openGql.grammar.GraphPatternQuantifier implements Serializable {
    public PlusSign () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PlusSign)) {
        return false;
      }
      PlusSign o = (PlusSign) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(GraphPatternQuantifier other) {
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

  public static final class Fixed extends openGql.grammar.GraphPatternQuantifier implements Serializable {
    public final openGql.grammar.UnsignedInteger value;

    public Fixed (openGql.grammar.UnsignedInteger value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Fixed)) {
        return false;
      }
      Fixed o = (Fixed) other;
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
    public int compareTo(GraphPatternQuantifier other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Fixed o = (Fixed) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class General extends openGql.grammar.GraphPatternQuantifier implements Serializable {
    public final openGql.grammar.GeneralQuantifier value;

    public General (openGql.grammar.GeneralQuantifier value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof General)) {
        return false;
      }
      General o = (General) other;
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
    public int compareTo(GraphPatternQuantifier other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      General o = (General) other;
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
