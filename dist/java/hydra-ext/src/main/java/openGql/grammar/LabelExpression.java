// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class LabelExpression implements Serializable, Comparable<LabelExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.LabelExpression");

  public static final hydra.core.Name NEGATION = new hydra.core.Name("negation");

  public static final hydra.core.Name CONJUNCTION = new hydra.core.Name("conjunction");

  public static final hydra.core.Name DISJUNCTION = new hydra.core.Name("disjunction");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name WILDCARD = new hydra.core.Name("wildcard");

  public static final hydra.core.Name PARENTHESIZED = new hydra.core.Name("parenthesized");

  private LabelExpression () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Negation instance) ;

    R visit(Conjunction instance) ;

    R visit(Disjunction instance) ;

    R visit(Name instance) ;

    R visit(Wildcard instance) ;

    R visit(Parenthesized instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(LabelExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Negation instance) {
      return otherwise(instance);
    }

    default R visit(Conjunction instance) {
      return otherwise(instance);
    }

    default R visit(Disjunction instance) {
      return otherwise(instance);
    }

    default R visit(Name instance) {
      return otherwise(instance);
    }

    default R visit(Wildcard instance) {
      return otherwise(instance);
    }

    default R visit(Parenthesized instance) {
      return otherwise(instance);
    }
  }

  public static final class Negation extends openGql.grammar.LabelExpression implements Serializable {
    public final openGql.grammar.LabelExpression value;

    public Negation (openGql.grammar.LabelExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Negation)) {
        return false;
      }
      Negation o = (Negation) other;
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
    public int compareTo(LabelExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Negation o = (Negation) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Conjunction extends openGql.grammar.LabelExpression implements Serializable {
    public final openGql.grammar.ConjunctionLabelExpression value;

    public Conjunction (openGql.grammar.ConjunctionLabelExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Conjunction)) {
        return false;
      }
      Conjunction o = (Conjunction) other;
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
    public int compareTo(LabelExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Conjunction o = (Conjunction) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Disjunction extends openGql.grammar.LabelExpression implements Serializable {
    public final openGql.grammar.DisjunctionLabelExpression value;

    public Disjunction (openGql.grammar.DisjunctionLabelExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Disjunction)) {
        return false;
      }
      Disjunction o = (Disjunction) other;
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
    public int compareTo(LabelExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Disjunction o = (Disjunction) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Name extends openGql.grammar.LabelExpression implements Serializable {
    public final String value;

    public Name (String value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Name)) {
        return false;
      }
      Name o = (Name) other;
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
    public int compareTo(LabelExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Name o = (Name) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Wildcard extends openGql.grammar.LabelExpression implements Serializable {
    public Wildcard () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Wildcard)) {
        return false;
      }
      Wildcard o = (Wildcard) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(LabelExpression other) {
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

  public static final class Parenthesized extends openGql.grammar.LabelExpression implements Serializable {
    public final openGql.grammar.LabelExpression value;

    public Parenthesized (openGql.grammar.LabelExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Parenthesized)) {
        return false;
      }
      Parenthesized o = (Parenthesized) other;
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
    public int compareTo(LabelExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Parenthesized o = (Parenthesized) other;
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
