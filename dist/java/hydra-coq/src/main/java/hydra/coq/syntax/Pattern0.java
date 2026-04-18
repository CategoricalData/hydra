// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public abstract class Pattern0 implements Serializable, Comparable<Pattern0> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.Pattern0");

  public static final hydra.core.Name QUALID = new hydra.core.Name("qualid");

  public static final hydra.core.Name QUAL_ID_AND_PATTERN = new hydra.core.Name("qualIdAndPattern");

  public static final hydra.core.Name PLACEHOLDER = new hydra.core.Name("placeholder");

  public static final hydra.core.Name PARENS = new hydra.core.Name("parens");

  public static final hydra.core.Name NUMBER = new hydra.core.Name("number");

  public static final hydra.core.Name STRING = new hydra.core.Name("string");

  private Pattern0 () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Qualid instance) ;

    R visit(QualIdAndPattern instance) ;

    R visit(Placeholder instance) ;

    R visit(Parens instance) ;

    R visit(Number_ instance) ;

    R visit(String_ instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Pattern0 instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Qualid instance) {
      return otherwise(instance);
    }

    default R visit(QualIdAndPattern instance) {
      return otherwise(instance);
    }

    default R visit(Placeholder instance) {
      return otherwise(instance);
    }

    default R visit(Parens instance) {
      return otherwise(instance);
    }

    default R visit(Number_ instance) {
      return otherwise(instance);
    }

    default R visit(String_ instance) {
      return otherwise(instance);
    }
  }

  public static final class Qualid extends hydra.coq.syntax.Pattern0 implements Serializable {
    public final hydra.coq.syntax.Qualid value;

    public Qualid (hydra.coq.syntax.Qualid value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Qualid)) {
        return false;
      }
      Qualid o = (Qualid) other;
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
    public int compareTo(Pattern0 other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Qualid o = (Qualid) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class QualIdAndPattern extends hydra.coq.syntax.Pattern0 implements Serializable {
    public final hydra.coq.syntax.QualidAndPattern value;

    public QualIdAndPattern (hydra.coq.syntax.QualidAndPattern value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof QualIdAndPattern)) {
        return false;
      }
      QualIdAndPattern o = (QualIdAndPattern) other;
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
    public int compareTo(Pattern0 other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      QualIdAndPattern o = (QualIdAndPattern) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Placeholder extends hydra.coq.syntax.Pattern0 implements Serializable {
    public Placeholder () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Placeholder)) {
        return false;
      }
      Placeholder o = (Placeholder) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Pattern0 other) {
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

  public static final class Parens extends hydra.coq.syntax.Pattern0 implements Serializable {
    public final java.util.List<hydra.coq.syntax.Pattern> value;

    public Parens (java.util.List<hydra.coq.syntax.Pattern> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Parens)) {
        return false;
      }
      Parens o = (Parens) other;
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
    public int compareTo(Pattern0 other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Parens o = (Parens) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Number_ extends hydra.coq.syntax.Pattern0 implements Serializable {
    public final hydra.coq.syntax.Number_ value;

    public Number_ (hydra.coq.syntax.Number_ value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Number_)) {
        return false;
      }
      Number_ o = (Number_) other;
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
    public int compareTo(Pattern0 other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Number_ o = (Number_) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class String_ extends hydra.coq.syntax.Pattern0 implements Serializable {
    public final hydra.coq.syntax.String_ value;

    public String_ (hydra.coq.syntax.String_ value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof String_)) {
        return false;
      }
      String_ o = (String_) other;
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
    public int compareTo(Pattern0 other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      String_ o = (String_) other;
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
