// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class CaseExpression implements Serializable, Comparable<CaseExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.CaseExpression");

  public static final hydra.core.Name ABBREVIATION = new hydra.core.Name("abbreviation");

  public static final hydra.core.Name SPECIFICATION = new hydra.core.Name("specification");

  private CaseExpression () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Abbreviation instance) ;

    R visit(Specification instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(CaseExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Abbreviation instance) {
      return otherwise(instance);
    }

    default R visit(Specification instance) {
      return otherwise(instance);
    }
  }

  public static final class Abbreviation extends openGql.grammar.CaseExpression implements Serializable {
    public final openGql.grammar.CaseAbbreviation value;

    public Abbreviation (openGql.grammar.CaseAbbreviation value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Abbreviation)) {
        return false;
      }
      Abbreviation o = (Abbreviation) other;
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
    public int compareTo(CaseExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Abbreviation o = (Abbreviation) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Specification extends openGql.grammar.CaseExpression implements Serializable {
    public final openGql.grammar.CaseSpecification value;

    public Specification (openGql.grammar.CaseSpecification value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Specification)) {
        return false;
      }
      Specification o = (Specification) other;
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
    public int compareTo(CaseExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Specification o = (Specification) other;
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
