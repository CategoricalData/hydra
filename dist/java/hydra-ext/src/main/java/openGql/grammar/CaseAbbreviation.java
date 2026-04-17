// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class CaseAbbreviation implements Serializable, Comparable<CaseAbbreviation> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.CaseAbbreviation");

  public static final hydra.core.Name NULL_IF = new hydra.core.Name("nullIf");

  public static final hydra.core.Name COALESCE = new hydra.core.Name("coalesce");

  private CaseAbbreviation () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(NullIf instance) ;

    R visit(Coalesce instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(CaseAbbreviation instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(NullIf instance) {
      return otherwise(instance);
    }

    default R visit(Coalesce instance) {
      return otherwise(instance);
    }
  }

  public static final class NullIf extends openGql.grammar.CaseAbbreviation implements Serializable {
    public final openGql.grammar.NullIfAbbreviation value;

    public NullIf (openGql.grammar.NullIfAbbreviation value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NullIf)) {
        return false;
      }
      NullIf o = (NullIf) other;
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
    public int compareTo(CaseAbbreviation other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      NullIf o = (NullIf) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Coalesce extends openGql.grammar.CaseAbbreviation implements Serializable {
    public final java.util.List<openGql.grammar.ValueExpression> value;

    public Coalesce (java.util.List<openGql.grammar.ValueExpression> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Coalesce)) {
        return false;
      }
      Coalesce o = (Coalesce) other;
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
    public int compareTo(CaseAbbreviation other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Coalesce o = (Coalesce) other;
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
