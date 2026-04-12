// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public abstract class StringLiteralLong1_Elmt_Sequence_Alts_Option implements Serializable, Comparable<StringLiteralLong1_Elmt_Sequence_Alts_Option> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.StringLiteralLong1_Elmt_Sequence_Alts_Option");

  public static final hydra.core.Name APOS = new hydra.core.Name("Apos");

  public static final hydra.core.Name SEQUENCE = new hydra.core.Name("sequence");

  private StringLiteralLong1_Elmt_Sequence_Alts_Option () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Apos instance) ;

    R visit(Sequence instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(StringLiteralLong1_Elmt_Sequence_Alts_Option instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Apos instance) {
      return otherwise(instance);
    }

    default R visit(Sequence instance) {
      return otherwise(instance);
    }
  }

  public static final class Apos extends hydra.ext.io.shex.syntax.StringLiteralLong1_Elmt_Sequence_Alts_Option implements Serializable {
    public Apos () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Apos)) {
        return false;
      }
      Apos o = (Apos) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(StringLiteralLong1_Elmt_Sequence_Alts_Option other) {
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

  public static final class Sequence extends hydra.ext.io.shex.syntax.StringLiteralLong1_Elmt_Sequence_Alts_Option implements Serializable {
    public final hydra.ext.io.shex.syntax.StringLiteralLong1_Elmt_Sequence_Alts_Option_Sequence value;

    public Sequence (hydra.ext.io.shex.syntax.StringLiteralLong1_Elmt_Sequence_Alts_Option_Sequence value) {
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
    public int compareTo(StringLiteralLong1_Elmt_Sequence_Alts_Option other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Sequence o = (Sequence) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
