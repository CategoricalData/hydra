// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class RecordType implements Serializable, Comparable<RecordType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.RecordType");

  public static final hydra.core.Name ANY_RECORD = new hydra.core.Name("anyRecord");

  public static final hydra.core.Name SPECIFIED_RECORD = new hydra.core.Name("specifiedRecord");

  private RecordType () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(AnyRecord instance) ;

    R visit(SpecifiedRecord instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(RecordType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(AnyRecord instance) {
      return otherwise(instance);
    }

    default R visit(SpecifiedRecord instance) {
      return otherwise(instance);
    }
  }

  public static final class AnyRecord extends openGql.grammar.RecordType implements Serializable {
    public final openGql.grammar.AnyRecordType value;

    public AnyRecord (openGql.grammar.AnyRecordType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AnyRecord)) {
        return false;
      }
      AnyRecord o = (AnyRecord) other;
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
    public int compareTo(RecordType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      AnyRecord o = (AnyRecord) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class SpecifiedRecord extends openGql.grammar.RecordType implements Serializable {
    public final openGql.grammar.SpecifiedRecordType value;

    public SpecifiedRecord (openGql.grammar.SpecifiedRecordType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SpecifiedRecord)) {
        return false;
      }
      SpecifiedRecord o = (SpecifiedRecord) other;
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
    public int compareTo(RecordType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      SpecifiedRecord o = (SpecifiedRecord) other;
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
