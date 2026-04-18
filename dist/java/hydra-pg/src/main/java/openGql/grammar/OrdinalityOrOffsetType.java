// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class OrdinalityOrOffsetType implements Serializable, Comparable<OrdinalityOrOffsetType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.OrdinalityOrOffsetType");

  public static final hydra.core.Name ORDINALITY = new hydra.core.Name("ordinality");

  public static final hydra.core.Name OFFSET = new hydra.core.Name("offset");

  private OrdinalityOrOffsetType () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Ordinality instance) ;

    R visit(Offset instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(OrdinalityOrOffsetType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Ordinality instance) {
      return otherwise(instance);
    }

    default R visit(Offset instance) {
      return otherwise(instance);
    }
  }

  public static final class Ordinality extends openGql.grammar.OrdinalityOrOffsetType implements Serializable {
    public Ordinality () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Ordinality)) {
        return false;
      }
      Ordinality o = (Ordinality) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(OrdinalityOrOffsetType other) {
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

  public static final class Offset extends openGql.grammar.OrdinalityOrOffsetType implements Serializable {
    public Offset () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Offset)) {
        return false;
      }
      Offset o = (Offset) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(OrdinalityOrOffsetType other) {
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
