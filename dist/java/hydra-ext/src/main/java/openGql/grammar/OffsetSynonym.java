// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class OffsetSynonym implements Serializable, Comparable<OffsetSynonym> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.OffsetSynonym");

  public static final hydra.core.Name OFFSET = new hydra.core.Name("offset");

  public static final hydra.core.Name SKIP_RESERVED_WORD = new hydra.core.Name("skipReservedWord");

  private OffsetSynonym () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Offset instance) ;

    R visit(SkipReservedWord instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(OffsetSynonym instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Offset instance) {
      return otherwise(instance);
    }

    default R visit(SkipReservedWord instance) {
      return otherwise(instance);
    }
  }

  public static final class Offset extends openGql.grammar.OffsetSynonym implements Serializable {
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
    public int compareTo(OffsetSynonym other) {
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

  public static final class SkipReservedWord extends openGql.grammar.OffsetSynonym implements Serializable {
    public SkipReservedWord () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SkipReservedWord)) {
        return false;
      }
      SkipReservedWord o = (SkipReservedWord) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(OffsetSynonym other) {
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
