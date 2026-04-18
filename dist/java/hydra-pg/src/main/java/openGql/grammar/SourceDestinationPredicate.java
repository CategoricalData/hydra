// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class SourceDestinationPredicate implements Serializable, Comparable<SourceDestinationPredicate> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.SourceDestinationPredicate");

  public static final hydra.core.Name SOURCE_PREDICATE = new hydra.core.Name("sourcePredicate");

  public static final hydra.core.Name DESTINATION_PREDICATE = new hydra.core.Name("destinationPredicate");

  private SourceDestinationPredicate () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(SourcePredicate instance) ;

    R visit(DestinationPredicate instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SourceDestinationPredicate instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(SourcePredicate instance) {
      return otherwise(instance);
    }

    default R visit(DestinationPredicate instance) {
      return otherwise(instance);
    }
  }

  public static final class SourcePredicate extends openGql.grammar.SourceDestinationPredicate implements Serializable {
    public final openGql.grammar.SourcePredicate value;

    public SourcePredicate (openGql.grammar.SourcePredicate value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SourcePredicate)) {
        return false;
      }
      SourcePredicate o = (SourcePredicate) other;
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
    public int compareTo(SourceDestinationPredicate other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      SourcePredicate o = (SourcePredicate) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class DestinationPredicate extends openGql.grammar.SourceDestinationPredicate implements Serializable {
    public final openGql.grammar.DestinationPredicate value;

    public DestinationPredicate (openGql.grammar.DestinationPredicate value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DestinationPredicate)) {
        return false;
      }
      DestinationPredicate o = (DestinationPredicate) other;
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
    public int compareTo(SourceDestinationPredicate other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DestinationPredicate o = (DestinationPredicate) other;
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
