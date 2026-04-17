// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class ShortestPathSearch implements Serializable, Comparable<ShortestPathSearch> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.ShortestPathSearch");

  public static final hydra.core.Name ALL_SHORTEST = new hydra.core.Name("allShortest");

  public static final hydra.core.Name ANY_SHORTEST = new hydra.core.Name("anyShortest");

  public static final hydra.core.Name COUNTED_SHORTEST = new hydra.core.Name("countedShortest");

  public static final hydra.core.Name COUNTED_SHORTEST_GROUP = new hydra.core.Name("countedShortestGroup");

  private ShortestPathSearch () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(AllShortest instance) ;

    R visit(AnyShortest instance) ;

    R visit(CountedShortest instance) ;

    R visit(CountedShortestGroup instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ShortestPathSearch instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(AllShortest instance) {
      return otherwise(instance);
    }

    default R visit(AnyShortest instance) {
      return otherwise(instance);
    }

    default R visit(CountedShortest instance) {
      return otherwise(instance);
    }

    default R visit(CountedShortestGroup instance) {
      return otherwise(instance);
    }
  }

  public static final class AllShortest extends openGql.grammar.ShortestPathSearch implements Serializable {
    public final openGql.grammar.AllShortestPathSearch value;

    public AllShortest (openGql.grammar.AllShortestPathSearch value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AllShortest)) {
        return false;
      }
      AllShortest o = (AllShortest) other;
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
    public int compareTo(ShortestPathSearch other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      AllShortest o = (AllShortest) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class AnyShortest extends openGql.grammar.ShortestPathSearch implements Serializable {
    public final openGql.grammar.AnyShortestPathSearch value;

    public AnyShortest (openGql.grammar.AnyShortestPathSearch value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AnyShortest)) {
        return false;
      }
      AnyShortest o = (AnyShortest) other;
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
    public int compareTo(ShortestPathSearch other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      AnyShortest o = (AnyShortest) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class CountedShortest extends openGql.grammar.ShortestPathSearch implements Serializable {
    public final openGql.grammar.CountedShortestPathSearch value;

    public CountedShortest (openGql.grammar.CountedShortestPathSearch value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof CountedShortest)) {
        return false;
      }
      CountedShortest o = (CountedShortest) other;
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
    public int compareTo(ShortestPathSearch other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      CountedShortest o = (CountedShortest) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class CountedShortestGroup extends openGql.grammar.ShortestPathSearch implements Serializable {
    public final openGql.grammar.CountedShortestGroupSearch value;

    public CountedShortestGroup (openGql.grammar.CountedShortestGroupSearch value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof CountedShortestGroup)) {
        return false;
      }
      CountedShortestGroup o = (CountedShortestGroup) other;
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
    public int compareTo(ShortestPathSearch other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      CountedShortestGroup o = (CountedShortestGroup) other;
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
