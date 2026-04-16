// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class PathPatternPrefix implements Serializable, Comparable<PathPatternPrefix> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.PathPatternPrefix");

  public static final hydra.core.Name MODE_PREFIX = new hydra.core.Name("modePrefix");

  public static final hydra.core.Name SEARCH_PREFIX = new hydra.core.Name("searchPrefix");

  private PathPatternPrefix () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(ModePrefix instance) ;

    R visit(SearchPrefix instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(PathPatternPrefix instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(ModePrefix instance) {
      return otherwise(instance);
    }

    default R visit(SearchPrefix instance) {
      return otherwise(instance);
    }
  }

  public static final class ModePrefix extends openGql.grammar.PathPatternPrefix implements Serializable {
    public final openGql.grammar.PathModePrefix value;

    public ModePrefix (openGql.grammar.PathModePrefix value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ModePrefix)) {
        return false;
      }
      ModePrefix o = (ModePrefix) other;
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
    public int compareTo(PathPatternPrefix other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ModePrefix o = (ModePrefix) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class SearchPrefix extends openGql.grammar.PathPatternPrefix implements Serializable {
    public final openGql.grammar.PathSearchPrefix value;

    public SearchPrefix (openGql.grammar.PathSearchPrefix value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SearchPrefix)) {
        return false;
      }
      SearchPrefix o = (SearchPrefix) other;
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
    public int compareTo(PathPatternPrefix other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      SearchPrefix o = (SearchPrefix) other;
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
