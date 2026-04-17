// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class GroupingElementList implements Serializable, Comparable<GroupingElementList> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.GroupingElementList");

  public static final hydra.core.Name ELEMENTS = new hydra.core.Name("elements");

  public static final hydra.core.Name EMPTY_SET = new hydra.core.Name("emptySet");

  private GroupingElementList () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Elements_ instance) ;

    R visit(EmptySet instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(GroupingElementList instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Elements_ instance) {
      return otherwise(instance);
    }

    default R visit(EmptySet instance) {
      return otherwise(instance);
    }
  }

  public static final class Elements_ extends openGql.grammar.GroupingElementList implements Serializable {
    public final java.util.List<String> value;

    public Elements_ (java.util.List<String> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Elements_)) {
        return false;
      }
      Elements_ o = (Elements_) other;
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
    public int compareTo(GroupingElementList other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Elements_ o = (Elements_) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class EmptySet extends openGql.grammar.GroupingElementList implements Serializable {
    public EmptySet () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof EmptySet)) {
        return false;
      }
      EmptySet o = (EmptySet) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(GroupingElementList other) {
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
