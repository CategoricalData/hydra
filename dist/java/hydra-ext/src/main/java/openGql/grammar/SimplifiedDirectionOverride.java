// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class SimplifiedDirectionOverride implements Serializable, Comparable<SimplifiedDirectionOverride> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.SimplifiedDirectionOverride");

  public static final hydra.core.Name OVERRIDE_LEFT = new hydra.core.Name("overrideLeft");

  public static final hydra.core.Name OVERRIDE_UNDIRECTED = new hydra.core.Name("overrideUndirected");

  public static final hydra.core.Name OVERRIDE_RIGHT = new hydra.core.Name("overrideRight");

  public static final hydra.core.Name OVERRIDE_LEFT_OR_UNDIRECTED = new hydra.core.Name("overrideLeftOrUndirected");

  public static final hydra.core.Name OVERRIDE_UNDIRECTED_OR_RIGHT = new hydra.core.Name("overrideUndirectedOrRight");

  public static final hydra.core.Name OVERRIDE_LEFT_OR_RIGHT = new hydra.core.Name("overrideLeftOrRight");

  public static final hydra.core.Name OVERRIDE_ANY_DIRECTION = new hydra.core.Name("overrideAnyDirection");

  private SimplifiedDirectionOverride () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(OverrideLeft instance) ;

    R visit(OverrideUndirected instance) ;

    R visit(OverrideRight instance) ;

    R visit(OverrideLeftOrUndirected instance) ;

    R visit(OverrideUndirectedOrRight instance) ;

    R visit(OverrideLeftOrRight instance) ;

    R visit(OverrideAnyDirection instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SimplifiedDirectionOverride instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(OverrideLeft instance) {
      return otherwise(instance);
    }

    default R visit(OverrideUndirected instance) {
      return otherwise(instance);
    }

    default R visit(OverrideRight instance) {
      return otherwise(instance);
    }

    default R visit(OverrideLeftOrUndirected instance) {
      return otherwise(instance);
    }

    default R visit(OverrideUndirectedOrRight instance) {
      return otherwise(instance);
    }

    default R visit(OverrideLeftOrRight instance) {
      return otherwise(instance);
    }

    default R visit(OverrideAnyDirection instance) {
      return otherwise(instance);
    }
  }

  public static final class OverrideLeft extends openGql.grammar.SimplifiedDirectionOverride implements Serializable {
    public final openGql.grammar.SimplifiedSecondary value;

    public OverrideLeft (openGql.grammar.SimplifiedSecondary value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof OverrideLeft)) {
        return false;
      }
      OverrideLeft o = (OverrideLeft) other;
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
    public int compareTo(SimplifiedDirectionOverride other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      OverrideLeft o = (OverrideLeft) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class OverrideUndirected extends openGql.grammar.SimplifiedDirectionOverride implements Serializable {
    public final openGql.grammar.SimplifiedSecondary value;

    public OverrideUndirected (openGql.grammar.SimplifiedSecondary value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof OverrideUndirected)) {
        return false;
      }
      OverrideUndirected o = (OverrideUndirected) other;
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
    public int compareTo(SimplifiedDirectionOverride other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      OverrideUndirected o = (OverrideUndirected) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class OverrideRight extends openGql.grammar.SimplifiedDirectionOverride implements Serializable {
    public final openGql.grammar.SimplifiedSecondary value;

    public OverrideRight (openGql.grammar.SimplifiedSecondary value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof OverrideRight)) {
        return false;
      }
      OverrideRight o = (OverrideRight) other;
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
    public int compareTo(SimplifiedDirectionOverride other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      OverrideRight o = (OverrideRight) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class OverrideLeftOrUndirected extends openGql.grammar.SimplifiedDirectionOverride implements Serializable {
    public final openGql.grammar.SimplifiedSecondary value;

    public OverrideLeftOrUndirected (openGql.grammar.SimplifiedSecondary value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof OverrideLeftOrUndirected)) {
        return false;
      }
      OverrideLeftOrUndirected o = (OverrideLeftOrUndirected) other;
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
    public int compareTo(SimplifiedDirectionOverride other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      OverrideLeftOrUndirected o = (OverrideLeftOrUndirected) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class OverrideUndirectedOrRight extends openGql.grammar.SimplifiedDirectionOverride implements Serializable {
    public final openGql.grammar.SimplifiedSecondary value;

    public OverrideUndirectedOrRight (openGql.grammar.SimplifiedSecondary value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof OverrideUndirectedOrRight)) {
        return false;
      }
      OverrideUndirectedOrRight o = (OverrideUndirectedOrRight) other;
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
    public int compareTo(SimplifiedDirectionOverride other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      OverrideUndirectedOrRight o = (OverrideUndirectedOrRight) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class OverrideLeftOrRight extends openGql.grammar.SimplifiedDirectionOverride implements Serializable {
    public final openGql.grammar.SimplifiedSecondary value;

    public OverrideLeftOrRight (openGql.grammar.SimplifiedSecondary value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof OverrideLeftOrRight)) {
        return false;
      }
      OverrideLeftOrRight o = (OverrideLeftOrRight) other;
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
    public int compareTo(SimplifiedDirectionOverride other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      OverrideLeftOrRight o = (OverrideLeftOrRight) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class OverrideAnyDirection extends openGql.grammar.SimplifiedDirectionOverride implements Serializable {
    public final openGql.grammar.SimplifiedSecondary value;

    public OverrideAnyDirection (openGql.grammar.SimplifiedSecondary value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof OverrideAnyDirection)) {
        return false;
      }
      OverrideAnyDirection o = (OverrideAnyDirection) other;
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
    public int compareTo(SimplifiedDirectionOverride other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      OverrideAnyDirection o = (OverrideAnyDirection) other;
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
