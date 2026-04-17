// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class SimplifiedTertiary implements Serializable, Comparable<SimplifiedTertiary> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.SimplifiedTertiary");

  public static final hydra.core.Name DIRECTION_OVERRIDE = new hydra.core.Name("directionOverride");

  public static final hydra.core.Name SECONDARY = new hydra.core.Name("secondary");

  private SimplifiedTertiary () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(DirectionOverride instance) ;

    R visit(Secondary instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SimplifiedTertiary instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(DirectionOverride instance) {
      return otherwise(instance);
    }

    default R visit(Secondary instance) {
      return otherwise(instance);
    }
  }

  public static final class DirectionOverride extends openGql.grammar.SimplifiedTertiary implements Serializable {
    public final openGql.grammar.SimplifiedDirectionOverride value;

    public DirectionOverride (openGql.grammar.SimplifiedDirectionOverride value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DirectionOverride)) {
        return false;
      }
      DirectionOverride o = (DirectionOverride) other;
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
    public int compareTo(SimplifiedTertiary other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DirectionOverride o = (DirectionOverride) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Secondary extends openGql.grammar.SimplifiedTertiary implements Serializable {
    public final openGql.grammar.SimplifiedSecondary value;

    public Secondary (openGql.grammar.SimplifiedSecondary value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Secondary)) {
        return false;
      }
      Secondary o = (Secondary) other;
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
    public int compareTo(SimplifiedTertiary other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Secondary o = (Secondary) other;
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
