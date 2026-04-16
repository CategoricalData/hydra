// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class TimeFunction implements Serializable, Comparable<TimeFunction> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.TimeFunction");

  public static final hydra.core.Name CURRENT_TIME = new hydra.core.Name("currentTime");

  public static final hydra.core.Name ZONED_TIME_WITH_PARAMS = new hydra.core.Name("zonedTimeWithParams");

  private TimeFunction () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(CurrentTime instance) ;

    R visit(ZonedTimeWithParams instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TimeFunction instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(CurrentTime instance) {
      return otherwise(instance);
    }

    default R visit(ZonedTimeWithParams instance) {
      return otherwise(instance);
    }
  }

  public static final class CurrentTime extends openGql.grammar.TimeFunction implements Serializable {
    public CurrentTime () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof CurrentTime)) {
        return false;
      }
      CurrentTime o = (CurrentTime) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TimeFunction other) {
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

  public static final class ZonedTimeWithParams extends openGql.grammar.TimeFunction implements Serializable {
    public final hydra.util.Maybe<openGql.grammar.TimeFunctionParameters> value;

    public ZonedTimeWithParams (hydra.util.Maybe<openGql.grammar.TimeFunctionParameters> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ZonedTimeWithParams)) {
        return false;
      }
      ZonedTimeWithParams o = (ZonedTimeWithParams) other;
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
    public int compareTo(TimeFunction other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ZonedTimeWithParams o = (ZonedTimeWithParams) other;
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
