// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class TemporalDurationQualifier implements Serializable, Comparable<TemporalDurationQualifier> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.TemporalDurationQualifier");

  public static final hydra.core.Name YEAR_TO_MONTH = new hydra.core.Name("yearToMonth");

  public static final hydra.core.Name DAY_TO_SECOND = new hydra.core.Name("dayToSecond");

  private TemporalDurationQualifier () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(YearToMonth instance) ;

    R visit(DayToSecond instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TemporalDurationQualifier instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(YearToMonth instance) {
      return otherwise(instance);
    }

    default R visit(DayToSecond instance) {
      return otherwise(instance);
    }
  }

  public static final class YearToMonth extends openGql.grammar.TemporalDurationQualifier implements Serializable {
    public YearToMonth () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof YearToMonth)) {
        return false;
      }
      YearToMonth o = (YearToMonth) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TemporalDurationQualifier other) {
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

  public static final class DayToSecond extends openGql.grammar.TemporalDurationQualifier implements Serializable {
    public DayToSecond () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DayToSecond)) {
        return false;
      }
      DayToSecond o = (DayToSecond) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TemporalDurationQualifier other) {
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
