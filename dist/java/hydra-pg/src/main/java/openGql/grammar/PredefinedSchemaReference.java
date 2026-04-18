// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class PredefinedSchemaReference implements Serializable, Comparable<PredefinedSchemaReference> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.PredefinedSchemaReference");

  public static final hydra.core.Name HOME_SCHEMA = new hydra.core.Name("homeSchema");

  public static final hydra.core.Name CURRENT_SCHEMA = new hydra.core.Name("currentSchema");

  public static final hydra.core.Name PERIOD = new hydra.core.Name("period");

  private PredefinedSchemaReference () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(HomeSchema instance) ;

    R visit(CurrentSchema instance) ;

    R visit(Period instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(PredefinedSchemaReference instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(HomeSchema instance) {
      return otherwise(instance);
    }

    default R visit(CurrentSchema instance) {
      return otherwise(instance);
    }

    default R visit(Period instance) {
      return otherwise(instance);
    }
  }

  public static final class HomeSchema extends openGql.grammar.PredefinedSchemaReference implements Serializable {
    public HomeSchema () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof HomeSchema)) {
        return false;
      }
      HomeSchema o = (HomeSchema) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(PredefinedSchemaReference other) {
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

  public static final class CurrentSchema extends openGql.grammar.PredefinedSchemaReference implements Serializable {
    public CurrentSchema () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof CurrentSchema)) {
        return false;
      }
      CurrentSchema o = (CurrentSchema) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(PredefinedSchemaReference other) {
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

  public static final class Period extends openGql.grammar.PredefinedSchemaReference implements Serializable {
    public Period () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Period)) {
        return false;
      }
      Period o = (Period) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(PredefinedSchemaReference other) {
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
