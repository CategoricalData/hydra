// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class SessionActivity implements Serializable, Comparable<SessionActivity> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.SessionActivity");

  public static final hydra.core.Name RESET = new hydra.core.Name("reset");

  public static final hydra.core.Name SET_AND_RESET_COMMANDS = new hydra.core.Name("setAndResetCommands");

  private SessionActivity () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Reset instance) ;

    R visit(SetAndResetCommands instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SessionActivity instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Reset instance) {
      return otherwise(instance);
    }

    default R visit(SetAndResetCommands instance) {
      return otherwise(instance);
    }
  }

  public static final class Reset extends openGql.grammar.SessionActivity implements Serializable {
    public final java.util.List<hydra.util.Maybe<openGql.grammar.SessionResetArguments>> value;

    public Reset (java.util.List<hydra.util.Maybe<openGql.grammar.SessionResetArguments>> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Reset)) {
        return false;
      }
      Reset o = (Reset) other;
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
    public int compareTo(SessionActivity other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Reset o = (Reset) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class SetAndResetCommands extends openGql.grammar.SessionActivity implements Serializable {
    public final openGql.grammar.SessionSetAndResetCommands value;

    public SetAndResetCommands (openGql.grammar.SessionSetAndResetCommands value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SetAndResetCommands)) {
        return false;
      }
      SetAndResetCommands o = (SetAndResetCommands) other;
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
    public int compareTo(SessionActivity other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      SetAndResetCommands o = (SetAndResetCommands) other;
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
