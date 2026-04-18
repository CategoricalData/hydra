// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class SessionSetAndResetCommands implements Serializable, Comparable<SessionSetAndResetCommands> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.SessionSetAndResetCommands");

  public static final hydra.core.Name SET = new hydra.core.Name("set");

  public static final hydra.core.Name RESET = new hydra.core.Name("reset");

  public final java.util.List<openGql.grammar.SessionSetCommand> set;

  public final java.util.List<hydra.util.Maybe<openGql.grammar.SessionResetArguments>> reset;

  public SessionSetAndResetCommands (java.util.List<openGql.grammar.SessionSetCommand> set, java.util.List<hydra.util.Maybe<openGql.grammar.SessionResetArguments>> reset) {
    this.set = set;
    this.reset = reset;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SessionSetAndResetCommands)) {
      return false;
    }
    SessionSetAndResetCommands o = (SessionSetAndResetCommands) other;
    return java.util.Objects.equals(
      this.set,
      o.set) && java.util.Objects.equals(
      this.reset,
      o.reset);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(set) + 3 * java.util.Objects.hashCode(reset);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SessionSetAndResetCommands other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      set,
      other.set);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      reset,
      other.reset);
  }

  public SessionSetAndResetCommands withSet(java.util.List<openGql.grammar.SessionSetCommand> set) {
    return new SessionSetAndResetCommands(set, reset);
  }

  public SessionSetAndResetCommands withReset(java.util.List<hydra.util.Maybe<openGql.grammar.SessionResetArguments>> reset) {
    return new SessionSetAndResetCommands(set, reset);
  }
}
