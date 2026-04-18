// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class GqlProgram implements Serializable, Comparable<GqlProgram> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.GqlProgram");

  public static final hydra.core.Name ACTIVITY = new hydra.core.Name("activity");

  public static final hydra.core.Name CLOSE = new hydra.core.Name("close");

  public final hydra.util.Maybe<openGql.grammar.ProgramActivity> activity;

  public final hydra.util.Maybe<java.lang.Void> close;

  public GqlProgram (hydra.util.Maybe<openGql.grammar.ProgramActivity> activity, hydra.util.Maybe<java.lang.Void> close) {
    this.activity = activity;
    this.close = close;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof GqlProgram)) {
      return false;
    }
    GqlProgram o = (GqlProgram) other;
    return java.util.Objects.equals(
      this.activity,
      o.activity) && java.util.Objects.equals(
      this.close,
      o.close);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(activity) + 3 * java.util.Objects.hashCode(close);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(GqlProgram other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      activity,
      other.activity);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      close,
      other.close);
  }

  public GqlProgram withActivity(hydra.util.Maybe<openGql.grammar.ProgramActivity> activity) {
    return new GqlProgram(activity, close);
  }

  public GqlProgram withClose(hydra.util.Maybe<java.lang.Void> close) {
    return new GqlProgram(activity, close);
  }
}
