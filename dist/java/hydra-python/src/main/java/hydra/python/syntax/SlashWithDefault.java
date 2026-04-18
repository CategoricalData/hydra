// Note: this is an automatically generated file. Do not edit.

package hydra.python.syntax;

import java.io.Serializable;

public class SlashWithDefault implements Serializable, Comparable<SlashWithDefault> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.python.syntax.SlashWithDefault");

  public static final hydra.core.Name PARAM_NO_DEFAULT = new hydra.core.Name("paramNoDefault");

  public static final hydra.core.Name PARAM_WITH_DEFAULT = new hydra.core.Name("paramWithDefault");

  public final java.util.List<hydra.python.syntax.ParamNoDefault> paramNoDefault;

  public final java.util.List<hydra.python.syntax.ParamWithDefault> paramWithDefault;

  public SlashWithDefault (java.util.List<hydra.python.syntax.ParamNoDefault> paramNoDefault, java.util.List<hydra.python.syntax.ParamWithDefault> paramWithDefault) {
    this.paramNoDefault = paramNoDefault;
    this.paramWithDefault = paramWithDefault;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SlashWithDefault)) {
      return false;
    }
    SlashWithDefault o = (SlashWithDefault) other;
    return java.util.Objects.equals(
      this.paramNoDefault,
      o.paramNoDefault) && java.util.Objects.equals(
      this.paramWithDefault,
      o.paramWithDefault);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(paramNoDefault) + 3 * java.util.Objects.hashCode(paramWithDefault);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SlashWithDefault other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      paramNoDefault,
      other.paramNoDefault);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      paramWithDefault,
      other.paramWithDefault);
  }

  public SlashWithDefault withParamNoDefault(java.util.List<hydra.python.syntax.ParamNoDefault> paramNoDefault) {
    return new SlashWithDefault(paramNoDefault, paramWithDefault);
  }

  public SlashWithDefault withParamWithDefault(java.util.List<hydra.python.syntax.ParamWithDefault> paramWithDefault) {
    return new SlashWithDefault(paramNoDefault, paramWithDefault);
  }
}
