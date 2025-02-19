// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class SlashWithDefault implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.SlashWithDefault");
  
  public static final hydra.core.Name FIELD_NAME_PARAM_NO_DEFAULT = new hydra.core.Name("paramNoDefault");
  
  public static final hydra.core.Name FIELD_NAME_PARAM_WITH_DEFAULT = new hydra.core.Name("paramWithDefault");
  
  public final java.util.List<hydra.ext.python.syntax.ParamNoDefault> paramNoDefault;
  
  public final java.util.List<hydra.ext.python.syntax.ParamWithDefault> paramWithDefault;
  
  public SlashWithDefault (java.util.List<hydra.ext.python.syntax.ParamNoDefault> paramNoDefault, java.util.List<hydra.ext.python.syntax.ParamWithDefault> paramWithDefault) {
    java.util.Objects.requireNonNull((paramNoDefault));
    java.util.Objects.requireNonNull((paramWithDefault));
    this.paramNoDefault = paramNoDefault;
    this.paramWithDefault = paramWithDefault;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SlashWithDefault)) {
      return false;
    }
    SlashWithDefault o = (SlashWithDefault) (other);
    return paramNoDefault.equals(o.paramNoDefault) && paramWithDefault.equals(o.paramWithDefault);
  }
  
  @Override
  public int hashCode() {
    return 2 * paramNoDefault.hashCode() + 3 * paramWithDefault.hashCode();
  }
  
  public SlashWithDefault withParamNoDefault(java.util.List<hydra.ext.python.syntax.ParamNoDefault> paramNoDefault) {
    java.util.Objects.requireNonNull((paramNoDefault));
    return new SlashWithDefault(paramNoDefault, paramWithDefault);
  }
  
  public SlashWithDefault withParamWithDefault(java.util.List<hydra.ext.python.syntax.ParamWithDefault> paramWithDefault) {
    java.util.Objects.requireNonNull((paramWithDefault));
    return new SlashWithDefault(paramNoDefault, paramWithDefault);
  }
}