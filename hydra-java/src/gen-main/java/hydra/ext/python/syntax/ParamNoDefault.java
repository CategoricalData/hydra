// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class ParamNoDefault implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.ParamNoDefault");
  
  public static final hydra.core.Name FIELD_NAME_PARAM = new hydra.core.Name("param");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_COMMENT = new hydra.core.Name("typeComment");
  
  public final hydra.ext.python.syntax.Param param;
  
  public final hydra.util.Opt<hydra.ext.python.syntax.TypeComment> typeComment;
  
  public ParamNoDefault (hydra.ext.python.syntax.Param param, hydra.util.Opt<hydra.ext.python.syntax.TypeComment> typeComment) {
    java.util.Objects.requireNonNull((param));
    java.util.Objects.requireNonNull((typeComment));
    this.param = param;
    this.typeComment = typeComment;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ParamNoDefault)) {
      return false;
    }
    ParamNoDefault o = (ParamNoDefault) (other);
    return param.equals(o.param) && typeComment.equals(o.typeComment);
  }
  
  @Override
  public int hashCode() {
    return 2 * param.hashCode() + 3 * typeComment.hashCode();
  }
  
  public ParamNoDefault withParam(hydra.ext.python.syntax.Param param) {
    java.util.Objects.requireNonNull((param));
    return new ParamNoDefault(param, typeComment);
  }
  
  public ParamNoDefault withTypeComment(hydra.util.Opt<hydra.ext.python.syntax.TypeComment> typeComment) {
    java.util.Objects.requireNonNull((typeComment));
    return new ParamNoDefault(param, typeComment);
  }
}