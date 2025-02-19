// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class ParamMaybeDefault implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.ParamMaybeDefault");
  
  public static final hydra.core.Name FIELD_NAME_PARAM = new hydra.core.Name("param");
  
  public static final hydra.core.Name FIELD_NAME_DEFAULT = new hydra.core.Name("default");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_COMMENT = new hydra.core.Name("typeComment");
  
  public final hydra.ext.python.syntax.Param param;
  
  public final hydra.util.Opt<hydra.ext.python.syntax.Default> default_;
  
  public final hydra.util.Opt<hydra.ext.python.syntax.TypeComment> typeComment;
  
  public ParamMaybeDefault (hydra.ext.python.syntax.Param param, hydra.util.Opt<hydra.ext.python.syntax.Default> default_, hydra.util.Opt<hydra.ext.python.syntax.TypeComment> typeComment) {
    java.util.Objects.requireNonNull((param));
    java.util.Objects.requireNonNull((default_));
    java.util.Objects.requireNonNull((typeComment));
    this.param = param;
    this.default_ = default_;
    this.typeComment = typeComment;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ParamMaybeDefault)) {
      return false;
    }
    ParamMaybeDefault o = (ParamMaybeDefault) (other);
    return param.equals(o.param) && default_.equals(o.default_) && typeComment.equals(o.typeComment);
  }
  
  @Override
  public int hashCode() {
    return 2 * param.hashCode() + 3 * default_.hashCode() + 5 * typeComment.hashCode();
  }
  
  public ParamMaybeDefault withParam(hydra.ext.python.syntax.Param param) {
    java.util.Objects.requireNonNull((param));
    return new ParamMaybeDefault(param, default_, typeComment);
  }
  
  public ParamMaybeDefault withDefault(hydra.util.Opt<hydra.ext.python.syntax.Default> default_) {
    java.util.Objects.requireNonNull((default_));
    return new ParamMaybeDefault(param, default_, typeComment);
  }
  
  public ParamMaybeDefault withTypeComment(hydra.util.Opt<hydra.ext.python.syntax.TypeComment> typeComment) {
    java.util.Objects.requireNonNull((typeComment));
    return new ParamMaybeDefault(param, default_, typeComment);
  }
}