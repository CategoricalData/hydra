// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class ParamWithDefault implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.ParamWithDefault");
  
  public static final hydra.core.Name FIELD_NAME_PARAM = new hydra.core.Name("param");
  
  public static final hydra.core.Name FIELD_NAME_DEFAULT = new hydra.core.Name("default");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_COMMENT = new hydra.core.Name("typeComment");
  
  public final hydra.ext.python.syntax.Param param;
  
  public final hydra.ext.python.syntax.Default default_;
  
  public final hydra.util.Opt<hydra.ext.python.syntax.TypeComment> typeComment;
  
  public ParamWithDefault (hydra.ext.python.syntax.Param param, hydra.ext.python.syntax.Default default_, hydra.util.Opt<hydra.ext.python.syntax.TypeComment> typeComment) {
    java.util.Objects.requireNonNull((param));
    java.util.Objects.requireNonNull((default_));
    java.util.Objects.requireNonNull((typeComment));
    this.param = param;
    this.default_ = default_;
    this.typeComment = typeComment;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ParamWithDefault)) {
      return false;
    }
    ParamWithDefault o = (ParamWithDefault) (other);
    return param.equals(o.param) && default_.equals(o.default_) && typeComment.equals(o.typeComment);
  }
  
  @Override
  public int hashCode() {
    return 2 * param.hashCode() + 3 * default_.hashCode() + 5 * typeComment.hashCode();
  }
  
  public ParamWithDefault withParam(hydra.ext.python.syntax.Param param) {
    java.util.Objects.requireNonNull((param));
    return new ParamWithDefault(param, default_, typeComment);
  }
  
  public ParamWithDefault withDefault(hydra.ext.python.syntax.Default default_) {
    java.util.Objects.requireNonNull((default_));
    return new ParamWithDefault(param, default_, typeComment);
  }
  
  public ParamWithDefault withTypeComment(hydra.util.Opt<hydra.ext.python.syntax.TypeComment> typeComment) {
    java.util.Objects.requireNonNull((typeComment));
    return new ParamWithDefault(param, default_, typeComment);
  }
}