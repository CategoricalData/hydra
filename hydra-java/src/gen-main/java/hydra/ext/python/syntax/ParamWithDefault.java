// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class ParamWithDefault implements Serializable, Comparable<ParamWithDefault> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.ParamWithDefault");
  
  public static final hydra.core.Name FIELD_NAME_PARAM = new hydra.core.Name("param");
  
  public static final hydra.core.Name FIELD_NAME_DEFAULT = new hydra.core.Name("default");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_COMMENT = new hydra.core.Name("typeComment");
  
  public final hydra.ext.python.syntax.Param param;
  
  public final hydra.ext.python.syntax.Default default_;
  
  public final hydra.util.Maybe<hydra.ext.python.syntax.TypeComment> typeComment;
  
  public ParamWithDefault (hydra.ext.python.syntax.Param param, hydra.ext.python.syntax.Default default_, hydra.util.Maybe<hydra.ext.python.syntax.TypeComment> typeComment) {
    this.param = param;
    this.default_ = default_;
    this.typeComment = typeComment;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ParamWithDefault)) {
      return false;
    }
    ParamWithDefault o = (ParamWithDefault) other;
    return java.util.Objects.equals(
      this.param,
      o.param) && java.util.Objects.equals(
      this.default_,
      o.default_) && java.util.Objects.equals(
      this.typeComment,
      o.typeComment);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(param) + 3 * java.util.Objects.hashCode(default_) + 5 * java.util.Objects.hashCode(typeComment);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ParamWithDefault other) {
    int cmp = 0;
    cmp = ((Comparable) param).compareTo(other.param);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) default_).compareTo(other.default_);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      typeComment.hashCode(),
      other.typeComment.hashCode());
  }
  
  public ParamWithDefault withParam(hydra.ext.python.syntax.Param param) {
    return new ParamWithDefault(param, default_, typeComment);
  }
  
  public ParamWithDefault withDefault(hydra.ext.python.syntax.Default default_) {
    return new ParamWithDefault(param, default_, typeComment);
  }
  
  public ParamWithDefault withTypeComment(hydra.util.Maybe<hydra.ext.python.syntax.TypeComment> typeComment) {
    return new ParamWithDefault(param, default_, typeComment);
  }
}
