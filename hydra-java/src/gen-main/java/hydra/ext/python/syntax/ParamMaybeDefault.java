// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class ParamMaybeDefault implements Serializable, Comparable<ParamMaybeDefault> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.ParamMaybeDefault");
  
  public static final hydra.core.Name FIELD_NAME_PARAM = new hydra.core.Name("param");
  
  public static final hydra.core.Name FIELD_NAME_DEFAULT = new hydra.core.Name("default");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_COMMENT = new hydra.core.Name("typeComment");
  
  public final hydra.ext.python.syntax.Param param;
  
  public final hydra.util.Maybe<hydra.ext.python.syntax.Default> default_;
  
  public final hydra.util.Maybe<hydra.ext.python.syntax.TypeComment> typeComment;
  
  public ParamMaybeDefault (hydra.ext.python.syntax.Param param, hydra.util.Maybe<hydra.ext.python.syntax.Default> default_, hydra.util.Maybe<hydra.ext.python.syntax.TypeComment> typeComment) {
    this.param = param;
    this.default_ = default_;
    this.typeComment = typeComment;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ParamMaybeDefault)) {
      return false;
    }
    ParamMaybeDefault o = (ParamMaybeDefault) other;
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
  public int compareTo(ParamMaybeDefault other) {
    int cmp = 0;
    cmp = ((Comparable) param).compareTo(other.param);
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      default_.hashCode(),
      other.default_.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      typeComment.hashCode(),
      other.typeComment.hashCode());
  }
  
  public ParamMaybeDefault withParam(hydra.ext.python.syntax.Param param) {
    return new ParamMaybeDefault(param, default_, typeComment);
  }
  
  public ParamMaybeDefault withDefault(hydra.util.Maybe<hydra.ext.python.syntax.Default> default_) {
    return new ParamMaybeDefault(param, default_, typeComment);
  }
  
  public ParamMaybeDefault withTypeComment(hydra.util.Maybe<hydra.ext.python.syntax.TypeComment> typeComment) {
    return new ParamMaybeDefault(param, default_, typeComment);
  }
}
