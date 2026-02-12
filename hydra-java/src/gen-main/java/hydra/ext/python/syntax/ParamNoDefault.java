// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class ParamNoDefault implements Serializable, Comparable<ParamNoDefault> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.ParamNoDefault");
  
  public static final hydra.core.Name FIELD_NAME_PARAM = new hydra.core.Name("param");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_COMMENT = new hydra.core.Name("typeComment");
  
  public final hydra.ext.python.syntax.Param param;
  
  public final hydra.util.Maybe<hydra.ext.python.syntax.TypeComment> typeComment;
  
  public ParamNoDefault (hydra.ext.python.syntax.Param param, hydra.util.Maybe<hydra.ext.python.syntax.TypeComment> typeComment) {
    this.param = param;
    this.typeComment = typeComment;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ParamNoDefault)) {
      return false;
    }
    ParamNoDefault o = (ParamNoDefault) other;
    return java.util.Objects.equals(
      this.param,
      o.param) && java.util.Objects.equals(
      this.typeComment,
      o.typeComment);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(param) + 3 * java.util.Objects.hashCode(typeComment);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ParamNoDefault other) {
    int cmp = 0;
    cmp = ((Comparable) param).compareTo(other.param);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      typeComment.hashCode(),
      other.typeComment.hashCode());
  }
  
  public ParamNoDefault withParam(hydra.ext.python.syntax.Param param) {
    return new ParamNoDefault(param, typeComment);
  }
  
  public ParamNoDefault withTypeComment(hydra.util.Maybe<hydra.ext.python.syntax.TypeComment> typeComment) {
    return new ParamNoDefault(param, typeComment);
  }
}
