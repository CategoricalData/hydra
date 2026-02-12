// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class ForStatement implements Serializable, Comparable<ForStatement> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.ForStatement");
  
  public static final hydra.core.Name FIELD_NAME_ASYNC = new hydra.core.Name("async");
  
  public static final hydra.core.Name FIELD_NAME_TARGETS = new hydra.core.Name("targets");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSIONS = new hydra.core.Name("expressions");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_COMMENT = new hydra.core.Name("typeComment");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public static final hydra.core.Name FIELD_NAME_ELSE = new hydra.core.Name("else");
  
  public final Boolean async;
  
  public final java.util.List<hydra.ext.python.syntax.StarTarget> targets;
  
  public final java.util.List<hydra.ext.python.syntax.StarExpression> expressions;
  
  public final hydra.util.Maybe<hydra.ext.python.syntax.TypeComment> typeComment;
  
  public final hydra.ext.python.syntax.Block body;
  
  public final hydra.util.Maybe<hydra.ext.python.syntax.Block> else_;
  
  public ForStatement (Boolean async, java.util.List<hydra.ext.python.syntax.StarTarget> targets, java.util.List<hydra.ext.python.syntax.StarExpression> expressions, hydra.util.Maybe<hydra.ext.python.syntax.TypeComment> typeComment, hydra.ext.python.syntax.Block body, hydra.util.Maybe<hydra.ext.python.syntax.Block> else_) {
    this.async = async;
    this.targets = targets;
    this.expressions = expressions;
    this.typeComment = typeComment;
    this.body = body;
    this.else_ = else_;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ForStatement)) {
      return false;
    }
    ForStatement o = (ForStatement) other;
    return java.util.Objects.equals(
      this.async,
      o.async) && java.util.Objects.equals(
      this.targets,
      o.targets) && java.util.Objects.equals(
      this.expressions,
      o.expressions) && java.util.Objects.equals(
      this.typeComment,
      o.typeComment) && java.util.Objects.equals(
      this.body,
      o.body) && java.util.Objects.equals(
      this.else_,
      o.else_);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(async) + 3 * java.util.Objects.hashCode(targets) + 5 * java.util.Objects.hashCode(expressions) + 7 * java.util.Objects.hashCode(typeComment) + 11 * java.util.Objects.hashCode(body) + 13 * java.util.Objects.hashCode(else_);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ForStatement other) {
    int cmp = 0;
    cmp = ((Comparable) async).compareTo(other.async);
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      targets.hashCode(),
      other.targets.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      expressions.hashCode(),
      other.expressions.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      typeComment.hashCode(),
      other.typeComment.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) body).compareTo(other.body);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      else_.hashCode(),
      other.else_.hashCode());
  }
  
  public ForStatement withAsync(Boolean async) {
    return new ForStatement(async, targets, expressions, typeComment, body, else_);
  }
  
  public ForStatement withTargets(java.util.List<hydra.ext.python.syntax.StarTarget> targets) {
    return new ForStatement(async, targets, expressions, typeComment, body, else_);
  }
  
  public ForStatement withExpressions(java.util.List<hydra.ext.python.syntax.StarExpression> expressions) {
    return new ForStatement(async, targets, expressions, typeComment, body, else_);
  }
  
  public ForStatement withTypeComment(hydra.util.Maybe<hydra.ext.python.syntax.TypeComment> typeComment) {
    return new ForStatement(async, targets, expressions, typeComment, body, else_);
  }
  
  public ForStatement withBody(hydra.ext.python.syntax.Block body) {
    return new ForStatement(async, targets, expressions, typeComment, body, else_);
  }
  
  public ForStatement withElse(hydra.util.Maybe<hydra.ext.python.syntax.Block> else_) {
    return new ForStatement(async, targets, expressions, typeComment, body, else_);
  }
}
