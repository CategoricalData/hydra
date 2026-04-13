// Note: this is an automatically generated file. Do not edit.

package hydra.python.syntax;

import java.io.Serializable;

public class ForStatement implements Serializable, Comparable<ForStatement> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.python.syntax.ForStatement");

  public static final hydra.core.Name ASYNC = new hydra.core.Name("async");

  public static final hydra.core.Name TARGETS = new hydra.core.Name("targets");

  public static final hydra.core.Name EXPRESSIONS = new hydra.core.Name("expressions");

  public static final hydra.core.Name TYPE_COMMENT = new hydra.core.Name("typeComment");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  public static final hydra.core.Name ELSE = new hydra.core.Name("else");

  public final Boolean async;

  public final java.util.List<hydra.python.syntax.StarTarget> targets;

  public final java.util.List<hydra.python.syntax.StarExpression> expressions;

  public final hydra.util.Maybe<hydra.python.syntax.TypeComment> typeComment;

  public final hydra.python.syntax.Block body;

  public final hydra.util.Maybe<hydra.python.syntax.Block> else_;

  public ForStatement (Boolean async, java.util.List<hydra.python.syntax.StarTarget> targets, java.util.List<hydra.python.syntax.StarExpression> expressions, hydra.util.Maybe<hydra.python.syntax.TypeComment> typeComment, hydra.python.syntax.Block body, hydra.util.Maybe<hydra.python.syntax.Block> else_) {
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
    cmp = hydra.util.Comparing.compare(
      async,
      other.async);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      targets,
      other.targets);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      expressions,
      other.expressions);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      typeComment,
      other.typeComment);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      body,
      other.body);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      else_,
      other.else_);
  }

  public ForStatement withAsync(Boolean async) {
    return new ForStatement(async, targets, expressions, typeComment, body, else_);
  }

  public ForStatement withTargets(java.util.List<hydra.python.syntax.StarTarget> targets) {
    return new ForStatement(async, targets, expressions, typeComment, body, else_);
  }

  public ForStatement withExpressions(java.util.List<hydra.python.syntax.StarExpression> expressions) {
    return new ForStatement(async, targets, expressions, typeComment, body, else_);
  }

  public ForStatement withTypeComment(hydra.util.Maybe<hydra.python.syntax.TypeComment> typeComment) {
    return new ForStatement(async, targets, expressions, typeComment, body, else_);
  }

  public ForStatement withBody(hydra.python.syntax.Block body) {
    return new ForStatement(async, targets, expressions, typeComment, body, else_);
  }

  public ForStatement withElse(hydra.util.Maybe<hydra.python.syntax.Block> else_) {
    return new ForStatement(async, targets, expressions, typeComment, body, else_);
  }
}
