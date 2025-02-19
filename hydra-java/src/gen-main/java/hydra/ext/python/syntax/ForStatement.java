// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class ForStatement implements Serializable {
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
  
  public final hydra.util.Opt<hydra.ext.python.syntax.TypeComment> typeComment;
  
  public final hydra.ext.python.syntax.Block body;
  
  public final hydra.util.Opt<hydra.ext.python.syntax.Block> else_;
  
  public ForStatement (Boolean async, java.util.List<hydra.ext.python.syntax.StarTarget> targets, java.util.List<hydra.ext.python.syntax.StarExpression> expressions, hydra.util.Opt<hydra.ext.python.syntax.TypeComment> typeComment, hydra.ext.python.syntax.Block body, hydra.util.Opt<hydra.ext.python.syntax.Block> else_) {
    java.util.Objects.requireNonNull((async));
    java.util.Objects.requireNonNull((targets));
    java.util.Objects.requireNonNull((expressions));
    java.util.Objects.requireNonNull((typeComment));
    java.util.Objects.requireNonNull((body));
    java.util.Objects.requireNonNull((else_));
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
    ForStatement o = (ForStatement) (other);
    return async.equals(o.async) && targets.equals(o.targets) && expressions.equals(o.expressions) && typeComment.equals(o.typeComment) && body.equals(o.body) && else_.equals(o.else_);
  }
  
  @Override
  public int hashCode() {
    return 2 * async.hashCode() + 3 * targets.hashCode() + 5 * expressions.hashCode() + 7 * typeComment.hashCode() + 11 * body.hashCode() + 13 * else_.hashCode();
  }
  
  public ForStatement withAsync(Boolean async) {
    java.util.Objects.requireNonNull((async));
    return new ForStatement(async, targets, expressions, typeComment, body, else_);
  }
  
  public ForStatement withTargets(java.util.List<hydra.ext.python.syntax.StarTarget> targets) {
    java.util.Objects.requireNonNull((targets));
    return new ForStatement(async, targets, expressions, typeComment, body, else_);
  }
  
  public ForStatement withExpressions(java.util.List<hydra.ext.python.syntax.StarExpression> expressions) {
    java.util.Objects.requireNonNull((expressions));
    return new ForStatement(async, targets, expressions, typeComment, body, else_);
  }
  
  public ForStatement withTypeComment(hydra.util.Opt<hydra.ext.python.syntax.TypeComment> typeComment) {
    java.util.Objects.requireNonNull((typeComment));
    return new ForStatement(async, targets, expressions, typeComment, body, else_);
  }
  
  public ForStatement withBody(hydra.ext.python.syntax.Block body) {
    java.util.Objects.requireNonNull((body));
    return new ForStatement(async, targets, expressions, typeComment, body, else_);
  }
  
  public ForStatement withElse(hydra.util.Opt<hydra.ext.python.syntax.Block> else_) {
    java.util.Objects.requireNonNull((else_));
    return new ForStatement(async, targets, expressions, typeComment, body, else_);
  }
}