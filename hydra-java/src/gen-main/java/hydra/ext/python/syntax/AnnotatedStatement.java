// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class AnnotatedStatement implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.AnnotatedStatement");
  
  public static final hydra.core.Name FIELD_NAME_COMMENT = new hydra.core.Name("comment");
  
  public static final hydra.core.Name FIELD_NAME_STATEMENT = new hydra.core.Name("statement");
  
  public final String comment;
  
  public final hydra.ext.python.syntax.Statement statement;
  
  public AnnotatedStatement (String comment, hydra.ext.python.syntax.Statement statement) {
    java.util.Objects.requireNonNull((comment));
    java.util.Objects.requireNonNull((statement));
    this.comment = comment;
    this.statement = statement;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AnnotatedStatement)) {
      return false;
    }
    AnnotatedStatement o = (AnnotatedStatement) (other);
    return comment.equals(o.comment) && statement.equals(o.statement);
  }
  
  @Override
  public int hashCode() {
    return 2 * comment.hashCode() + 3 * statement.hashCode();
  }
  
  public AnnotatedStatement withComment(String comment) {
    java.util.Objects.requireNonNull((comment));
    return new AnnotatedStatement(comment, statement);
  }
  
  public AnnotatedStatement withStatement(hydra.ext.python.syntax.Statement statement) {
    java.util.Objects.requireNonNull((statement));
    return new AnnotatedStatement(comment, statement);
  }
}