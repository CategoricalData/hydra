// Note: this is an automatically generated file. Do not edit.

package hydra.python.syntax;

import java.io.Serializable;

public class AnnotatedStatement implements Serializable, Comparable<AnnotatedStatement> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.python.syntax.AnnotatedStatement");

  public static final hydra.core.Name COMMENT = new hydra.core.Name("comment");

  public static final hydra.core.Name STATEMENT = new hydra.core.Name("statement");

  public final String comment;

  public final hydra.python.syntax.Statement statement;

  public AnnotatedStatement (String comment, hydra.python.syntax.Statement statement) {
    this.comment = comment;
    this.statement = statement;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AnnotatedStatement)) {
      return false;
    }
    AnnotatedStatement o = (AnnotatedStatement) other;
    return java.util.Objects.equals(
      this.comment,
      o.comment) && java.util.Objects.equals(
      this.statement,
      o.statement);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(comment) + 3 * java.util.Objects.hashCode(statement);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(AnnotatedStatement other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      comment,
      other.comment);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      statement,
      other.statement);
  }

  public AnnotatedStatement withComment(String comment) {
    return new AnnotatedStatement(comment, statement);
  }

  public AnnotatedStatement withStatement(hydra.python.syntax.Statement statement) {
    return new AnnotatedStatement(comment, statement);
  }
}
