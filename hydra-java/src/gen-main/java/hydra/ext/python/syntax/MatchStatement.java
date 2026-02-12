// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class MatchStatement implements Serializable, Comparable<MatchStatement> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.MatchStatement");
  
  public static final hydra.core.Name FIELD_NAME_SUBJECT = new hydra.core.Name("subject");
  
  public static final hydra.core.Name FIELD_NAME_CASES = new hydra.core.Name("cases");
  
  public final hydra.ext.python.syntax.SubjectExpression subject;
  
  public final java.util.List<hydra.ext.python.syntax.CaseBlock> cases;
  
  public MatchStatement (hydra.ext.python.syntax.SubjectExpression subject, java.util.List<hydra.ext.python.syntax.CaseBlock> cases) {
    this.subject = subject;
    this.cases = cases;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MatchStatement)) {
      return false;
    }
    MatchStatement o = (MatchStatement) other;
    return java.util.Objects.equals(
      this.subject,
      o.subject) && java.util.Objects.equals(
      this.cases,
      o.cases);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(subject) + 3 * java.util.Objects.hashCode(cases);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(MatchStatement other) {
    int cmp = 0;
    cmp = ((Comparable) subject).compareTo(other.subject);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      cases.hashCode(),
      other.cases.hashCode());
  }
  
  public MatchStatement withSubject(hydra.ext.python.syntax.SubjectExpression subject) {
    return new MatchStatement(subject, cases);
  }
  
  public MatchStatement withCases(java.util.List<hydra.ext.python.syntax.CaseBlock> cases) {
    return new MatchStatement(subject, cases);
  }
}
