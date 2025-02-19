// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class QueryBody implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.QueryBody");
  
  public static final hydra.core.Name FIELD_NAME_CLAUSES = new hydra.core.Name("clauses");
  
  public static final hydra.core.Name FIELD_NAME_SELECT_OR_GROUP = new hydra.core.Name("selectOrGroup");
  
  public static final hydra.core.Name FIELD_NAME_CONTINUATION = new hydra.core.Name("continuation");
  
  public final java.util.List<hydra.ext.csharp.syntax.QueryBodyClause> clauses;
  
  public final hydra.ext.csharp.syntax.SelectOrGroupClause selectOrGroup;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.QueryContinuation> continuation;
  
  public QueryBody (java.util.List<hydra.ext.csharp.syntax.QueryBodyClause> clauses, hydra.ext.csharp.syntax.SelectOrGroupClause selectOrGroup, hydra.util.Opt<hydra.ext.csharp.syntax.QueryContinuation> continuation) {
    java.util.Objects.requireNonNull((clauses));
    java.util.Objects.requireNonNull((selectOrGroup));
    java.util.Objects.requireNonNull((continuation));
    this.clauses = clauses;
    this.selectOrGroup = selectOrGroup;
    this.continuation = continuation;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof QueryBody)) {
      return false;
    }
    QueryBody o = (QueryBody) (other);
    return clauses.equals(o.clauses) && selectOrGroup.equals(o.selectOrGroup) && continuation.equals(o.continuation);
  }
  
  @Override
  public int hashCode() {
    return 2 * clauses.hashCode() + 3 * selectOrGroup.hashCode() + 5 * continuation.hashCode();
  }
  
  public QueryBody withClauses(java.util.List<hydra.ext.csharp.syntax.QueryBodyClause> clauses) {
    java.util.Objects.requireNonNull((clauses));
    return new QueryBody(clauses, selectOrGroup, continuation);
  }
  
  public QueryBody withSelectOrGroup(hydra.ext.csharp.syntax.SelectOrGroupClause selectOrGroup) {
    java.util.Objects.requireNonNull((selectOrGroup));
    return new QueryBody(clauses, selectOrGroup, continuation);
  }
  
  public QueryBody withContinuation(hydra.util.Opt<hydra.ext.csharp.syntax.QueryContinuation> continuation) {
    java.util.Objects.requireNonNull((continuation));
    return new QueryBody(clauses, selectOrGroup, continuation);
  }
}