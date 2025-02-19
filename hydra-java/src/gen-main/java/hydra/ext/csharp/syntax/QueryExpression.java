// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class QueryExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.QueryExpression");
  
  public static final hydra.core.Name FIELD_NAME_FROM = new hydra.core.Name("from");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final hydra.ext.csharp.syntax.FromClause from;
  
  public final hydra.ext.csharp.syntax.QueryBody body;
  
  public QueryExpression (hydra.ext.csharp.syntax.FromClause from, hydra.ext.csharp.syntax.QueryBody body) {
    java.util.Objects.requireNonNull((from));
    java.util.Objects.requireNonNull((body));
    this.from = from;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof QueryExpression)) {
      return false;
    }
    QueryExpression o = (QueryExpression) (other);
    return from.equals(o.from) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * from.hashCode() + 3 * body.hashCode();
  }
  
  public QueryExpression withFrom(hydra.ext.csharp.syntax.FromClause from) {
    java.util.Objects.requireNonNull((from));
    return new QueryExpression(from, body);
  }
  
  public QueryExpression withBody(hydra.ext.csharp.syntax.QueryBody body) {
    java.util.Objects.requireNonNull((body));
    return new QueryExpression(from, body);
  }
}