// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class QueryContinuation implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.QueryContinuation");
  
  public static final hydra.core.Name FIELD_NAME_INTO = new hydra.core.Name("into");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final hydra.ext.csharp.syntax.Identifier into;
  
  public final hydra.ext.csharp.syntax.QueryBody body;
  
  public QueryContinuation (hydra.ext.csharp.syntax.Identifier into, hydra.ext.csharp.syntax.QueryBody body) {
    java.util.Objects.requireNonNull((into));
    java.util.Objects.requireNonNull((body));
    this.into = into;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof QueryContinuation)) {
      return false;
    }
    QueryContinuation o = (QueryContinuation) (other);
    return into.equals(o.into) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * into.hashCode() + 3 * body.hashCode();
  }
  
  public QueryContinuation withInto(hydra.ext.csharp.syntax.Identifier into) {
    java.util.Objects.requireNonNull((into));
    return new QueryContinuation(into, body);
  }
  
  public QueryContinuation withBody(hydra.ext.csharp.syntax.QueryBody body) {
    java.util.Objects.requireNonNull((body));
    return new QueryContinuation(into, body);
  }
}