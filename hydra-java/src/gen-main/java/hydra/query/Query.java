// Note: this is an automatically generated file. Do not edit.

package hydra.query;

import java.io.Serializable;

/**
 * A SELECT-style graph pattern matching query
 */
public class Query implements Serializable, Comparable<Query> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.query.Query");
  
  public static final hydra.core.Name FIELD_NAME_VARIABLES = new hydra.core.Name("variables");
  
  public static final hydra.core.Name FIELD_NAME_PATTERNS = new hydra.core.Name("patterns");
  
  /**
   * The variables selected by the query
   */
  public final java.util.List<hydra.query.Variable> variables;
  
  /**
   * The patterns to be matched
   */
  public final java.util.List<hydra.query.Pattern> patterns;
  
  public Query (java.util.List<hydra.query.Variable> variables, java.util.List<hydra.query.Pattern> patterns) {
    this.variables = variables;
    this.patterns = patterns;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Query)) {
      return false;
    }
    Query o = (Query) other;
    return java.util.Objects.equals(
      this.variables,
      o.variables) && java.util.Objects.equals(
      this.patterns,
      o.patterns);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(variables) + 3 * java.util.Objects.hashCode(patterns);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Query other) {
    int cmp = 0;
    cmp = Integer.compare(
      variables.hashCode(),
      other.variables.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      patterns.hashCode(),
      other.patterns.hashCode());
  }
  
  public Query withVariables(java.util.List<hydra.query.Variable> variables) {
    return new Query(variables, patterns);
  }
  
  public Query withPatterns(java.util.List<hydra.query.Pattern> patterns) {
    return new Query(variables, patterns);
  }
}
