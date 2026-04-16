// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class SearchedWhenClause implements Serializable, Comparable<SearchedWhenClause> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.SearchedWhenClause");

  public static final hydra.core.Name SEARCH_CONDITION = new hydra.core.Name("searchCondition");

  public static final hydra.core.Name RESULT = new hydra.core.Name("result");

  public final openGql.grammar.ValueExpression searchCondition;

  public final openGql.grammar.Result result;

  public SearchedWhenClause (openGql.grammar.ValueExpression searchCondition, openGql.grammar.Result result) {
    this.searchCondition = searchCondition;
    this.result = result;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SearchedWhenClause)) {
      return false;
    }
    SearchedWhenClause o = (SearchedWhenClause) other;
    return java.util.Objects.equals(
      this.searchCondition,
      o.searchCondition) && java.util.Objects.equals(
      this.result,
      o.result);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(searchCondition) + 3 * java.util.Objects.hashCode(result);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SearchedWhenClause other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      searchCondition,
      other.searchCondition);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      result,
      other.result);
  }

  public SearchedWhenClause withSearchCondition(openGql.grammar.ValueExpression searchCondition) {
    return new SearchedWhenClause(searchCondition, result);
  }

  public SearchedWhenClause withResult(openGql.grammar.Result result) {
    return new SearchedWhenClause(searchCondition, result);
  }
}
