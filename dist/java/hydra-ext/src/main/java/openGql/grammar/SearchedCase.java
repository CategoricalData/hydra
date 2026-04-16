// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class SearchedCase implements Serializable, Comparable<SearchedCase> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.SearchedCase");

  public static final hydra.core.Name WHEN_CLAUSES = new hydra.core.Name("whenClauses");

  public static final hydra.core.Name ELSE_CLAUSE = new hydra.core.Name("elseClause");

  public final java.util.List<openGql.grammar.SearchedWhenClause> whenClauses;

  public final hydra.util.Maybe<openGql.grammar.Result> elseClause;

  public SearchedCase (java.util.List<openGql.grammar.SearchedWhenClause> whenClauses, hydra.util.Maybe<openGql.grammar.Result> elseClause) {
    this.whenClauses = whenClauses;
    this.elseClause = elseClause;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SearchedCase)) {
      return false;
    }
    SearchedCase o = (SearchedCase) other;
    return java.util.Objects.equals(
      this.whenClauses,
      o.whenClauses) && java.util.Objects.equals(
      this.elseClause,
      o.elseClause);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(whenClauses) + 3 * java.util.Objects.hashCode(elseClause);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SearchedCase other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      whenClauses,
      other.whenClauses);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      elseClause,
      other.elseClause);
  }

  public SearchedCase withWhenClauses(java.util.List<openGql.grammar.SearchedWhenClause> whenClauses) {
    return new SearchedCase(whenClauses, elseClause);
  }

  public SearchedCase withElseClause(hydra.util.Maybe<openGql.grammar.Result> elseClause) {
    return new SearchedCase(whenClauses, elseClause);
  }
}
