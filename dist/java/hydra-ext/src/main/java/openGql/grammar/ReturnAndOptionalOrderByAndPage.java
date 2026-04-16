// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class ReturnAndOptionalOrderByAndPage implements Serializable, Comparable<ReturnAndOptionalOrderByAndPage> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.ReturnAndOptionalOrderByAndPage");

  public static final hydra.core.Name RETURN = new hydra.core.Name("return");

  public static final hydra.core.Name ORDER_BY_AND_PAGE = new hydra.core.Name("orderByAndPage");

  public final openGql.grammar.ReturnStatementBody return_;

  public final hydra.util.Maybe<openGql.grammar.OrderByAndPageStatement> orderByAndPage;

  public ReturnAndOptionalOrderByAndPage (openGql.grammar.ReturnStatementBody return_, hydra.util.Maybe<openGql.grammar.OrderByAndPageStatement> orderByAndPage) {
    this.return_ = return_;
    this.orderByAndPage = orderByAndPage;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ReturnAndOptionalOrderByAndPage)) {
      return false;
    }
    ReturnAndOptionalOrderByAndPage o = (ReturnAndOptionalOrderByAndPage) other;
    return java.util.Objects.equals(
      this.return_,
      o.return_) && java.util.Objects.equals(
      this.orderByAndPage,
      o.orderByAndPage);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(return_) + 3 * java.util.Objects.hashCode(orderByAndPage);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ReturnAndOptionalOrderByAndPage other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      return_,
      other.return_);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      orderByAndPage,
      other.orderByAndPage);
  }

  public ReturnAndOptionalOrderByAndPage withReturn(openGql.grammar.ReturnStatementBody return_) {
    return new ReturnAndOptionalOrderByAndPage(return_, orderByAndPage);
  }

  public ReturnAndOptionalOrderByAndPage withOrderByAndPage(hydra.util.Maybe<openGql.grammar.OrderByAndPageStatement> orderByAndPage) {
    return new ReturnAndOptionalOrderByAndPage(return_, orderByAndPage);
  }
}
