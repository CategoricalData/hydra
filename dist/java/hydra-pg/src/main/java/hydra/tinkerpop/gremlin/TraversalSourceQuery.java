// Note: this is an automatically generated file. Do not edit.

package hydra.tinkerpop.gremlin;

import java.io.Serializable;

public class TraversalSourceQuery implements Serializable, Comparable<TraversalSourceQuery> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.tinkerpop.gremlin.TraversalSourceQuery");

  public static final hydra.core.Name SOURCE = new hydra.core.Name("source");

  public static final hydra.core.Name TRANSACTION_PART = new hydra.core.Name("transactionPart");

  public final hydra.tinkerpop.gremlin.TraversalSource source;

  public final hydra.util.Maybe<hydra.tinkerpop.gremlin.TransactionPart> transactionPart;

  public TraversalSourceQuery (hydra.tinkerpop.gremlin.TraversalSource source, hydra.util.Maybe<hydra.tinkerpop.gremlin.TransactionPart> transactionPart) {
    this.source = source;
    this.transactionPart = transactionPart;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TraversalSourceQuery)) {
      return false;
    }
    TraversalSourceQuery o = (TraversalSourceQuery) other;
    return java.util.Objects.equals(
      this.source,
      o.source) && java.util.Objects.equals(
      this.transactionPart,
      o.transactionPart);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(source) + 3 * java.util.Objects.hashCode(transactionPart);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TraversalSourceQuery other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      source,
      other.source);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      transactionPart,
      other.transactionPart);
  }

  public TraversalSourceQuery withSource(hydra.tinkerpop.gremlin.TraversalSource source) {
    return new TraversalSourceQuery(source, transactionPart);
  }

  public TraversalSourceQuery withTransactionPart(hydra.util.Maybe<hydra.tinkerpop.gremlin.TransactionPart> transactionPart) {
    return new TraversalSourceQuery(source, transactionPart);
  }
}
