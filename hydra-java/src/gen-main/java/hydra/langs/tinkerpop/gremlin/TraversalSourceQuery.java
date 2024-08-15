// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public class TraversalSourceQuery implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.TraversalSourceQuery");
  
  public static final hydra.core.Name FIELD_NAME_SOURCE = new hydra.core.Name("source");
  
  public static final hydra.core.Name FIELD_NAME_TRANSACTION_PART = new hydra.core.Name("transactionPart");
  
  public final hydra.langs.tinkerpop.gremlin.TraversalSource source;
  
  public final hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TransactionPart> transactionPart;
  
  public TraversalSourceQuery (hydra.langs.tinkerpop.gremlin.TraversalSource source, hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TransactionPart> transactionPart) {
    java.util.Objects.requireNonNull((source));
    java.util.Objects.requireNonNull((transactionPart));
    this.source = source;
    this.transactionPart = transactionPart;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TraversalSourceQuery)) {
      return false;
    }
    TraversalSourceQuery o = (TraversalSourceQuery) (other);
    return source.equals(o.source) && transactionPart.equals(o.transactionPart);
  }
  
  @Override
  public int hashCode() {
    return 2 * source.hashCode() + 3 * transactionPart.hashCode();
  }
  
  public TraversalSourceQuery withSource(hydra.langs.tinkerpop.gremlin.TraversalSource source) {
    java.util.Objects.requireNonNull((source));
    return new TraversalSourceQuery(source, transactionPart);
  }
  
  public TraversalSourceQuery withTransactionPart(hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TransactionPart> transactionPart) {
    java.util.Objects.requireNonNull((transactionPart));
    return new TraversalSourceQuery(source, transactionPart);
  }
}