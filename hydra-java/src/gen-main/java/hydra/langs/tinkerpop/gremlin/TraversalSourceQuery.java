// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public class TraversalSourceQuery implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.TraversalSourceQuery");
  
  public final hydra.langs.tinkerpop.gremlin.TraversalSource source;
  
  public final java.util.Optional<hydra.langs.tinkerpop.gremlin.TransactionPart> transactionPart;
  
  public TraversalSourceQuery (hydra.langs.tinkerpop.gremlin.TraversalSource source, java.util.Optional<hydra.langs.tinkerpop.gremlin.TransactionPart> transactionPart) {
    if (source == null) {
      throw new IllegalArgumentException("null value for 'source' argument");
    }
    if (transactionPart == null) {
      throw new IllegalArgumentException("null value for 'transactionPart' argument");
    }
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
    if (source == null) {
      throw new IllegalArgumentException("null value for 'source' argument");
    }
    return new TraversalSourceQuery(source, transactionPart);
  }
  
  public TraversalSourceQuery withTransactionPart(java.util.Optional<hydra.langs.tinkerpop.gremlin.TransactionPart> transactionPart) {
    if (transactionPart == null) {
      throw new IllegalArgumentException("null value for 'transactionPart' argument");
    }
    return new TraversalSourceQuery(source, transactionPart);
  }
}