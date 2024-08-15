// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.features;

import java.io.Serializable;

/**
 * Features specific to a operations of a graph.
 */
public class GraphFeatures implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/tinkerpop/features.GraphFeatures");
  
  public static final hydra.core.Name FIELD_NAME_SUPPORTS_COMPUTER = new hydra.core.Name("supportsComputer");
  
  public static final hydra.core.Name FIELD_NAME_SUPPORTS_CONCURRENT_ACCESS = new hydra.core.Name("supportsConcurrentAccess");
  
  public static final hydra.core.Name FIELD_NAME_SUPPORTS_IO_READ = new hydra.core.Name("supportsIoRead");
  
  public static final hydra.core.Name FIELD_NAME_SUPPORTS_IO_WRITE = new hydra.core.Name("supportsIoWrite");
  
  public static final hydra.core.Name FIELD_NAME_SUPPORTS_PERSISTENCE = new hydra.core.Name("supportsPersistence");
  
  public static final hydra.core.Name FIELD_NAME_SUPPORTS_THREADED_TRANSACTIONS = new hydra.core.Name("supportsThreadedTransactions");
  
  public static final hydra.core.Name FIELD_NAME_SUPPORTS_TRANSACTIONS = new hydra.core.Name("supportsTransactions");
  
  public static final hydra.core.Name FIELD_NAME_VARIABLES = new hydra.core.Name("variables");
  
  /**
   * Determines if the Graph implementation supports GraphComputer based processing.
   */
  public final Boolean supportsComputer;
  
  /**
   * Determines if the Graph implementation supports more than one connection to the same instance at the same time.
   */
  public final Boolean supportsConcurrentAccess;
  
  /**
   * Determines if the Graph implementations supports read operations as executed with the GraphTraversalSource.io(String) step.
   */
  public final Boolean supportsIoRead;
  
  /**
   * Determines if the Graph implementations supports write operations as executed with the GraphTraversalSource.io(String) step.
   */
  public final Boolean supportsIoWrite;
  
  /**
   * Determines if the Graph implementation supports persisting it's contents natively to disk.
   */
  public final Boolean supportsPersistence;
  
  /**
   * Determines if the Graph implementation supports threaded transactions which allow a transaction to be executed across multiple threads via Transaction.createThreadedTx().
   */
  public final Boolean supportsThreadedTransactions;
  
  /**
   * Determines if the Graph implementations supports transactions.
   */
  public final Boolean supportsTransactions;
  
  /**
   * Gets the features related to graph sideEffects operation.
   */
  public final hydra.langs.tinkerpop.features.VariableFeatures variables;
  
  public GraphFeatures (Boolean supportsComputer, Boolean supportsConcurrentAccess, Boolean supportsIoRead, Boolean supportsIoWrite, Boolean supportsPersistence, Boolean supportsThreadedTransactions, Boolean supportsTransactions, hydra.langs.tinkerpop.features.VariableFeatures variables) {
    java.util.Objects.requireNonNull((supportsComputer));
    java.util.Objects.requireNonNull((supportsConcurrentAccess));
    java.util.Objects.requireNonNull((supportsIoRead));
    java.util.Objects.requireNonNull((supportsIoWrite));
    java.util.Objects.requireNonNull((supportsPersistence));
    java.util.Objects.requireNonNull((supportsThreadedTransactions));
    java.util.Objects.requireNonNull((supportsTransactions));
    java.util.Objects.requireNonNull((variables));
    this.supportsComputer = supportsComputer;
    this.supportsConcurrentAccess = supportsConcurrentAccess;
    this.supportsIoRead = supportsIoRead;
    this.supportsIoWrite = supportsIoWrite;
    this.supportsPersistence = supportsPersistence;
    this.supportsThreadedTransactions = supportsThreadedTransactions;
    this.supportsTransactions = supportsTransactions;
    this.variables = variables;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof GraphFeatures)) {
      return false;
    }
    GraphFeatures o = (GraphFeatures) (other);
    return supportsComputer.equals(o.supportsComputer) && supportsConcurrentAccess.equals(o.supportsConcurrentAccess) && supportsIoRead.equals(o.supportsIoRead) && supportsIoWrite.equals(o.supportsIoWrite) && supportsPersistence.equals(o.supportsPersistence) && supportsThreadedTransactions.equals(o.supportsThreadedTransactions) && supportsTransactions.equals(o.supportsTransactions) && variables.equals(o.variables);
  }
  
  @Override
  public int hashCode() {
    return 2 * supportsComputer.hashCode() + 3 * supportsConcurrentAccess.hashCode() + 5 * supportsIoRead.hashCode() + 7 * supportsIoWrite.hashCode() + 11 * supportsPersistence.hashCode() + 13 * supportsThreadedTransactions.hashCode() + 17 * supportsTransactions.hashCode() + 19 * variables.hashCode();
  }
  
  public GraphFeatures withSupportsComputer(Boolean supportsComputer) {
    java.util.Objects.requireNonNull((supportsComputer));
    return new GraphFeatures(supportsComputer, supportsConcurrentAccess, supportsIoRead, supportsIoWrite, supportsPersistence, supportsThreadedTransactions, supportsTransactions, variables);
  }
  
  public GraphFeatures withSupportsConcurrentAccess(Boolean supportsConcurrentAccess) {
    java.util.Objects.requireNonNull((supportsConcurrentAccess));
    return new GraphFeatures(supportsComputer, supportsConcurrentAccess, supportsIoRead, supportsIoWrite, supportsPersistence, supportsThreadedTransactions, supportsTransactions, variables);
  }
  
  public GraphFeatures withSupportsIoRead(Boolean supportsIoRead) {
    java.util.Objects.requireNonNull((supportsIoRead));
    return new GraphFeatures(supportsComputer, supportsConcurrentAccess, supportsIoRead, supportsIoWrite, supportsPersistence, supportsThreadedTransactions, supportsTransactions, variables);
  }
  
  public GraphFeatures withSupportsIoWrite(Boolean supportsIoWrite) {
    java.util.Objects.requireNonNull((supportsIoWrite));
    return new GraphFeatures(supportsComputer, supportsConcurrentAccess, supportsIoRead, supportsIoWrite, supportsPersistence, supportsThreadedTransactions, supportsTransactions, variables);
  }
  
  public GraphFeatures withSupportsPersistence(Boolean supportsPersistence) {
    java.util.Objects.requireNonNull((supportsPersistence));
    return new GraphFeatures(supportsComputer, supportsConcurrentAccess, supportsIoRead, supportsIoWrite, supportsPersistence, supportsThreadedTransactions, supportsTransactions, variables);
  }
  
  public GraphFeatures withSupportsThreadedTransactions(Boolean supportsThreadedTransactions) {
    java.util.Objects.requireNonNull((supportsThreadedTransactions));
    return new GraphFeatures(supportsComputer, supportsConcurrentAccess, supportsIoRead, supportsIoWrite, supportsPersistence, supportsThreadedTransactions, supportsTransactions, variables);
  }
  
  public GraphFeatures withSupportsTransactions(Boolean supportsTransactions) {
    java.util.Objects.requireNonNull((supportsTransactions));
    return new GraphFeatures(supportsComputer, supportsConcurrentAccess, supportsIoRead, supportsIoWrite, supportsPersistence, supportsThreadedTransactions, supportsTransactions, variables);
  }
  
  public GraphFeatures withVariables(hydra.langs.tinkerpop.features.VariableFeatures variables) {
    java.util.Objects.requireNonNull((variables));
    return new GraphFeatures(supportsComputer, supportsConcurrentAccess, supportsIoRead, supportsIoWrite, supportsPersistence, supportsThreadedTransactions, supportsTransactions, variables);
  }
}