// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.features;

import java.io.Serializable;

/**
 * Features specific to a operations of a graph.
 */
public class GraphFeatures implements Serializable, Comparable<GraphFeatures> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.features.GraphFeatures");

  public static final hydra.core.Name SUPPORTS_COMPUTER = new hydra.core.Name("supportsComputer");

  public static final hydra.core.Name SUPPORTS_CONCURRENT_ACCESS = new hydra.core.Name("supportsConcurrentAccess");

  public static final hydra.core.Name SUPPORTS_IO_READ = new hydra.core.Name("supportsIoRead");

  public static final hydra.core.Name SUPPORTS_IO_WRITE = new hydra.core.Name("supportsIoWrite");

  public static final hydra.core.Name SUPPORTS_PERSISTENCE = new hydra.core.Name("supportsPersistence");

  public static final hydra.core.Name SUPPORTS_THREADED_TRANSACTIONS = new hydra.core.Name("supportsThreadedTransactions");

  public static final hydra.core.Name SUPPORTS_TRANSACTIONS = new hydra.core.Name("supportsTransactions");

  public static final hydra.core.Name VARIABLES = new hydra.core.Name("variables");

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
  public final hydra.ext.org.apache.tinkerpop.features.VariableFeatures variables;

  public GraphFeatures (Boolean supportsComputer, Boolean supportsConcurrentAccess, Boolean supportsIoRead, Boolean supportsIoWrite, Boolean supportsPersistence, Boolean supportsThreadedTransactions, Boolean supportsTransactions, hydra.ext.org.apache.tinkerpop.features.VariableFeatures variables) {
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
    GraphFeatures o = (GraphFeatures) other;
    return java.util.Objects.equals(
      this.supportsComputer,
      o.supportsComputer) && java.util.Objects.equals(
      this.supportsConcurrentAccess,
      o.supportsConcurrentAccess) && java.util.Objects.equals(
      this.supportsIoRead,
      o.supportsIoRead) && java.util.Objects.equals(
      this.supportsIoWrite,
      o.supportsIoWrite) && java.util.Objects.equals(
      this.supportsPersistence,
      o.supportsPersistence) && java.util.Objects.equals(
      this.supportsThreadedTransactions,
      o.supportsThreadedTransactions) && java.util.Objects.equals(
      this.supportsTransactions,
      o.supportsTransactions) && java.util.Objects.equals(
      this.variables,
      o.variables);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(supportsComputer) + 3 * java.util.Objects.hashCode(supportsConcurrentAccess) + 5 * java.util.Objects.hashCode(supportsIoRead) + 7 * java.util.Objects.hashCode(supportsIoWrite) + 11 * java.util.Objects.hashCode(supportsPersistence) + 13 * java.util.Objects.hashCode(supportsThreadedTransactions) + 17 * java.util.Objects.hashCode(supportsTransactions) + 19 * java.util.Objects.hashCode(variables);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(GraphFeatures other) {
    int cmp = 0;
    cmp = ((Comparable) supportsComputer).compareTo(other.supportsComputer);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) supportsConcurrentAccess).compareTo(other.supportsConcurrentAccess);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) supportsIoRead).compareTo(other.supportsIoRead);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) supportsIoWrite).compareTo(other.supportsIoWrite);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) supportsPersistence).compareTo(other.supportsPersistence);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) supportsThreadedTransactions).compareTo(other.supportsThreadedTransactions);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) supportsTransactions).compareTo(other.supportsTransactions);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) variables).compareTo(other.variables);
  }

  public GraphFeatures withSupportsComputer(Boolean supportsComputer) {
    return new GraphFeatures(supportsComputer, supportsConcurrentAccess, supportsIoRead, supportsIoWrite, supportsPersistence, supportsThreadedTransactions, supportsTransactions, variables);
  }

  public GraphFeatures withSupportsConcurrentAccess(Boolean supportsConcurrentAccess) {
    return new GraphFeatures(supportsComputer, supportsConcurrentAccess, supportsIoRead, supportsIoWrite, supportsPersistence, supportsThreadedTransactions, supportsTransactions, variables);
  }

  public GraphFeatures withSupportsIoRead(Boolean supportsIoRead) {
    return new GraphFeatures(supportsComputer, supportsConcurrentAccess, supportsIoRead, supportsIoWrite, supportsPersistence, supportsThreadedTransactions, supportsTransactions, variables);
  }

  public GraphFeatures withSupportsIoWrite(Boolean supportsIoWrite) {
    return new GraphFeatures(supportsComputer, supportsConcurrentAccess, supportsIoRead, supportsIoWrite, supportsPersistence, supportsThreadedTransactions, supportsTransactions, variables);
  }

  public GraphFeatures withSupportsPersistence(Boolean supportsPersistence) {
    return new GraphFeatures(supportsComputer, supportsConcurrentAccess, supportsIoRead, supportsIoWrite, supportsPersistence, supportsThreadedTransactions, supportsTransactions, variables);
  }

  public GraphFeatures withSupportsThreadedTransactions(Boolean supportsThreadedTransactions) {
    return new GraphFeatures(supportsComputer, supportsConcurrentAccess, supportsIoRead, supportsIoWrite, supportsPersistence, supportsThreadedTransactions, supportsTransactions, variables);
  }

  public GraphFeatures withSupportsTransactions(Boolean supportsTransactions) {
    return new GraphFeatures(supportsComputer, supportsConcurrentAccess, supportsIoRead, supportsIoWrite, supportsPersistence, supportsThreadedTransactions, supportsTransactions, variables);
  }

  public GraphFeatures withVariables(hydra.ext.org.apache.tinkerpop.features.VariableFeatures variables) {
    return new GraphFeatures(supportsComputer, supportsConcurrentAccess, supportsIoRead, supportsIoWrite, supportsPersistence, supportsThreadedTransactions, supportsTransactions, variables);
  }
}
