// Note: this is an automatically generated file. Do not edit.

package hydra.cypher.features;

import java.io.Serializable;

/**
 * Standard Cypher functions
 */
public class FunctionFeatures implements Serializable, Comparable<FunctionFeatures> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.cypher.features.FunctionFeatures");

  public static final hydra.core.Name AGGREGATE_FUNCTION = new hydra.core.Name("aggregateFunction");

  public static final hydra.core.Name DATABASE_FUNCTION = new hydra.core.Name("databaseFunction");

  public static final hydra.core.Name GEN_A_I_FUNCTION = new hydra.core.Name("genAIFunction");

  public static final hydra.core.Name GRAPH_FUNCTION = new hydra.core.Name("graphFunction");

  public static final hydra.core.Name LIST_FUNCTION = new hydra.core.Name("listFunction");

  public static final hydra.core.Name LOAD_C_S_V_FUNCTION = new hydra.core.Name("loadCSVFunction");

  public static final hydra.core.Name LOGARITHMIC_FUNCTION = new hydra.core.Name("logarithmicFunction");

  public static final hydra.core.Name NUMERIC_FUNCTION = new hydra.core.Name("numericFunction");

  public static final hydra.core.Name PREDICATE_FUNCTION = new hydra.core.Name("predicateFunction");

  public static final hydra.core.Name SCALAR_FUNCTION = new hydra.core.Name("scalarFunction");

  public static final hydra.core.Name SPATIAL_FUNCTION = new hydra.core.Name("spatialFunction");

  public static final hydra.core.Name STRING_FUNCTION = new hydra.core.Name("stringFunction");

  public static final hydra.core.Name TEMPORAL_DURATION_FUNCTION = new hydra.core.Name("temporalDurationFunction");

  public static final hydra.core.Name TEMPORAL_INSTANT_FUNCTION = new hydra.core.Name("temporalInstantFunction");

  public static final hydra.core.Name TRIGONOMETRIC_FUNCTION = new hydra.core.Name("trigonometricFunction");

  public static final hydra.core.Name VECTOR_FUNCTION = new hydra.core.Name("vectorFunction");

  /**
   * Aggregate functions
   */
  public final hydra.cypher.features.AggregateFunctionFeatures aggregateFunction;

  /**
   * Database functions
   */
  public final hydra.cypher.features.DatabaseFunctionFeatures databaseFunction;

  /**
   * GenAI functions
   */
  public final hydra.cypher.features.GenAIFunctionFeatures genAIFunction;

  /**
   * Graph functions
   */
  public final hydra.cypher.features.GraphFunctionFeatures graphFunction;

  /**
   * List functions
   */
  public final hydra.cypher.features.ListFunctionFeatures listFunction;

  /**
   * Load CSV functions
   */
  public final hydra.cypher.features.LoadCSVFunctionFeatures loadCSVFunction;

  /**
   * Logarithmic functions
   */
  public final hydra.cypher.features.LogarithmicFunctionFeatures logarithmicFunction;

  /**
   * Numeric functions
   */
  public final hydra.cypher.features.NumericFunctionFeatures numericFunction;

  /**
   * Predicate functions
   */
  public final hydra.cypher.features.PredicateFunctionFeatures predicateFunction;

  /**
   * Scalar functions
   */
  public final hydra.cypher.features.ScalarFunctionFeatures scalarFunction;

  /**
   * Spatial functions
   */
  public final hydra.cypher.features.SpatialFunctionFeatures spatialFunction;

  /**
   * String functions
   */
  public final hydra.cypher.features.StringFunctionFeatures stringFunction;

  /**
   * Temporal duration functions
   */
  public final hydra.cypher.features.TemporalDurationFunctionFeatures temporalDurationFunction;

  /**
   * Temporal instant functions
   */
  public final hydra.cypher.features.TemporalInstantFunctionFeatures temporalInstantFunction;

  /**
   * Trigonometric functions
   */
  public final hydra.cypher.features.TrigonometricFunctionFeatures trigonometricFunction;

  /**
   * Vector functions
   */
  public final hydra.cypher.features.VectorFunctionFeatures vectorFunction;

  public FunctionFeatures (hydra.cypher.features.AggregateFunctionFeatures aggregateFunction, hydra.cypher.features.DatabaseFunctionFeatures databaseFunction, hydra.cypher.features.GenAIFunctionFeatures genAIFunction, hydra.cypher.features.GraphFunctionFeatures graphFunction, hydra.cypher.features.ListFunctionFeatures listFunction, hydra.cypher.features.LoadCSVFunctionFeatures loadCSVFunction, hydra.cypher.features.LogarithmicFunctionFeatures logarithmicFunction, hydra.cypher.features.NumericFunctionFeatures numericFunction, hydra.cypher.features.PredicateFunctionFeatures predicateFunction, hydra.cypher.features.ScalarFunctionFeatures scalarFunction, hydra.cypher.features.SpatialFunctionFeatures spatialFunction, hydra.cypher.features.StringFunctionFeatures stringFunction, hydra.cypher.features.TemporalDurationFunctionFeatures temporalDurationFunction, hydra.cypher.features.TemporalInstantFunctionFeatures temporalInstantFunction, hydra.cypher.features.TrigonometricFunctionFeatures trigonometricFunction, hydra.cypher.features.VectorFunctionFeatures vectorFunction) {
    this.aggregateFunction = aggregateFunction;
    this.databaseFunction = databaseFunction;
    this.genAIFunction = genAIFunction;
    this.graphFunction = graphFunction;
    this.listFunction = listFunction;
    this.loadCSVFunction = loadCSVFunction;
    this.logarithmicFunction = logarithmicFunction;
    this.numericFunction = numericFunction;
    this.predicateFunction = predicateFunction;
    this.scalarFunction = scalarFunction;
    this.spatialFunction = spatialFunction;
    this.stringFunction = stringFunction;
    this.temporalDurationFunction = temporalDurationFunction;
    this.temporalInstantFunction = temporalInstantFunction;
    this.trigonometricFunction = trigonometricFunction;
    this.vectorFunction = vectorFunction;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FunctionFeatures)) {
      return false;
    }
    FunctionFeatures o = (FunctionFeatures) other;
    return java.util.Objects.equals(
      this.aggregateFunction,
      o.aggregateFunction) && java.util.Objects.equals(
      this.databaseFunction,
      o.databaseFunction) && java.util.Objects.equals(
      this.genAIFunction,
      o.genAIFunction) && java.util.Objects.equals(
      this.graphFunction,
      o.graphFunction) && java.util.Objects.equals(
      this.listFunction,
      o.listFunction) && java.util.Objects.equals(
      this.loadCSVFunction,
      o.loadCSVFunction) && java.util.Objects.equals(
      this.logarithmicFunction,
      o.logarithmicFunction) && java.util.Objects.equals(
      this.numericFunction,
      o.numericFunction) && java.util.Objects.equals(
      this.predicateFunction,
      o.predicateFunction) && java.util.Objects.equals(
      this.scalarFunction,
      o.scalarFunction) && java.util.Objects.equals(
      this.spatialFunction,
      o.spatialFunction) && java.util.Objects.equals(
      this.stringFunction,
      o.stringFunction) && java.util.Objects.equals(
      this.temporalDurationFunction,
      o.temporalDurationFunction) && java.util.Objects.equals(
      this.temporalInstantFunction,
      o.temporalInstantFunction) && java.util.Objects.equals(
      this.trigonometricFunction,
      o.trigonometricFunction) && java.util.Objects.equals(
      this.vectorFunction,
      o.vectorFunction);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(aggregateFunction) + 3 * java.util.Objects.hashCode(databaseFunction) + 5 * java.util.Objects.hashCode(genAIFunction) + 7 * java.util.Objects.hashCode(graphFunction) + 11 * java.util.Objects.hashCode(listFunction) + 13 * java.util.Objects.hashCode(loadCSVFunction) + 17 * java.util.Objects.hashCode(logarithmicFunction) + 19 * java.util.Objects.hashCode(numericFunction) + 23 * java.util.Objects.hashCode(predicateFunction) + 29 * java.util.Objects.hashCode(scalarFunction) + 31 * java.util.Objects.hashCode(spatialFunction) + 37 * java.util.Objects.hashCode(stringFunction) + 41 * java.util.Objects.hashCode(temporalDurationFunction) + 43 * java.util.Objects.hashCode(temporalInstantFunction) + 47 * java.util.Objects.hashCode(trigonometricFunction) + 53 * java.util.Objects.hashCode(vectorFunction);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(FunctionFeatures other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      aggregateFunction,
      other.aggregateFunction);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      databaseFunction,
      other.databaseFunction);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      genAIFunction,
      other.genAIFunction);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      graphFunction,
      other.graphFunction);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      listFunction,
      other.listFunction);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      loadCSVFunction,
      other.loadCSVFunction);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      logarithmicFunction,
      other.logarithmicFunction);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      numericFunction,
      other.numericFunction);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      predicateFunction,
      other.predicateFunction);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      scalarFunction,
      other.scalarFunction);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      spatialFunction,
      other.spatialFunction);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      stringFunction,
      other.stringFunction);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      temporalDurationFunction,
      other.temporalDurationFunction);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      temporalInstantFunction,
      other.temporalInstantFunction);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      trigonometricFunction,
      other.trigonometricFunction);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      vectorFunction,
      other.vectorFunction);
  }

  public FunctionFeatures withAggregateFunction(hydra.cypher.features.AggregateFunctionFeatures aggregateFunction) {
    return new FunctionFeatures(aggregateFunction, databaseFunction, genAIFunction, graphFunction, listFunction, loadCSVFunction, logarithmicFunction, numericFunction, predicateFunction, scalarFunction, spatialFunction, stringFunction, temporalDurationFunction, temporalInstantFunction, trigonometricFunction, vectorFunction);
  }

  public FunctionFeatures withDatabaseFunction(hydra.cypher.features.DatabaseFunctionFeatures databaseFunction) {
    return new FunctionFeatures(aggregateFunction, databaseFunction, genAIFunction, graphFunction, listFunction, loadCSVFunction, logarithmicFunction, numericFunction, predicateFunction, scalarFunction, spatialFunction, stringFunction, temporalDurationFunction, temporalInstantFunction, trigonometricFunction, vectorFunction);
  }

  public FunctionFeatures withGenAIFunction(hydra.cypher.features.GenAIFunctionFeatures genAIFunction) {
    return new FunctionFeatures(aggregateFunction, databaseFunction, genAIFunction, graphFunction, listFunction, loadCSVFunction, logarithmicFunction, numericFunction, predicateFunction, scalarFunction, spatialFunction, stringFunction, temporalDurationFunction, temporalInstantFunction, trigonometricFunction, vectorFunction);
  }

  public FunctionFeatures withGraphFunction(hydra.cypher.features.GraphFunctionFeatures graphFunction) {
    return new FunctionFeatures(aggregateFunction, databaseFunction, genAIFunction, graphFunction, listFunction, loadCSVFunction, logarithmicFunction, numericFunction, predicateFunction, scalarFunction, spatialFunction, stringFunction, temporalDurationFunction, temporalInstantFunction, trigonometricFunction, vectorFunction);
  }

  public FunctionFeatures withListFunction(hydra.cypher.features.ListFunctionFeatures listFunction) {
    return new FunctionFeatures(aggregateFunction, databaseFunction, genAIFunction, graphFunction, listFunction, loadCSVFunction, logarithmicFunction, numericFunction, predicateFunction, scalarFunction, spatialFunction, stringFunction, temporalDurationFunction, temporalInstantFunction, trigonometricFunction, vectorFunction);
  }

  public FunctionFeatures withLoadCSVFunction(hydra.cypher.features.LoadCSVFunctionFeatures loadCSVFunction) {
    return new FunctionFeatures(aggregateFunction, databaseFunction, genAIFunction, graphFunction, listFunction, loadCSVFunction, logarithmicFunction, numericFunction, predicateFunction, scalarFunction, spatialFunction, stringFunction, temporalDurationFunction, temporalInstantFunction, trigonometricFunction, vectorFunction);
  }

  public FunctionFeatures withLogarithmicFunction(hydra.cypher.features.LogarithmicFunctionFeatures logarithmicFunction) {
    return new FunctionFeatures(aggregateFunction, databaseFunction, genAIFunction, graphFunction, listFunction, loadCSVFunction, logarithmicFunction, numericFunction, predicateFunction, scalarFunction, spatialFunction, stringFunction, temporalDurationFunction, temporalInstantFunction, trigonometricFunction, vectorFunction);
  }

  public FunctionFeatures withNumericFunction(hydra.cypher.features.NumericFunctionFeatures numericFunction) {
    return new FunctionFeatures(aggregateFunction, databaseFunction, genAIFunction, graphFunction, listFunction, loadCSVFunction, logarithmicFunction, numericFunction, predicateFunction, scalarFunction, spatialFunction, stringFunction, temporalDurationFunction, temporalInstantFunction, trigonometricFunction, vectorFunction);
  }

  public FunctionFeatures withPredicateFunction(hydra.cypher.features.PredicateFunctionFeatures predicateFunction) {
    return new FunctionFeatures(aggregateFunction, databaseFunction, genAIFunction, graphFunction, listFunction, loadCSVFunction, logarithmicFunction, numericFunction, predicateFunction, scalarFunction, spatialFunction, stringFunction, temporalDurationFunction, temporalInstantFunction, trigonometricFunction, vectorFunction);
  }

  public FunctionFeatures withScalarFunction(hydra.cypher.features.ScalarFunctionFeatures scalarFunction) {
    return new FunctionFeatures(aggregateFunction, databaseFunction, genAIFunction, graphFunction, listFunction, loadCSVFunction, logarithmicFunction, numericFunction, predicateFunction, scalarFunction, spatialFunction, stringFunction, temporalDurationFunction, temporalInstantFunction, trigonometricFunction, vectorFunction);
  }

  public FunctionFeatures withSpatialFunction(hydra.cypher.features.SpatialFunctionFeatures spatialFunction) {
    return new FunctionFeatures(aggregateFunction, databaseFunction, genAIFunction, graphFunction, listFunction, loadCSVFunction, logarithmicFunction, numericFunction, predicateFunction, scalarFunction, spatialFunction, stringFunction, temporalDurationFunction, temporalInstantFunction, trigonometricFunction, vectorFunction);
  }

  public FunctionFeatures withStringFunction(hydra.cypher.features.StringFunctionFeatures stringFunction) {
    return new FunctionFeatures(aggregateFunction, databaseFunction, genAIFunction, graphFunction, listFunction, loadCSVFunction, logarithmicFunction, numericFunction, predicateFunction, scalarFunction, spatialFunction, stringFunction, temporalDurationFunction, temporalInstantFunction, trigonometricFunction, vectorFunction);
  }

  public FunctionFeatures withTemporalDurationFunction(hydra.cypher.features.TemporalDurationFunctionFeatures temporalDurationFunction) {
    return new FunctionFeatures(aggregateFunction, databaseFunction, genAIFunction, graphFunction, listFunction, loadCSVFunction, logarithmicFunction, numericFunction, predicateFunction, scalarFunction, spatialFunction, stringFunction, temporalDurationFunction, temporalInstantFunction, trigonometricFunction, vectorFunction);
  }

  public FunctionFeatures withTemporalInstantFunction(hydra.cypher.features.TemporalInstantFunctionFeatures temporalInstantFunction) {
    return new FunctionFeatures(aggregateFunction, databaseFunction, genAIFunction, graphFunction, listFunction, loadCSVFunction, logarithmicFunction, numericFunction, predicateFunction, scalarFunction, spatialFunction, stringFunction, temporalDurationFunction, temporalInstantFunction, trigonometricFunction, vectorFunction);
  }

  public FunctionFeatures withTrigonometricFunction(hydra.cypher.features.TrigonometricFunctionFeatures trigonometricFunction) {
    return new FunctionFeatures(aggregateFunction, databaseFunction, genAIFunction, graphFunction, listFunction, loadCSVFunction, logarithmicFunction, numericFunction, predicateFunction, scalarFunction, spatialFunction, stringFunction, temporalDurationFunction, temporalInstantFunction, trigonometricFunction, vectorFunction);
  }

  public FunctionFeatures withVectorFunction(hydra.cypher.features.VectorFunctionFeatures vectorFunction) {
    return new FunctionFeatures(aggregateFunction, databaseFunction, genAIFunction, graphFunction, listFunction, loadCSVFunction, logarithmicFunction, numericFunction, predicateFunction, scalarFunction, spatialFunction, stringFunction, temporalDurationFunction, temporalInstantFunction, trigonometricFunction, vectorFunction);
  }
}
