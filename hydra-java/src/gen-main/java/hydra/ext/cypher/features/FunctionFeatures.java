// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * Standard Cypher functions
 */
public class FunctionFeatures implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.cypher.features.FunctionFeatures");
  
  public static final hydra.core.Name FIELD_NAME_AGGREGATE_FUNCTION = new hydra.core.Name("aggregateFunction");
  
  public static final hydra.core.Name FIELD_NAME_DATABASE_FUNCTION = new hydra.core.Name("databaseFunction");
  
  public static final hydra.core.Name FIELD_NAME_GEN_A_I_FUNCTION = new hydra.core.Name("genAIFunction");
  
  public static final hydra.core.Name FIELD_NAME_GRAPH_FUNCTION = new hydra.core.Name("graphFunction");
  
  public static final hydra.core.Name FIELD_NAME_LIST_FUNCTION = new hydra.core.Name("listFunction");
  
  public static final hydra.core.Name FIELD_NAME_LOAD_C_S_V_FUNCTION = new hydra.core.Name("loadCSVFunction");
  
  public static final hydra.core.Name FIELD_NAME_LOGARITHMIC_FUNCTION = new hydra.core.Name("logarithmicFunction");
  
  public static final hydra.core.Name FIELD_NAME_NUMERIC_FUNCTION = new hydra.core.Name("numericFunction");
  
  public static final hydra.core.Name FIELD_NAME_PREDICATE_FUNCTION = new hydra.core.Name("predicateFunction");
  
  public static final hydra.core.Name FIELD_NAME_SCALAR_FUNCTION = new hydra.core.Name("scalarFunction");
  
  public static final hydra.core.Name FIELD_NAME_SPATIAL_FUNCTION = new hydra.core.Name("spatialFunction");
  
  public static final hydra.core.Name FIELD_NAME_STRING_FUNCTION = new hydra.core.Name("stringFunction");
  
  public static final hydra.core.Name FIELD_NAME_TEMPORAL_DURATION_FUNCTION = new hydra.core.Name("temporalDurationFunction");
  
  public static final hydra.core.Name FIELD_NAME_TEMPORAL_INSTANT_FUNCTION = new hydra.core.Name("temporalInstantFunction");
  
  public static final hydra.core.Name FIELD_NAME_TRIGONOMETRIC_FUNCTION = new hydra.core.Name("trigonometricFunction");
  
  public static final hydra.core.Name FIELD_NAME_VECTOR_FUNCTION = new hydra.core.Name("vectorFunction");
  
  /**
   * Aggregate functions
   */
  public final hydra.ext.cypher.features.AggregateFunctionFeatures aggregateFunction;
  
  /**
   * Database functions
   */
  public final hydra.ext.cypher.features.DatabaseFunctionFeatures databaseFunction;
  
  /**
   * GenAI functions
   */
  public final hydra.ext.cypher.features.GenAIFunctionFeatures genAIFunction;
  
  /**
   * Graph functions
   */
  public final hydra.ext.cypher.features.GraphFunctionFeatures graphFunction;
  
  /**
   * List functions
   */
  public final hydra.ext.cypher.features.ListFunctionFeatures listFunction;
  
  /**
   * Load CSV functions
   */
  public final hydra.ext.cypher.features.LoadCSVFunctionFeatures loadCSVFunction;
  
  /**
   * Logarithmic functions
   */
  public final hydra.ext.cypher.features.LogarithmicFunctionFeatures logarithmicFunction;
  
  /**
   * Numeric functions
   */
  public final hydra.ext.cypher.features.NumericFunctionFeatures numericFunction;
  
  /**
   * Predicate functions
   */
  public final hydra.ext.cypher.features.PredicateFunctionFeatures predicateFunction;
  
  /**
   * Scalar functions
   */
  public final hydra.ext.cypher.features.ScalarFunctionFeatures scalarFunction;
  
  /**
   * Spatial functions
   */
  public final hydra.ext.cypher.features.SpatialFunctionFeatures spatialFunction;
  
  /**
   * String functions
   */
  public final hydra.ext.cypher.features.StringFunctionFeatures stringFunction;
  
  /**
   * Temporal duration functions
   */
  public final hydra.ext.cypher.features.TemporalDurationFunctionFeatures temporalDurationFunction;
  
  /**
   * Temporal instant functions
   */
  public final hydra.ext.cypher.features.TemporalInstantFunctionFeatures temporalInstantFunction;
  
  /**
   * Trigonometric functions
   */
  public final hydra.ext.cypher.features.TrigonometricFunctionFeatures trigonometricFunction;
  
  /**
   * Vector functions
   */
  public final hydra.ext.cypher.features.VectorFunctionFeatures vectorFunction;
  
  public FunctionFeatures (hydra.ext.cypher.features.AggregateFunctionFeatures aggregateFunction, hydra.ext.cypher.features.DatabaseFunctionFeatures databaseFunction, hydra.ext.cypher.features.GenAIFunctionFeatures genAIFunction, hydra.ext.cypher.features.GraphFunctionFeatures graphFunction, hydra.ext.cypher.features.ListFunctionFeatures listFunction, hydra.ext.cypher.features.LoadCSVFunctionFeatures loadCSVFunction, hydra.ext.cypher.features.LogarithmicFunctionFeatures logarithmicFunction, hydra.ext.cypher.features.NumericFunctionFeatures numericFunction, hydra.ext.cypher.features.PredicateFunctionFeatures predicateFunction, hydra.ext.cypher.features.ScalarFunctionFeatures scalarFunction, hydra.ext.cypher.features.SpatialFunctionFeatures spatialFunction, hydra.ext.cypher.features.StringFunctionFeatures stringFunction, hydra.ext.cypher.features.TemporalDurationFunctionFeatures temporalDurationFunction, hydra.ext.cypher.features.TemporalInstantFunctionFeatures temporalInstantFunction, hydra.ext.cypher.features.TrigonometricFunctionFeatures trigonometricFunction, hydra.ext.cypher.features.VectorFunctionFeatures vectorFunction) {
    java.util.Objects.requireNonNull((aggregateFunction));
    java.util.Objects.requireNonNull((databaseFunction));
    java.util.Objects.requireNonNull((genAIFunction));
    java.util.Objects.requireNonNull((graphFunction));
    java.util.Objects.requireNonNull((listFunction));
    java.util.Objects.requireNonNull((loadCSVFunction));
    java.util.Objects.requireNonNull((logarithmicFunction));
    java.util.Objects.requireNonNull((numericFunction));
    java.util.Objects.requireNonNull((predicateFunction));
    java.util.Objects.requireNonNull((scalarFunction));
    java.util.Objects.requireNonNull((spatialFunction));
    java.util.Objects.requireNonNull((stringFunction));
    java.util.Objects.requireNonNull((temporalDurationFunction));
    java.util.Objects.requireNonNull((temporalInstantFunction));
    java.util.Objects.requireNonNull((trigonometricFunction));
    java.util.Objects.requireNonNull((vectorFunction));
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
    FunctionFeatures o = (FunctionFeatures) (other);
    return aggregateFunction.equals(o.aggregateFunction) && databaseFunction.equals(o.databaseFunction) && genAIFunction.equals(o.genAIFunction) && graphFunction.equals(o.graphFunction) && listFunction.equals(o.listFunction) && loadCSVFunction.equals(o.loadCSVFunction) && logarithmicFunction.equals(o.logarithmicFunction) && numericFunction.equals(o.numericFunction) && predicateFunction.equals(o.predicateFunction) && scalarFunction.equals(o.scalarFunction) && spatialFunction.equals(o.spatialFunction) && stringFunction.equals(o.stringFunction) && temporalDurationFunction.equals(o.temporalDurationFunction) && temporalInstantFunction.equals(o.temporalInstantFunction) && trigonometricFunction.equals(o.trigonometricFunction) && vectorFunction.equals(o.vectorFunction);
  }
  
  @Override
  public int hashCode() {
    return 2 * aggregateFunction.hashCode() + 3 * databaseFunction.hashCode() + 5 * genAIFunction.hashCode() + 7 * graphFunction.hashCode() + 11 * listFunction.hashCode() + 13 * loadCSVFunction.hashCode() + 17 * logarithmicFunction.hashCode() + 19 * numericFunction.hashCode() + 23 * predicateFunction.hashCode() + 29 * scalarFunction.hashCode() + 31 * spatialFunction.hashCode() + 37 * stringFunction.hashCode() + 41 * temporalDurationFunction.hashCode() + 43 * temporalInstantFunction.hashCode() + 47 * trigonometricFunction.hashCode() + 53 * vectorFunction.hashCode();
  }
  
  public FunctionFeatures withAggregateFunction(hydra.ext.cypher.features.AggregateFunctionFeatures aggregateFunction) {
    java.util.Objects.requireNonNull((aggregateFunction));
    return new FunctionFeatures(aggregateFunction, databaseFunction, genAIFunction, graphFunction, listFunction, loadCSVFunction, logarithmicFunction, numericFunction, predicateFunction, scalarFunction, spatialFunction, stringFunction, temporalDurationFunction, temporalInstantFunction, trigonometricFunction, vectorFunction);
  }
  
  public FunctionFeatures withDatabaseFunction(hydra.ext.cypher.features.DatabaseFunctionFeatures databaseFunction) {
    java.util.Objects.requireNonNull((databaseFunction));
    return new FunctionFeatures(aggregateFunction, databaseFunction, genAIFunction, graphFunction, listFunction, loadCSVFunction, logarithmicFunction, numericFunction, predicateFunction, scalarFunction, spatialFunction, stringFunction, temporalDurationFunction, temporalInstantFunction, trigonometricFunction, vectorFunction);
  }
  
  public FunctionFeatures withGenAIFunction(hydra.ext.cypher.features.GenAIFunctionFeatures genAIFunction) {
    java.util.Objects.requireNonNull((genAIFunction));
    return new FunctionFeatures(aggregateFunction, databaseFunction, genAIFunction, graphFunction, listFunction, loadCSVFunction, logarithmicFunction, numericFunction, predicateFunction, scalarFunction, spatialFunction, stringFunction, temporalDurationFunction, temporalInstantFunction, trigonometricFunction, vectorFunction);
  }
  
  public FunctionFeatures withGraphFunction(hydra.ext.cypher.features.GraphFunctionFeatures graphFunction) {
    java.util.Objects.requireNonNull((graphFunction));
    return new FunctionFeatures(aggregateFunction, databaseFunction, genAIFunction, graphFunction, listFunction, loadCSVFunction, logarithmicFunction, numericFunction, predicateFunction, scalarFunction, spatialFunction, stringFunction, temporalDurationFunction, temporalInstantFunction, trigonometricFunction, vectorFunction);
  }
  
  public FunctionFeatures withListFunction(hydra.ext.cypher.features.ListFunctionFeatures listFunction) {
    java.util.Objects.requireNonNull((listFunction));
    return new FunctionFeatures(aggregateFunction, databaseFunction, genAIFunction, graphFunction, listFunction, loadCSVFunction, logarithmicFunction, numericFunction, predicateFunction, scalarFunction, spatialFunction, stringFunction, temporalDurationFunction, temporalInstantFunction, trigonometricFunction, vectorFunction);
  }
  
  public FunctionFeatures withLoadCSVFunction(hydra.ext.cypher.features.LoadCSVFunctionFeatures loadCSVFunction) {
    java.util.Objects.requireNonNull((loadCSVFunction));
    return new FunctionFeatures(aggregateFunction, databaseFunction, genAIFunction, graphFunction, listFunction, loadCSVFunction, logarithmicFunction, numericFunction, predicateFunction, scalarFunction, spatialFunction, stringFunction, temporalDurationFunction, temporalInstantFunction, trigonometricFunction, vectorFunction);
  }
  
  public FunctionFeatures withLogarithmicFunction(hydra.ext.cypher.features.LogarithmicFunctionFeatures logarithmicFunction) {
    java.util.Objects.requireNonNull((logarithmicFunction));
    return new FunctionFeatures(aggregateFunction, databaseFunction, genAIFunction, graphFunction, listFunction, loadCSVFunction, logarithmicFunction, numericFunction, predicateFunction, scalarFunction, spatialFunction, stringFunction, temporalDurationFunction, temporalInstantFunction, trigonometricFunction, vectorFunction);
  }
  
  public FunctionFeatures withNumericFunction(hydra.ext.cypher.features.NumericFunctionFeatures numericFunction) {
    java.util.Objects.requireNonNull((numericFunction));
    return new FunctionFeatures(aggregateFunction, databaseFunction, genAIFunction, graphFunction, listFunction, loadCSVFunction, logarithmicFunction, numericFunction, predicateFunction, scalarFunction, spatialFunction, stringFunction, temporalDurationFunction, temporalInstantFunction, trigonometricFunction, vectorFunction);
  }
  
  public FunctionFeatures withPredicateFunction(hydra.ext.cypher.features.PredicateFunctionFeatures predicateFunction) {
    java.util.Objects.requireNonNull((predicateFunction));
    return new FunctionFeatures(aggregateFunction, databaseFunction, genAIFunction, graphFunction, listFunction, loadCSVFunction, logarithmicFunction, numericFunction, predicateFunction, scalarFunction, spatialFunction, stringFunction, temporalDurationFunction, temporalInstantFunction, trigonometricFunction, vectorFunction);
  }
  
  public FunctionFeatures withScalarFunction(hydra.ext.cypher.features.ScalarFunctionFeatures scalarFunction) {
    java.util.Objects.requireNonNull((scalarFunction));
    return new FunctionFeatures(aggregateFunction, databaseFunction, genAIFunction, graphFunction, listFunction, loadCSVFunction, logarithmicFunction, numericFunction, predicateFunction, scalarFunction, spatialFunction, stringFunction, temporalDurationFunction, temporalInstantFunction, trigonometricFunction, vectorFunction);
  }
  
  public FunctionFeatures withSpatialFunction(hydra.ext.cypher.features.SpatialFunctionFeatures spatialFunction) {
    java.util.Objects.requireNonNull((spatialFunction));
    return new FunctionFeatures(aggregateFunction, databaseFunction, genAIFunction, graphFunction, listFunction, loadCSVFunction, logarithmicFunction, numericFunction, predicateFunction, scalarFunction, spatialFunction, stringFunction, temporalDurationFunction, temporalInstantFunction, trigonometricFunction, vectorFunction);
  }
  
  public FunctionFeatures withStringFunction(hydra.ext.cypher.features.StringFunctionFeatures stringFunction) {
    java.util.Objects.requireNonNull((stringFunction));
    return new FunctionFeatures(aggregateFunction, databaseFunction, genAIFunction, graphFunction, listFunction, loadCSVFunction, logarithmicFunction, numericFunction, predicateFunction, scalarFunction, spatialFunction, stringFunction, temporalDurationFunction, temporalInstantFunction, trigonometricFunction, vectorFunction);
  }
  
  public FunctionFeatures withTemporalDurationFunction(hydra.ext.cypher.features.TemporalDurationFunctionFeatures temporalDurationFunction) {
    java.util.Objects.requireNonNull((temporalDurationFunction));
    return new FunctionFeatures(aggregateFunction, databaseFunction, genAIFunction, graphFunction, listFunction, loadCSVFunction, logarithmicFunction, numericFunction, predicateFunction, scalarFunction, spatialFunction, stringFunction, temporalDurationFunction, temporalInstantFunction, trigonometricFunction, vectorFunction);
  }
  
  public FunctionFeatures withTemporalInstantFunction(hydra.ext.cypher.features.TemporalInstantFunctionFeatures temporalInstantFunction) {
    java.util.Objects.requireNonNull((temporalInstantFunction));
    return new FunctionFeatures(aggregateFunction, databaseFunction, genAIFunction, graphFunction, listFunction, loadCSVFunction, logarithmicFunction, numericFunction, predicateFunction, scalarFunction, spatialFunction, stringFunction, temporalDurationFunction, temporalInstantFunction, trigonometricFunction, vectorFunction);
  }
  
  public FunctionFeatures withTrigonometricFunction(hydra.ext.cypher.features.TrigonometricFunctionFeatures trigonometricFunction) {
    java.util.Objects.requireNonNull((trigonometricFunction));
    return new FunctionFeatures(aggregateFunction, databaseFunction, genAIFunction, graphFunction, listFunction, loadCSVFunction, logarithmicFunction, numericFunction, predicateFunction, scalarFunction, spatialFunction, stringFunction, temporalDurationFunction, temporalInstantFunction, trigonometricFunction, vectorFunction);
  }
  
  public FunctionFeatures withVectorFunction(hydra.ext.cypher.features.VectorFunctionFeatures vectorFunction) {
    java.util.Objects.requireNonNull((vectorFunction));
    return new FunctionFeatures(aggregateFunction, databaseFunction, genAIFunction, graphFunction, listFunction, loadCSVFunction, logarithmicFunction, numericFunction, predicateFunction, scalarFunction, spatialFunction, stringFunction, temporalDurationFunction, temporalInstantFunction, trigonometricFunction, vectorFunction);
  }
}