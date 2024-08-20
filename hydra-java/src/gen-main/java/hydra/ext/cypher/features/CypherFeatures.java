// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * A set of features which characterize an OpenCypher query or implementation. Any features which are omitted from the set are assumed to be unsupported or nonrequired.
 */
public class CypherFeatures implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/cypher/features.CypherFeatures");
  
  public static final hydra.core.Name FIELD_NAME_ARITHMETIC = new hydra.core.Name("arithmetic");
  
  public static final hydra.core.Name FIELD_NAME_ATOM = new hydra.core.Name("atom");
  
  public static final hydra.core.Name FIELD_NAME_COMPARISON = new hydra.core.Name("comparison");
  
  public static final hydra.core.Name FIELD_NAME_DELETE = new hydra.core.Name("delete");
  
  public static final hydra.core.Name FIELD_NAME_FUNCTION = new hydra.core.Name("function");
  
  public static final hydra.core.Name FIELD_NAME_LIST = new hydra.core.Name("list");
  
  public static final hydra.core.Name FIELD_NAME_LITERAL = new hydra.core.Name("literal");
  
  public static final hydra.core.Name FIELD_NAME_LOGICAL = new hydra.core.Name("logical");
  
  public static final hydra.core.Name FIELD_NAME_MATCH = new hydra.core.Name("match");
  
  public static final hydra.core.Name FIELD_NAME_MERGE = new hydra.core.Name("merge");
  
  public static final hydra.core.Name FIELD_NAME_NODE_PATTERN = new hydra.core.Name("nodePattern");
  
  public static final hydra.core.Name FIELD_NAME_NULL = new hydra.core.Name("null");
  
  public static final hydra.core.Name FIELD_NAME_PATH = new hydra.core.Name("path");
  
  public static final hydra.core.Name FIELD_NAME_PROCEDURE_CALL = new hydra.core.Name("procedureCall");
  
  public static final hydra.core.Name FIELD_NAME_PROJECTION = new hydra.core.Name("projection");
  
  public static final hydra.core.Name FIELD_NAME_QUANTIFIER = new hydra.core.Name("quantifier");
  
  public static final hydra.core.Name FIELD_NAME_RANGE_LITERAL = new hydra.core.Name("rangeLiteral");
  
  public static final hydra.core.Name FIELD_NAME_READING = new hydra.core.Name("reading");
  
  public static final hydra.core.Name FIELD_NAME_RELATIONSHIP_DIRECTION = new hydra.core.Name("relationshipDirection");
  
  public static final hydra.core.Name FIELD_NAME_RELATIONSHIP_PATTERN = new hydra.core.Name("relationshipPattern");
  
  public static final hydra.core.Name FIELD_NAME_REMOVE = new hydra.core.Name("remove");
  
  public static final hydra.core.Name FIELD_NAME_SET = new hydra.core.Name("set");
  
  public static final hydra.core.Name FIELD_NAME_STRING = new hydra.core.Name("string");
  
  public static final hydra.core.Name FIELD_NAME_UPDATING = new hydra.core.Name("updating");
  
  /**
   * Arithmetic operations
   */
  public final hydra.ext.cypher.features.ArithmeticFeatures arithmetic;
  
  /**
   * Various kinds of atomic expressions
   */
  public final hydra.ext.cypher.features.AtomFeatures atom;
  
  /**
   * Comparison operators and functions
   */
  public final hydra.ext.cypher.features.ComparisonFeatures comparison;
  
  /**
   * Delete operations
   */
  public final hydra.ext.cypher.features.DeleteFeatures delete;
  
  /**
   * Standard Cypher functions
   */
  public final hydra.ext.cypher.features.FunctionFeatures function;
  
  /**
   * List functionality
   */
  public final hydra.ext.cypher.features.ListFeatures list;
  
  /**
   * Various types of literal values
   */
  public final hydra.ext.cypher.features.LiteralFeatures literal;
  
  /**
   * Logical operations
   */
  public final hydra.ext.cypher.features.LogicalFeatures logical;
  
  /**
   * Match queries
   */
  public final hydra.ext.cypher.features.MatchFeatures match;
  
  /**
   * Merge operations
   */
  public final hydra.ext.cypher.features.MergeFeatures merge;
  
  /**
   * Node patterns
   */
  public final hydra.ext.cypher.features.NodePatternFeatures nodePattern;
  
  /**
   * IS NULL / IS NOT NULL checks
   */
  public final hydra.ext.cypher.features.NullFeatures null_;
  
  /**
   * Path functions only found in OpenCypher
   */
  public final hydra.ext.cypher.features.PathFeatures path;
  
  /**
   * Procedure calls
   */
  public final hydra.ext.cypher.features.ProcedureCallFeatures procedureCall;
  
  /**
   * Projections
   */
  public final hydra.ext.cypher.features.ProjectionFeatures projection;
  
  /**
   * Quantifier expressions
   */
  public final hydra.ext.cypher.features.QuantifierFeatures quantifier;
  
  /**
   * Range literals within relationship patterns
   */
  public final hydra.ext.cypher.features.RangeLiteralFeatures rangeLiteral;
  
  /**
   * Specific syntax related to reading data from the graph.
   */
  public final hydra.ext.cypher.features.ReadingFeatures reading;
  
  /**
   * Relationship directions / arrow patterns
   */
  public final hydra.ext.cypher.features.RelationshipDirectionFeatures relationshipDirection;
  
  /**
   * Relationship patterns
   */
  public final hydra.ext.cypher.features.RelationshipPatternFeatures relationshipPattern;
  
  /**
   * REMOVE operations
   */
  public final hydra.ext.cypher.features.RemoveFeatures remove;
  
  /**
   * Set definitions
   */
  public final hydra.ext.cypher.features.SetFeatures set;
  
  /**
   * String functions/keywords only found in OpenCypher
   */
  public final hydra.ext.cypher.features.StringFeatures string;
  
  /**
   * Specific syntax related to updating data in the graph
   */
  public final hydra.ext.cypher.features.UpdatingFeatures updating;
  
  public CypherFeatures (hydra.ext.cypher.features.ArithmeticFeatures arithmetic, hydra.ext.cypher.features.AtomFeatures atom, hydra.ext.cypher.features.ComparisonFeatures comparison, hydra.ext.cypher.features.DeleteFeatures delete, hydra.ext.cypher.features.FunctionFeatures function, hydra.ext.cypher.features.ListFeatures list, hydra.ext.cypher.features.LiteralFeatures literal, hydra.ext.cypher.features.LogicalFeatures logical, hydra.ext.cypher.features.MatchFeatures match, hydra.ext.cypher.features.MergeFeatures merge, hydra.ext.cypher.features.NodePatternFeatures nodePattern, hydra.ext.cypher.features.NullFeatures null_, hydra.ext.cypher.features.PathFeatures path, hydra.ext.cypher.features.ProcedureCallFeatures procedureCall, hydra.ext.cypher.features.ProjectionFeatures projection, hydra.ext.cypher.features.QuantifierFeatures quantifier, hydra.ext.cypher.features.RangeLiteralFeatures rangeLiteral, hydra.ext.cypher.features.ReadingFeatures reading, hydra.ext.cypher.features.RelationshipDirectionFeatures relationshipDirection, hydra.ext.cypher.features.RelationshipPatternFeatures relationshipPattern, hydra.ext.cypher.features.RemoveFeatures remove, hydra.ext.cypher.features.SetFeatures set, hydra.ext.cypher.features.StringFeatures string, hydra.ext.cypher.features.UpdatingFeatures updating) {
    java.util.Objects.requireNonNull((arithmetic));
    java.util.Objects.requireNonNull((atom));
    java.util.Objects.requireNonNull((comparison));
    java.util.Objects.requireNonNull((delete));
    java.util.Objects.requireNonNull((function));
    java.util.Objects.requireNonNull((list));
    java.util.Objects.requireNonNull((literal));
    java.util.Objects.requireNonNull((logical));
    java.util.Objects.requireNonNull((match));
    java.util.Objects.requireNonNull((merge));
    java.util.Objects.requireNonNull((nodePattern));
    java.util.Objects.requireNonNull((null_));
    java.util.Objects.requireNonNull((path));
    java.util.Objects.requireNonNull((procedureCall));
    java.util.Objects.requireNonNull((projection));
    java.util.Objects.requireNonNull((quantifier));
    java.util.Objects.requireNonNull((rangeLiteral));
    java.util.Objects.requireNonNull((reading));
    java.util.Objects.requireNonNull((relationshipDirection));
    java.util.Objects.requireNonNull((relationshipPattern));
    java.util.Objects.requireNonNull((remove));
    java.util.Objects.requireNonNull((set));
    java.util.Objects.requireNonNull((string));
    java.util.Objects.requireNonNull((updating));
    this.arithmetic = arithmetic;
    this.atom = atom;
    this.comparison = comparison;
    this.delete = delete;
    this.function = function;
    this.list = list;
    this.literal = literal;
    this.logical = logical;
    this.match = match;
    this.merge = merge;
    this.nodePattern = nodePattern;
    this.null_ = null_;
    this.path = path;
    this.procedureCall = procedureCall;
    this.projection = projection;
    this.quantifier = quantifier;
    this.rangeLiteral = rangeLiteral;
    this.reading = reading;
    this.relationshipDirection = relationshipDirection;
    this.relationshipPattern = relationshipPattern;
    this.remove = remove;
    this.set = set;
    this.string = string;
    this.updating = updating;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CypherFeatures)) {
      return false;
    }
    CypherFeatures o = (CypherFeatures) (other);
    return arithmetic.equals(o.arithmetic) && atom.equals(o.atom) && comparison.equals(o.comparison) && delete.equals(o.delete) && function.equals(o.function) && list.equals(o.list) && literal.equals(o.literal) && logical.equals(o.logical) && match.equals(o.match) && merge.equals(o.merge) && nodePattern.equals(o.nodePattern) && null_.equals(o.null_) && path.equals(o.path) && procedureCall.equals(o.procedureCall) && projection.equals(o.projection) && quantifier.equals(o.quantifier) && rangeLiteral.equals(o.rangeLiteral) && reading.equals(o.reading) && relationshipDirection.equals(o.relationshipDirection) && relationshipPattern.equals(o.relationshipPattern) && remove.equals(o.remove) && set.equals(o.set) && string.equals(o.string) && updating.equals(o.updating);
  }
  
  @Override
  public int hashCode() {
    return 2 * arithmetic.hashCode() + 3 * atom.hashCode() + 5 * comparison.hashCode() + 7 * delete.hashCode() + 11 * function.hashCode() + 13 * list.hashCode() + 17 * literal.hashCode() + 19 * logical.hashCode() + 23 * match.hashCode() + 29 * merge.hashCode() + 31 * nodePattern.hashCode() + 37 * null_.hashCode() + 41 * path.hashCode() + 43 * procedureCall.hashCode() + 47 * projection.hashCode() + 53 * quantifier.hashCode() + 59 * rangeLiteral.hashCode() + 61 * reading.hashCode() + 67 * relationshipDirection.hashCode() + 71 * relationshipPattern.hashCode() + 2 * remove.hashCode() + 3 * set.hashCode() + 5 * string.hashCode() + 7 * updating.hashCode();
  }
  
  public CypherFeatures withArithmetic(hydra.ext.cypher.features.ArithmeticFeatures arithmetic) {
    java.util.Objects.requireNonNull((arithmetic));
    return new CypherFeatures(arithmetic, atom, comparison, delete, function, list, literal, logical, match, merge, nodePattern, null_, path, procedureCall, projection, quantifier, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, set, string, updating);
  }
  
  public CypherFeatures withAtom(hydra.ext.cypher.features.AtomFeatures atom) {
    java.util.Objects.requireNonNull((atom));
    return new CypherFeatures(arithmetic, atom, comparison, delete, function, list, literal, logical, match, merge, nodePattern, null_, path, procedureCall, projection, quantifier, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, set, string, updating);
  }
  
  public CypherFeatures withComparison(hydra.ext.cypher.features.ComparisonFeatures comparison) {
    java.util.Objects.requireNonNull((comparison));
    return new CypherFeatures(arithmetic, atom, comparison, delete, function, list, literal, logical, match, merge, nodePattern, null_, path, procedureCall, projection, quantifier, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, set, string, updating);
  }
  
  public CypherFeatures withDelete(hydra.ext.cypher.features.DeleteFeatures delete) {
    java.util.Objects.requireNonNull((delete));
    return new CypherFeatures(arithmetic, atom, comparison, delete, function, list, literal, logical, match, merge, nodePattern, null_, path, procedureCall, projection, quantifier, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, set, string, updating);
  }
  
  public CypherFeatures withFunction(hydra.ext.cypher.features.FunctionFeatures function) {
    java.util.Objects.requireNonNull((function));
    return new CypherFeatures(arithmetic, atom, comparison, delete, function, list, literal, logical, match, merge, nodePattern, null_, path, procedureCall, projection, quantifier, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, set, string, updating);
  }
  
  public CypherFeatures withList(hydra.ext.cypher.features.ListFeatures list) {
    java.util.Objects.requireNonNull((list));
    return new CypherFeatures(arithmetic, atom, comparison, delete, function, list, literal, logical, match, merge, nodePattern, null_, path, procedureCall, projection, quantifier, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, set, string, updating);
  }
  
  public CypherFeatures withLiteral(hydra.ext.cypher.features.LiteralFeatures literal) {
    java.util.Objects.requireNonNull((literal));
    return new CypherFeatures(arithmetic, atom, comparison, delete, function, list, literal, logical, match, merge, nodePattern, null_, path, procedureCall, projection, quantifier, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, set, string, updating);
  }
  
  public CypherFeatures withLogical(hydra.ext.cypher.features.LogicalFeatures logical) {
    java.util.Objects.requireNonNull((logical));
    return new CypherFeatures(arithmetic, atom, comparison, delete, function, list, literal, logical, match, merge, nodePattern, null_, path, procedureCall, projection, quantifier, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, set, string, updating);
  }
  
  public CypherFeatures withMatch(hydra.ext.cypher.features.MatchFeatures match) {
    java.util.Objects.requireNonNull((match));
    return new CypherFeatures(arithmetic, atom, comparison, delete, function, list, literal, logical, match, merge, nodePattern, null_, path, procedureCall, projection, quantifier, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, set, string, updating);
  }
  
  public CypherFeatures withMerge(hydra.ext.cypher.features.MergeFeatures merge) {
    java.util.Objects.requireNonNull((merge));
    return new CypherFeatures(arithmetic, atom, comparison, delete, function, list, literal, logical, match, merge, nodePattern, null_, path, procedureCall, projection, quantifier, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, set, string, updating);
  }
  
  public CypherFeatures withNodePattern(hydra.ext.cypher.features.NodePatternFeatures nodePattern) {
    java.util.Objects.requireNonNull((nodePattern));
    return new CypherFeatures(arithmetic, atom, comparison, delete, function, list, literal, logical, match, merge, nodePattern, null_, path, procedureCall, projection, quantifier, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, set, string, updating);
  }
  
  public CypherFeatures withNull(hydra.ext.cypher.features.NullFeatures null_) {
    java.util.Objects.requireNonNull((null_));
    return new CypherFeatures(arithmetic, atom, comparison, delete, function, list, literal, logical, match, merge, nodePattern, null_, path, procedureCall, projection, quantifier, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, set, string, updating);
  }
  
  public CypherFeatures withPath(hydra.ext.cypher.features.PathFeatures path) {
    java.util.Objects.requireNonNull((path));
    return new CypherFeatures(arithmetic, atom, comparison, delete, function, list, literal, logical, match, merge, nodePattern, null_, path, procedureCall, projection, quantifier, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, set, string, updating);
  }
  
  public CypherFeatures withProcedureCall(hydra.ext.cypher.features.ProcedureCallFeatures procedureCall) {
    java.util.Objects.requireNonNull((procedureCall));
    return new CypherFeatures(arithmetic, atom, comparison, delete, function, list, literal, logical, match, merge, nodePattern, null_, path, procedureCall, projection, quantifier, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, set, string, updating);
  }
  
  public CypherFeatures withProjection(hydra.ext.cypher.features.ProjectionFeatures projection) {
    java.util.Objects.requireNonNull((projection));
    return new CypherFeatures(arithmetic, atom, comparison, delete, function, list, literal, logical, match, merge, nodePattern, null_, path, procedureCall, projection, quantifier, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, set, string, updating);
  }
  
  public CypherFeatures withQuantifier(hydra.ext.cypher.features.QuantifierFeatures quantifier) {
    java.util.Objects.requireNonNull((quantifier));
    return new CypherFeatures(arithmetic, atom, comparison, delete, function, list, literal, logical, match, merge, nodePattern, null_, path, procedureCall, projection, quantifier, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, set, string, updating);
  }
  
  public CypherFeatures withRangeLiteral(hydra.ext.cypher.features.RangeLiteralFeatures rangeLiteral) {
    java.util.Objects.requireNonNull((rangeLiteral));
    return new CypherFeatures(arithmetic, atom, comparison, delete, function, list, literal, logical, match, merge, nodePattern, null_, path, procedureCall, projection, quantifier, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, set, string, updating);
  }
  
  public CypherFeatures withReading(hydra.ext.cypher.features.ReadingFeatures reading) {
    java.util.Objects.requireNonNull((reading));
    return new CypherFeatures(arithmetic, atom, comparison, delete, function, list, literal, logical, match, merge, nodePattern, null_, path, procedureCall, projection, quantifier, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, set, string, updating);
  }
  
  public CypherFeatures withRelationshipDirection(hydra.ext.cypher.features.RelationshipDirectionFeatures relationshipDirection) {
    java.util.Objects.requireNonNull((relationshipDirection));
    return new CypherFeatures(arithmetic, atom, comparison, delete, function, list, literal, logical, match, merge, nodePattern, null_, path, procedureCall, projection, quantifier, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, set, string, updating);
  }
  
  public CypherFeatures withRelationshipPattern(hydra.ext.cypher.features.RelationshipPatternFeatures relationshipPattern) {
    java.util.Objects.requireNonNull((relationshipPattern));
    return new CypherFeatures(arithmetic, atom, comparison, delete, function, list, literal, logical, match, merge, nodePattern, null_, path, procedureCall, projection, quantifier, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, set, string, updating);
  }
  
  public CypherFeatures withRemove(hydra.ext.cypher.features.RemoveFeatures remove) {
    java.util.Objects.requireNonNull((remove));
    return new CypherFeatures(arithmetic, atom, comparison, delete, function, list, literal, logical, match, merge, nodePattern, null_, path, procedureCall, projection, quantifier, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, set, string, updating);
  }
  
  public CypherFeatures withSet(hydra.ext.cypher.features.SetFeatures set) {
    java.util.Objects.requireNonNull((set));
    return new CypherFeatures(arithmetic, atom, comparison, delete, function, list, literal, logical, match, merge, nodePattern, null_, path, procedureCall, projection, quantifier, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, set, string, updating);
  }
  
  public CypherFeatures withString(hydra.ext.cypher.features.StringFeatures string) {
    java.util.Objects.requireNonNull((string));
    return new CypherFeatures(arithmetic, atom, comparison, delete, function, list, literal, logical, match, merge, nodePattern, null_, path, procedureCall, projection, quantifier, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, set, string, updating);
  }
  
  public CypherFeatures withUpdating(hydra.ext.cypher.features.UpdatingFeatures updating) {
    java.util.Objects.requireNonNull((updating));
    return new CypherFeatures(arithmetic, atom, comparison, delete, function, list, literal, logical, match, merge, nodePattern, null_, path, procedureCall, projection, quantifier, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, set, string, updating);
  }
}