// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * A set of features which characterize an OpenCypher query or implementation. Any features which are omitted from the set are assumed to be unsupported or nonrequired.
 */
public class CypherFeatures implements Serializable, Comparable<CypherFeatures> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.features.CypherFeatures");

  public static final hydra.core.Name ARITHMETIC = new hydra.core.Name("arithmetic");

  public static final hydra.core.Name ATOM = new hydra.core.Name("atom");

  public static final hydra.core.Name COMPARISON = new hydra.core.Name("comparison");

  public static final hydra.core.Name DELETE = new hydra.core.Name("delete");

  public static final hydra.core.Name FUNCTION = new hydra.core.Name("function");

  public static final hydra.core.Name LIST = new hydra.core.Name("list");

  public static final hydra.core.Name LITERAL = new hydra.core.Name("literal");

  public static final hydra.core.Name LOGICAL = new hydra.core.Name("logical");

  public static final hydra.core.Name MATCH = new hydra.core.Name("match");

  public static final hydra.core.Name MERGE = new hydra.core.Name("merge");

  public static final hydra.core.Name NODE_PATTERN = new hydra.core.Name("nodePattern");

  public static final hydra.core.Name NULL = new hydra.core.Name("null");

  public static final hydra.core.Name PATH = new hydra.core.Name("path");

  public static final hydra.core.Name PROCEDURE_CALL = new hydra.core.Name("procedureCall");

  public static final hydra.core.Name PROJECTION = new hydra.core.Name("projection");

  public static final hydra.core.Name QUANTIFIER = new hydra.core.Name("quantifier");

  public static final hydra.core.Name RANGE_LITERAL = new hydra.core.Name("rangeLiteral");

  public static final hydra.core.Name READING = new hydra.core.Name("reading");

  public static final hydra.core.Name RELATIONSHIP_DIRECTION = new hydra.core.Name("relationshipDirection");

  public static final hydra.core.Name RELATIONSHIP_PATTERN = new hydra.core.Name("relationshipPattern");

  public static final hydra.core.Name REMOVE = new hydra.core.Name("remove");

  public static final hydra.core.Name SET = new hydra.core.Name("set");

  public static final hydra.core.Name STRING = new hydra.core.Name("string");

  public static final hydra.core.Name UPDATING = new hydra.core.Name("updating");

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
    CypherFeatures o = (CypherFeatures) other;
    return java.util.Objects.equals(
      this.arithmetic,
      o.arithmetic) && java.util.Objects.equals(
      this.atom,
      o.atom) && java.util.Objects.equals(
      this.comparison,
      o.comparison) && java.util.Objects.equals(
      this.delete,
      o.delete) && java.util.Objects.equals(
      this.function,
      o.function) && java.util.Objects.equals(
      this.list,
      o.list) && java.util.Objects.equals(
      this.literal,
      o.literal) && java.util.Objects.equals(
      this.logical,
      o.logical) && java.util.Objects.equals(
      this.match,
      o.match) && java.util.Objects.equals(
      this.merge,
      o.merge) && java.util.Objects.equals(
      this.nodePattern,
      o.nodePattern) && java.util.Objects.equals(
      this.null_,
      o.null_) && java.util.Objects.equals(
      this.path,
      o.path) && java.util.Objects.equals(
      this.procedureCall,
      o.procedureCall) && java.util.Objects.equals(
      this.projection,
      o.projection) && java.util.Objects.equals(
      this.quantifier,
      o.quantifier) && java.util.Objects.equals(
      this.rangeLiteral,
      o.rangeLiteral) && java.util.Objects.equals(
      this.reading,
      o.reading) && java.util.Objects.equals(
      this.relationshipDirection,
      o.relationshipDirection) && java.util.Objects.equals(
      this.relationshipPattern,
      o.relationshipPattern) && java.util.Objects.equals(
      this.remove,
      o.remove) && java.util.Objects.equals(
      this.set,
      o.set) && java.util.Objects.equals(
      this.string,
      o.string) && java.util.Objects.equals(
      this.updating,
      o.updating);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(arithmetic) + 3 * java.util.Objects.hashCode(atom) + 5 * java.util.Objects.hashCode(comparison) + 7 * java.util.Objects.hashCode(delete) + 11 * java.util.Objects.hashCode(function) + 13 * java.util.Objects.hashCode(list) + 17 * java.util.Objects.hashCode(literal) + 19 * java.util.Objects.hashCode(logical) + 23 * java.util.Objects.hashCode(match) + 29 * java.util.Objects.hashCode(merge) + 31 * java.util.Objects.hashCode(nodePattern) + 37 * java.util.Objects.hashCode(null_) + 41 * java.util.Objects.hashCode(path) + 43 * java.util.Objects.hashCode(procedureCall) + 47 * java.util.Objects.hashCode(projection) + 53 * java.util.Objects.hashCode(quantifier) + 59 * java.util.Objects.hashCode(rangeLiteral) + 61 * java.util.Objects.hashCode(reading) + 67 * java.util.Objects.hashCode(relationshipDirection) + 71 * java.util.Objects.hashCode(relationshipPattern);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(CypherFeatures other) {
    int cmp = 0;
    cmp = ((Comparable) arithmetic).compareTo(other.arithmetic);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) atom).compareTo(other.atom);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) comparison).compareTo(other.comparison);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) delete).compareTo(other.delete);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) function).compareTo(other.function);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) list).compareTo(other.list);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) literal).compareTo(other.literal);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) logical).compareTo(other.logical);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) match).compareTo(other.match);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) merge).compareTo(other.merge);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) nodePattern).compareTo(other.nodePattern);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) null_).compareTo(other.null_);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) path).compareTo(other.path);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) procedureCall).compareTo(other.procedureCall);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) projection).compareTo(other.projection);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) quantifier).compareTo(other.quantifier);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) rangeLiteral).compareTo(other.rangeLiteral);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) reading).compareTo(other.reading);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) relationshipDirection).compareTo(other.relationshipDirection);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) relationshipPattern).compareTo(other.relationshipPattern);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) remove).compareTo(other.remove);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) set).compareTo(other.set);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) string).compareTo(other.string);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) updating).compareTo(other.updating);
  }

  public CypherFeatures withArithmetic(hydra.ext.cypher.features.ArithmeticFeatures arithmetic) {
    return new CypherFeatures(arithmetic, atom, comparison, delete, function, list, literal, logical, match, merge, nodePattern, null_, path, procedureCall, projection, quantifier, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, set, string, updating);
  }

  public CypherFeatures withAtom(hydra.ext.cypher.features.AtomFeatures atom) {
    return new CypherFeatures(arithmetic, atom, comparison, delete, function, list, literal, logical, match, merge, nodePattern, null_, path, procedureCall, projection, quantifier, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, set, string, updating);
  }

  public CypherFeatures withComparison(hydra.ext.cypher.features.ComparisonFeatures comparison) {
    return new CypherFeatures(arithmetic, atom, comparison, delete, function, list, literal, logical, match, merge, nodePattern, null_, path, procedureCall, projection, quantifier, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, set, string, updating);
  }

  public CypherFeatures withDelete(hydra.ext.cypher.features.DeleteFeatures delete) {
    return new CypherFeatures(arithmetic, atom, comparison, delete, function, list, literal, logical, match, merge, nodePattern, null_, path, procedureCall, projection, quantifier, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, set, string, updating);
  }

  public CypherFeatures withFunction(hydra.ext.cypher.features.FunctionFeatures function) {
    return new CypherFeatures(arithmetic, atom, comparison, delete, function, list, literal, logical, match, merge, nodePattern, null_, path, procedureCall, projection, quantifier, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, set, string, updating);
  }

  public CypherFeatures withList(hydra.ext.cypher.features.ListFeatures list) {
    return new CypherFeatures(arithmetic, atom, comparison, delete, function, list, literal, logical, match, merge, nodePattern, null_, path, procedureCall, projection, quantifier, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, set, string, updating);
  }

  public CypherFeatures withLiteral(hydra.ext.cypher.features.LiteralFeatures literal) {
    return new CypherFeatures(arithmetic, atom, comparison, delete, function, list, literal, logical, match, merge, nodePattern, null_, path, procedureCall, projection, quantifier, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, set, string, updating);
  }

  public CypherFeatures withLogical(hydra.ext.cypher.features.LogicalFeatures logical) {
    return new CypherFeatures(arithmetic, atom, comparison, delete, function, list, literal, logical, match, merge, nodePattern, null_, path, procedureCall, projection, quantifier, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, set, string, updating);
  }

  public CypherFeatures withMatch(hydra.ext.cypher.features.MatchFeatures match) {
    return new CypherFeatures(arithmetic, atom, comparison, delete, function, list, literal, logical, match, merge, nodePattern, null_, path, procedureCall, projection, quantifier, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, set, string, updating);
  }

  public CypherFeatures withMerge(hydra.ext.cypher.features.MergeFeatures merge) {
    return new CypherFeatures(arithmetic, atom, comparison, delete, function, list, literal, logical, match, merge, nodePattern, null_, path, procedureCall, projection, quantifier, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, set, string, updating);
  }

  public CypherFeatures withNodePattern(hydra.ext.cypher.features.NodePatternFeatures nodePattern) {
    return new CypherFeatures(arithmetic, atom, comparison, delete, function, list, literal, logical, match, merge, nodePattern, null_, path, procedureCall, projection, quantifier, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, set, string, updating);
  }

  public CypherFeatures withNull(hydra.ext.cypher.features.NullFeatures null_) {
    return new CypherFeatures(arithmetic, atom, comparison, delete, function, list, literal, logical, match, merge, nodePattern, null_, path, procedureCall, projection, quantifier, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, set, string, updating);
  }

  public CypherFeatures withPath(hydra.ext.cypher.features.PathFeatures path) {
    return new CypherFeatures(arithmetic, atom, comparison, delete, function, list, literal, logical, match, merge, nodePattern, null_, path, procedureCall, projection, quantifier, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, set, string, updating);
  }

  public CypherFeatures withProcedureCall(hydra.ext.cypher.features.ProcedureCallFeatures procedureCall) {
    return new CypherFeatures(arithmetic, atom, comparison, delete, function, list, literal, logical, match, merge, nodePattern, null_, path, procedureCall, projection, quantifier, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, set, string, updating);
  }

  public CypherFeatures withProjection(hydra.ext.cypher.features.ProjectionFeatures projection) {
    return new CypherFeatures(arithmetic, atom, comparison, delete, function, list, literal, logical, match, merge, nodePattern, null_, path, procedureCall, projection, quantifier, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, set, string, updating);
  }

  public CypherFeatures withQuantifier(hydra.ext.cypher.features.QuantifierFeatures quantifier) {
    return new CypherFeatures(arithmetic, atom, comparison, delete, function, list, literal, logical, match, merge, nodePattern, null_, path, procedureCall, projection, quantifier, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, set, string, updating);
  }

  public CypherFeatures withRangeLiteral(hydra.ext.cypher.features.RangeLiteralFeatures rangeLiteral) {
    return new CypherFeatures(arithmetic, atom, comparison, delete, function, list, literal, logical, match, merge, nodePattern, null_, path, procedureCall, projection, quantifier, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, set, string, updating);
  }

  public CypherFeatures withReading(hydra.ext.cypher.features.ReadingFeatures reading) {
    return new CypherFeatures(arithmetic, atom, comparison, delete, function, list, literal, logical, match, merge, nodePattern, null_, path, procedureCall, projection, quantifier, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, set, string, updating);
  }

  public CypherFeatures withRelationshipDirection(hydra.ext.cypher.features.RelationshipDirectionFeatures relationshipDirection) {
    return new CypherFeatures(arithmetic, atom, comparison, delete, function, list, literal, logical, match, merge, nodePattern, null_, path, procedureCall, projection, quantifier, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, set, string, updating);
  }

  public CypherFeatures withRelationshipPattern(hydra.ext.cypher.features.RelationshipPatternFeatures relationshipPattern) {
    return new CypherFeatures(arithmetic, atom, comparison, delete, function, list, literal, logical, match, merge, nodePattern, null_, path, procedureCall, projection, quantifier, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, set, string, updating);
  }

  public CypherFeatures withRemove(hydra.ext.cypher.features.RemoveFeatures remove) {
    return new CypherFeatures(arithmetic, atom, comparison, delete, function, list, literal, logical, match, merge, nodePattern, null_, path, procedureCall, projection, quantifier, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, set, string, updating);
  }

  public CypherFeatures withSet(hydra.ext.cypher.features.SetFeatures set) {
    return new CypherFeatures(arithmetic, atom, comparison, delete, function, list, literal, logical, match, merge, nodePattern, null_, path, procedureCall, projection, quantifier, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, set, string, updating);
  }

  public CypherFeatures withString(hydra.ext.cypher.features.StringFeatures string) {
    return new CypherFeatures(arithmetic, atom, comparison, delete, function, list, literal, logical, match, merge, nodePattern, null_, path, procedureCall, projection, quantifier, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, set, string, updating);
  }

  public CypherFeatures withUpdating(hydra.ext.cypher.features.UpdatingFeatures updating) {
    return new CypherFeatures(arithmetic, atom, comparison, delete, function, list, literal, logical, match, merge, nodePattern, null_, path, procedureCall, projection, quantifier, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, set, string, updating);
  }
}
