// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.features;

import java.io.Serializable;

/**
 * A set of features which characterize an OpenCypher query or implementation. Any features which are omitted from the set are assumed to be unsupported or nonrequired.
 */
public class CypherFeatures implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/features.CypherFeatures");
  
  /**
   * Whether to expect aggregate functions, and if so, which specific features
   */
  public final hydra.util.Opt<hydra.langs.cypher.features.AggregateFeatures> aggregate;
  
  /**
   * Whether to expect arithmetic operations, and if so, which specific features
   */
  public final hydra.util.Opt<hydra.langs.cypher.features.ArithmeticFeatures> arithmetic;
  
  /**
   * Whether to expect atomic expressions, and if so, which specific features
   */
  public final hydra.util.Opt<hydra.langs.cypher.features.AtomFeatures> atom;
  
  /**
   * Whether to expect comparison operations, and if so, which specific features
   */
  public final hydra.util.Opt<hydra.langs.cypher.features.ComparisonFeatures> comparison;
  
  /**
   * Whether to expect delete operations, and if so, which specific features
   */
  public final hydra.util.Opt<hydra.langs.cypher.features.DeleteFeatures> delete;
  
  /**
   * Whether to expect element functions, and if so, which specific features
   */
  public final hydra.util.Opt<hydra.langs.cypher.features.ElementFeatures> element;
  
  /**
   * Whether to expect logical operations, and if so, which specific features
   */
  public final hydra.util.Opt<hydra.langs.cypher.features.LogicalFeatures> logical;
  
  /**
   * Whether to expect property map functions, and if so, which specific features
   */
  public final hydra.util.Opt<hydra.langs.cypher.features.MapFeatures> map;
  
  /**
   * Whether to expect match queries, and if so, which specific features
   */
  public final hydra.util.Opt<hydra.langs.cypher.features.MatchFeatures> match;
  
  /**
   * Whether to expect merge operations, and if so, which specific features
   */
  public final hydra.util.Opt<hydra.langs.cypher.features.MergeFeatures> merge;
  
  /**
   * Whether to expect node patterns, and if so, which specific features
   */
  public final hydra.util.Opt<hydra.langs.cypher.features.NodePatternFeatures> nodePattern;
  
  /**
   * Whether to expect IS NULL / IS NOT NULL checks, and if so, which specific features
   */
  public final hydra.util.Opt<hydra.langs.cypher.features.NullFeatures> null_;
  
  /**
   * Whether to expect numeric functions, and if so, which specific features
   */
  public final hydra.util.Opt<hydra.langs.cypher.features.NumericFeatures> numeric;
  
  /**
   * Whether to expect path functions, and if so, which specific features
   */
  public final hydra.util.Opt<hydra.langs.cypher.features.PathFeatures> path;
  
  /**
   * Whether to expect procedure calls, and if so, which specific features
   */
  public final hydra.util.Opt<hydra.langs.cypher.features.ProcedureCallFeatures> procedureCall;
  
  /**
   * Whether to expect projection operations, and if so, which specific features
   */
  public final hydra.util.Opt<hydra.langs.cypher.features.ProjectionFeatures> projection;
  
  /**
   * Whether to expect random value generation, and if so, which specific features
   */
  public final hydra.util.Opt<hydra.langs.cypher.features.RandomnessFeatures> randomness;
  
  /**
   * Whether to expect range literals, and if so, which specific features
   */
  public final hydra.util.Opt<hydra.langs.cypher.features.RangeLiteralFeatures> rangeLiteral;
  
  /**
   * Whether to expect reading operations, and if so, which specific features
   */
  public final hydra.util.Opt<hydra.langs.cypher.features.ReadingFeatures> reading;
  
  /**
   * Whether to expect relationship directions, and if so, which specific features
   */
  public final hydra.util.Opt<hydra.langs.cypher.features.RelationshipDirectionFeatures> relationshipDirection;
  
  /**
   * Whether to expect relationship patterns, and if so, which specific features
   */
  public final hydra.util.Opt<hydra.langs.cypher.features.RelationshipPatternFeatures> relationshipPattern;
  
  /**
   * Whether to expect remove operations, and if so, which specific features
   */
  public final hydra.util.Opt<hydra.langs.cypher.features.RemoveFeatures> remove;
  
  /**
   * Whether to expect schema functions, and if so, which specific features
   */
  public final hydra.util.Opt<hydra.langs.cypher.features.SchemaFeatures> schema;
  
  /**
   * Whether to expect set operations, and if so, which specific features
   */
  public final hydra.util.Opt<hydra.langs.cypher.features.SetFeatures> set;
  
  /**
   * Whether to expect string operations, and if so, which specific features
   */
  public final hydra.util.Opt<hydra.langs.cypher.features.StringFeatures> string;
  
  /**
   * Whether to expect updating operations, and if so, which specific features
   */
  public final hydra.util.Opt<hydra.langs.cypher.features.UpdatingFeatures> updating;
  
  public CypherFeatures (hydra.util.Opt<hydra.langs.cypher.features.AggregateFeatures> aggregate, hydra.util.Opt<hydra.langs.cypher.features.ArithmeticFeatures> arithmetic, hydra.util.Opt<hydra.langs.cypher.features.AtomFeatures> atom, hydra.util.Opt<hydra.langs.cypher.features.ComparisonFeatures> comparison, hydra.util.Opt<hydra.langs.cypher.features.DeleteFeatures> delete, hydra.util.Opt<hydra.langs.cypher.features.ElementFeatures> element, hydra.util.Opt<hydra.langs.cypher.features.LogicalFeatures> logical, hydra.util.Opt<hydra.langs.cypher.features.MapFeatures> map, hydra.util.Opt<hydra.langs.cypher.features.MatchFeatures> match, hydra.util.Opt<hydra.langs.cypher.features.MergeFeatures> merge, hydra.util.Opt<hydra.langs.cypher.features.NodePatternFeatures> nodePattern, hydra.util.Opt<hydra.langs.cypher.features.NullFeatures> null_, hydra.util.Opt<hydra.langs.cypher.features.NumericFeatures> numeric, hydra.util.Opt<hydra.langs.cypher.features.PathFeatures> path, hydra.util.Opt<hydra.langs.cypher.features.ProcedureCallFeatures> procedureCall, hydra.util.Opt<hydra.langs.cypher.features.ProjectionFeatures> projection, hydra.util.Opt<hydra.langs.cypher.features.RandomnessFeatures> randomness, hydra.util.Opt<hydra.langs.cypher.features.RangeLiteralFeatures> rangeLiteral, hydra.util.Opt<hydra.langs.cypher.features.ReadingFeatures> reading, hydra.util.Opt<hydra.langs.cypher.features.RelationshipDirectionFeatures> relationshipDirection, hydra.util.Opt<hydra.langs.cypher.features.RelationshipPatternFeatures> relationshipPattern, hydra.util.Opt<hydra.langs.cypher.features.RemoveFeatures> remove, hydra.util.Opt<hydra.langs.cypher.features.SchemaFeatures> schema, hydra.util.Opt<hydra.langs.cypher.features.SetFeatures> set, hydra.util.Opt<hydra.langs.cypher.features.StringFeatures> string, hydra.util.Opt<hydra.langs.cypher.features.UpdatingFeatures> updating) {
    if (aggregate == null) {
      throw new IllegalArgumentException("null value for 'aggregate' argument");
    }
    if (arithmetic == null) {
      throw new IllegalArgumentException("null value for 'arithmetic' argument");
    }
    if (atom == null) {
      throw new IllegalArgumentException("null value for 'atom' argument");
    }
    if (comparison == null) {
      throw new IllegalArgumentException("null value for 'comparison' argument");
    }
    if (delete == null) {
      throw new IllegalArgumentException("null value for 'delete' argument");
    }
    if (element == null) {
      throw new IllegalArgumentException("null value for 'element' argument");
    }
    if (logical == null) {
      throw new IllegalArgumentException("null value for 'logical' argument");
    }
    if (map == null) {
      throw new IllegalArgumentException("null value for 'map' argument");
    }
    if (match == null) {
      throw new IllegalArgumentException("null value for 'match' argument");
    }
    if (merge == null) {
      throw new IllegalArgumentException("null value for 'merge' argument");
    }
    if (nodePattern == null) {
      throw new IllegalArgumentException("null value for 'nodePattern' argument");
    }
    if (null_ == null) {
      throw new IllegalArgumentException("null value for 'null' argument");
    }
    if (numeric == null) {
      throw new IllegalArgumentException("null value for 'numeric' argument");
    }
    if (path == null) {
      throw new IllegalArgumentException("null value for 'path' argument");
    }
    if (procedureCall == null) {
      throw new IllegalArgumentException("null value for 'procedureCall' argument");
    }
    if (projection == null) {
      throw new IllegalArgumentException("null value for 'projection' argument");
    }
    if (randomness == null) {
      throw new IllegalArgumentException("null value for 'randomness' argument");
    }
    if (rangeLiteral == null) {
      throw new IllegalArgumentException("null value for 'rangeLiteral' argument");
    }
    if (reading == null) {
      throw new IllegalArgumentException("null value for 'reading' argument");
    }
    if (relationshipDirection == null) {
      throw new IllegalArgumentException("null value for 'relationshipDirection' argument");
    }
    if (relationshipPattern == null) {
      throw new IllegalArgumentException("null value for 'relationshipPattern' argument");
    }
    if (remove == null) {
      throw new IllegalArgumentException("null value for 'remove' argument");
    }
    if (schema == null) {
      throw new IllegalArgumentException("null value for 'schema' argument");
    }
    if (set == null) {
      throw new IllegalArgumentException("null value for 'set' argument");
    }
    if (string == null) {
      throw new IllegalArgumentException("null value for 'string' argument");
    }
    if (updating == null) {
      throw new IllegalArgumentException("null value for 'updating' argument");
    }
    this.aggregate = aggregate;
    this.arithmetic = arithmetic;
    this.atom = atom;
    this.comparison = comparison;
    this.delete = delete;
    this.element = element;
    this.logical = logical;
    this.map = map;
    this.match = match;
    this.merge = merge;
    this.nodePattern = nodePattern;
    this.null_ = null_;
    this.numeric = numeric;
    this.path = path;
    this.procedureCall = procedureCall;
    this.projection = projection;
    this.randomness = randomness;
    this.rangeLiteral = rangeLiteral;
    this.reading = reading;
    this.relationshipDirection = relationshipDirection;
    this.relationshipPattern = relationshipPattern;
    this.remove = remove;
    this.schema = schema;
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
    return aggregate.equals(o.aggregate) && arithmetic.equals(o.arithmetic) && atom.equals(o.atom) && comparison.equals(o.comparison) && delete.equals(o.delete) && element.equals(o.element) && logical.equals(o.logical) && map.equals(o.map) && match.equals(o.match) && merge.equals(o.merge) && nodePattern.equals(o.nodePattern) && null_.equals(o.null_) && numeric.equals(o.numeric) && path.equals(o.path) && procedureCall.equals(o.procedureCall) && projection.equals(o.projection) && randomness.equals(o.randomness) && rangeLiteral.equals(o.rangeLiteral) && reading.equals(o.reading) && relationshipDirection.equals(o.relationshipDirection) && relationshipPattern.equals(o.relationshipPattern) && remove.equals(o.remove) && schema.equals(o.schema) && set.equals(o.set) && string.equals(o.string) && updating.equals(o.updating);
  }
  
  @Override
  public int hashCode() {
    return 2 * aggregate.hashCode() + 3 * arithmetic.hashCode() + 5 * atom.hashCode() + 7 * comparison.hashCode() + 11 * delete.hashCode() + 13 * element.hashCode() + 17 * logical.hashCode() + 19 * map.hashCode() + 23 * match.hashCode() + 29 * merge.hashCode() + 31 * nodePattern.hashCode() + 37 * null_.hashCode() + 41 * numeric.hashCode() + 43 * path.hashCode() + 47 * procedureCall.hashCode() + 53 * projection.hashCode() + 59 * randomness.hashCode() + 61 * rangeLiteral.hashCode() + 67 * reading.hashCode() + 71 * relationshipDirection.hashCode() + 2 * relationshipPattern.hashCode() + 3 * remove.hashCode() + 5 * schema.hashCode() + 7 * set.hashCode() + 11 * string.hashCode() + 13 * updating.hashCode();
  }
  
  public CypherFeatures withAggregate(hydra.util.Opt<hydra.langs.cypher.features.AggregateFeatures> aggregate) {
    if (aggregate == null) {
      throw new IllegalArgumentException("null value for 'aggregate' argument");
    }
    return new CypherFeatures(aggregate, arithmetic, atom, comparison, delete, element, logical, map, match, merge, nodePattern, null_, numeric, path, procedureCall, projection, randomness, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, schema, set, string, updating);
  }
  
  public CypherFeatures withArithmetic(hydra.util.Opt<hydra.langs.cypher.features.ArithmeticFeatures> arithmetic) {
    if (arithmetic == null) {
      throw new IllegalArgumentException("null value for 'arithmetic' argument");
    }
    return new CypherFeatures(aggregate, arithmetic, atom, comparison, delete, element, logical, map, match, merge, nodePattern, null_, numeric, path, procedureCall, projection, randomness, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, schema, set, string, updating);
  }
  
  public CypherFeatures withAtom(hydra.util.Opt<hydra.langs.cypher.features.AtomFeatures> atom) {
    if (atom == null) {
      throw new IllegalArgumentException("null value for 'atom' argument");
    }
    return new CypherFeatures(aggregate, arithmetic, atom, comparison, delete, element, logical, map, match, merge, nodePattern, null_, numeric, path, procedureCall, projection, randomness, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, schema, set, string, updating);
  }
  
  public CypherFeatures withComparison(hydra.util.Opt<hydra.langs.cypher.features.ComparisonFeatures> comparison) {
    if (comparison == null) {
      throw new IllegalArgumentException("null value for 'comparison' argument");
    }
    return new CypherFeatures(aggregate, arithmetic, atom, comparison, delete, element, logical, map, match, merge, nodePattern, null_, numeric, path, procedureCall, projection, randomness, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, schema, set, string, updating);
  }
  
  public CypherFeatures withDelete(hydra.util.Opt<hydra.langs.cypher.features.DeleteFeatures> delete) {
    if (delete == null) {
      throw new IllegalArgumentException("null value for 'delete' argument");
    }
    return new CypherFeatures(aggregate, arithmetic, atom, comparison, delete, element, logical, map, match, merge, nodePattern, null_, numeric, path, procedureCall, projection, randomness, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, schema, set, string, updating);
  }
  
  public CypherFeatures withElement(hydra.util.Opt<hydra.langs.cypher.features.ElementFeatures> element) {
    if (element == null) {
      throw new IllegalArgumentException("null value for 'element' argument");
    }
    return new CypherFeatures(aggregate, arithmetic, atom, comparison, delete, element, logical, map, match, merge, nodePattern, null_, numeric, path, procedureCall, projection, randomness, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, schema, set, string, updating);
  }
  
  public CypherFeatures withLogical(hydra.util.Opt<hydra.langs.cypher.features.LogicalFeatures> logical) {
    if (logical == null) {
      throw new IllegalArgumentException("null value for 'logical' argument");
    }
    return new CypherFeatures(aggregate, arithmetic, atom, comparison, delete, element, logical, map, match, merge, nodePattern, null_, numeric, path, procedureCall, projection, randomness, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, schema, set, string, updating);
  }
  
  public CypherFeatures withMap(hydra.util.Opt<hydra.langs.cypher.features.MapFeatures> map) {
    if (map == null) {
      throw new IllegalArgumentException("null value for 'map' argument");
    }
    return new CypherFeatures(aggregate, arithmetic, atom, comparison, delete, element, logical, map, match, merge, nodePattern, null_, numeric, path, procedureCall, projection, randomness, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, schema, set, string, updating);
  }
  
  public CypherFeatures withMatch(hydra.util.Opt<hydra.langs.cypher.features.MatchFeatures> match) {
    if (match == null) {
      throw new IllegalArgumentException("null value for 'match' argument");
    }
    return new CypherFeatures(aggregate, arithmetic, atom, comparison, delete, element, logical, map, match, merge, nodePattern, null_, numeric, path, procedureCall, projection, randomness, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, schema, set, string, updating);
  }
  
  public CypherFeatures withMerge(hydra.util.Opt<hydra.langs.cypher.features.MergeFeatures> merge) {
    if (merge == null) {
      throw new IllegalArgumentException("null value for 'merge' argument");
    }
    return new CypherFeatures(aggregate, arithmetic, atom, comparison, delete, element, logical, map, match, merge, nodePattern, null_, numeric, path, procedureCall, projection, randomness, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, schema, set, string, updating);
  }
  
  public CypherFeatures withNodePattern(hydra.util.Opt<hydra.langs.cypher.features.NodePatternFeatures> nodePattern) {
    if (nodePattern == null) {
      throw new IllegalArgumentException("null value for 'nodePattern' argument");
    }
    return new CypherFeatures(aggregate, arithmetic, atom, comparison, delete, element, logical, map, match, merge, nodePattern, null_, numeric, path, procedureCall, projection, randomness, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, schema, set, string, updating);
  }
  
  public CypherFeatures withNull(hydra.util.Opt<hydra.langs.cypher.features.NullFeatures> null_) {
    if (null_ == null) {
      throw new IllegalArgumentException("null value for 'null' argument");
    }
    return new CypherFeatures(aggregate, arithmetic, atom, comparison, delete, element, logical, map, match, merge, nodePattern, null_, numeric, path, procedureCall, projection, randomness, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, schema, set, string, updating);
  }
  
  public CypherFeatures withNumeric(hydra.util.Opt<hydra.langs.cypher.features.NumericFeatures> numeric) {
    if (numeric == null) {
      throw new IllegalArgumentException("null value for 'numeric' argument");
    }
    return new CypherFeatures(aggregate, arithmetic, atom, comparison, delete, element, logical, map, match, merge, nodePattern, null_, numeric, path, procedureCall, projection, randomness, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, schema, set, string, updating);
  }
  
  public CypherFeatures withPath(hydra.util.Opt<hydra.langs.cypher.features.PathFeatures> path) {
    if (path == null) {
      throw new IllegalArgumentException("null value for 'path' argument");
    }
    return new CypherFeatures(aggregate, arithmetic, atom, comparison, delete, element, logical, map, match, merge, nodePattern, null_, numeric, path, procedureCall, projection, randomness, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, schema, set, string, updating);
  }
  
  public CypherFeatures withProcedureCall(hydra.util.Opt<hydra.langs.cypher.features.ProcedureCallFeatures> procedureCall) {
    if (procedureCall == null) {
      throw new IllegalArgumentException("null value for 'procedureCall' argument");
    }
    return new CypherFeatures(aggregate, arithmetic, atom, comparison, delete, element, logical, map, match, merge, nodePattern, null_, numeric, path, procedureCall, projection, randomness, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, schema, set, string, updating);
  }
  
  public CypherFeatures withProjection(hydra.util.Opt<hydra.langs.cypher.features.ProjectionFeatures> projection) {
    if (projection == null) {
      throw new IllegalArgumentException("null value for 'projection' argument");
    }
    return new CypherFeatures(aggregate, arithmetic, atom, comparison, delete, element, logical, map, match, merge, nodePattern, null_, numeric, path, procedureCall, projection, randomness, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, schema, set, string, updating);
  }
  
  public CypherFeatures withRandomness(hydra.util.Opt<hydra.langs.cypher.features.RandomnessFeatures> randomness) {
    if (randomness == null) {
      throw new IllegalArgumentException("null value for 'randomness' argument");
    }
    return new CypherFeatures(aggregate, arithmetic, atom, comparison, delete, element, logical, map, match, merge, nodePattern, null_, numeric, path, procedureCall, projection, randomness, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, schema, set, string, updating);
  }
  
  public CypherFeatures withRangeLiteral(hydra.util.Opt<hydra.langs.cypher.features.RangeLiteralFeatures> rangeLiteral) {
    if (rangeLiteral == null) {
      throw new IllegalArgumentException("null value for 'rangeLiteral' argument");
    }
    return new CypherFeatures(aggregate, arithmetic, atom, comparison, delete, element, logical, map, match, merge, nodePattern, null_, numeric, path, procedureCall, projection, randomness, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, schema, set, string, updating);
  }
  
  public CypherFeatures withReading(hydra.util.Opt<hydra.langs.cypher.features.ReadingFeatures> reading) {
    if (reading == null) {
      throw new IllegalArgumentException("null value for 'reading' argument");
    }
    return new CypherFeatures(aggregate, arithmetic, atom, comparison, delete, element, logical, map, match, merge, nodePattern, null_, numeric, path, procedureCall, projection, randomness, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, schema, set, string, updating);
  }
  
  public CypherFeatures withRelationshipDirection(hydra.util.Opt<hydra.langs.cypher.features.RelationshipDirectionFeatures> relationshipDirection) {
    if (relationshipDirection == null) {
      throw new IllegalArgumentException("null value for 'relationshipDirection' argument");
    }
    return new CypherFeatures(aggregate, arithmetic, atom, comparison, delete, element, logical, map, match, merge, nodePattern, null_, numeric, path, procedureCall, projection, randomness, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, schema, set, string, updating);
  }
  
  public CypherFeatures withRelationshipPattern(hydra.util.Opt<hydra.langs.cypher.features.RelationshipPatternFeatures> relationshipPattern) {
    if (relationshipPattern == null) {
      throw new IllegalArgumentException("null value for 'relationshipPattern' argument");
    }
    return new CypherFeatures(aggregate, arithmetic, atom, comparison, delete, element, logical, map, match, merge, nodePattern, null_, numeric, path, procedureCall, projection, randomness, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, schema, set, string, updating);
  }
  
  public CypherFeatures withRemove(hydra.util.Opt<hydra.langs.cypher.features.RemoveFeatures> remove) {
    if (remove == null) {
      throw new IllegalArgumentException("null value for 'remove' argument");
    }
    return new CypherFeatures(aggregate, arithmetic, atom, comparison, delete, element, logical, map, match, merge, nodePattern, null_, numeric, path, procedureCall, projection, randomness, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, schema, set, string, updating);
  }
  
  public CypherFeatures withSchema(hydra.util.Opt<hydra.langs.cypher.features.SchemaFeatures> schema) {
    if (schema == null) {
      throw new IllegalArgumentException("null value for 'schema' argument");
    }
    return new CypherFeatures(aggregate, arithmetic, atom, comparison, delete, element, logical, map, match, merge, nodePattern, null_, numeric, path, procedureCall, projection, randomness, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, schema, set, string, updating);
  }
  
  public CypherFeatures withSet(hydra.util.Opt<hydra.langs.cypher.features.SetFeatures> set) {
    if (set == null) {
      throw new IllegalArgumentException("null value for 'set' argument");
    }
    return new CypherFeatures(aggregate, arithmetic, atom, comparison, delete, element, logical, map, match, merge, nodePattern, null_, numeric, path, procedureCall, projection, randomness, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, schema, set, string, updating);
  }
  
  public CypherFeatures withString(hydra.util.Opt<hydra.langs.cypher.features.StringFeatures> string) {
    if (string == null) {
      throw new IllegalArgumentException("null value for 'string' argument");
    }
    return new CypherFeatures(aggregate, arithmetic, atom, comparison, delete, element, logical, map, match, merge, nodePattern, null_, numeric, path, procedureCall, projection, randomness, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, schema, set, string, updating);
  }
  
  public CypherFeatures withUpdating(hydra.util.Opt<hydra.langs.cypher.features.UpdatingFeatures> updating) {
    if (updating == null) {
      throw new IllegalArgumentException("null value for 'updating' argument");
    }
    return new CypherFeatures(aggregate, arithmetic, atom, comparison, delete, element, logical, map, match, merge, nodePattern, null_, numeric, path, procedureCall, projection, randomness, rangeLiteral, reading, relationshipDirection, relationshipPattern, remove, schema, set, string, updating);
  }
}