// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class EdgeTypePattern implements Serializable, Comparable<EdgeTypePattern> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.EdgeTypePattern");

  public static final hydra.core.Name KIND_AND_SYNONYM = new hydra.core.Name("kindAndSynonym");

  public static final hydra.core.Name PATTERN_TYPE = new hydra.core.Name("patternType");

  public final hydra.util.Maybe<openGql.grammar.EdgeKindAndSynonym> kindAndSynonym;

  public final openGql.grammar.EdgeTypePatternType patternType;

  public EdgeTypePattern (hydra.util.Maybe<openGql.grammar.EdgeKindAndSynonym> kindAndSynonym, openGql.grammar.EdgeTypePatternType patternType) {
    this.kindAndSynonym = kindAndSynonym;
    this.patternType = patternType;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EdgeTypePattern)) {
      return false;
    }
    EdgeTypePattern o = (EdgeTypePattern) other;
    return java.util.Objects.equals(
      this.kindAndSynonym,
      o.kindAndSynonym) && java.util.Objects.equals(
      this.patternType,
      o.patternType);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(kindAndSynonym) + 3 * java.util.Objects.hashCode(patternType);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(EdgeTypePattern other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      kindAndSynonym,
      other.kindAndSynonym);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      patternType,
      other.patternType);
  }

  public EdgeTypePattern withKindAndSynonym(hydra.util.Maybe<openGql.grammar.EdgeKindAndSynonym> kindAndSynonym) {
    return new EdgeTypePattern(kindAndSynonym, patternType);
  }

  public EdgeTypePattern withPatternType(openGql.grammar.EdgeTypePatternType patternType) {
    return new EdgeTypePattern(kindAndSynonym, patternType);
  }
}
