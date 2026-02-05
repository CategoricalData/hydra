// Note: this is an automatically generated file. Do not edit.

package hydra.query;

import java.io.Serializable;

/**
 * A path with a regex quantifier
 */
public class RegexSequence implements Serializable, Comparable<RegexSequence> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.query.RegexSequence");
  
  public static final hydra.core.Name FIELD_NAME_PATH = new hydra.core.Name("path");
  
  public static final hydra.core.Name FIELD_NAME_QUANTIFIER = new hydra.core.Name("quantifier");
  
  /**
   * The path to which the quantifier applies
   */
  public final hydra.query.Path path;
  
  /**
   * The quantifier
   */
  public final hydra.query.RegexQuantifier quantifier;
  
  public RegexSequence (hydra.query.Path path, hydra.query.RegexQuantifier quantifier) {
    this.path = path;
    this.quantifier = quantifier;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RegexSequence)) {
      return false;
    }
    RegexSequence o = (RegexSequence) (other);
    return java.util.Objects.equals(
      this.path,
      o.path) && java.util.Objects.equals(
      this.quantifier,
      o.quantifier);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(path) + 3 * java.util.Objects.hashCode(quantifier);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(RegexSequence other) {
    int cmp = 0;
    cmp = ((Comparable) (path)).compareTo(other.path);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) (quantifier)).compareTo(other.quantifier);
  }
  
  public RegexSequence withPath(hydra.query.Path path) {
    return new RegexSequence(path, quantifier);
  }
  
  public RegexSequence withQuantifier(hydra.query.RegexQuantifier quantifier) {
    return new RegexSequence(path, quantifier);
  }
}
