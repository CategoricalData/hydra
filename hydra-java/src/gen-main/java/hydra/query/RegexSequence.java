// Note: this is an automatically generated file. Do not edit.

package hydra.query;

import java.io.Serializable;

/**
 * A path with a regex quantifier
 */
public class RegexSequence implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/query.RegexSequence");
  
  public final hydra.query.Path path;
  
  public final hydra.query.RegexQuantifier quantifier;
  
  public RegexSequence (hydra.query.Path path, hydra.query.RegexQuantifier quantifier) {
    if (path == null) {
      throw new IllegalArgumentException("null value for 'path' argument");
    }
    if (quantifier == null) {
      throw new IllegalArgumentException("null value for 'quantifier' argument");
    }
    this.path = path;
    this.quantifier = quantifier;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RegexSequence)) {
      return false;
    }
    RegexSequence o = (RegexSequence) (other);
    return path.equals(o.path) && quantifier.equals(o.quantifier);
  }
  
  @Override
  public int hashCode() {
    return 2 * path.hashCode() + 3 * quantifier.hashCode();
  }
  
  public RegexSequence withPath(hydra.query.Path path) {
    if (path == null) {
      throw new IllegalArgumentException("null value for 'path' argument");
    }
    return new RegexSequence(path, quantifier);
  }
  
  public RegexSequence withQuantifier(hydra.query.RegexQuantifier quantifier) {
    if (quantifier == null) {
      throw new IllegalArgumentException("null value for 'quantifier' argument");
    }
    return new RegexSequence(path, quantifier);
  }
}