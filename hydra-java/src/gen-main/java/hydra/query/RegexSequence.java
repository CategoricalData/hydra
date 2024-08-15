// Note: this is an automatically generated file. Do not edit.

package hydra.query;

import java.io.Serializable;

/**
 * A path with a regex quantifier
 */
public class RegexSequence implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/query.RegexSequence");
  
  public static final hydra.core.Name FIELD_NAME_PATH = new hydra.core.Name("path");
  
  public static final hydra.core.Name FIELD_NAME_QUANTIFIER = new hydra.core.Name("quantifier");
  
  public final hydra.query.Path path;
  
  public final hydra.query.RegexQuantifier quantifier;
  
  public RegexSequence (hydra.query.Path path, hydra.query.RegexQuantifier quantifier) {
    java.util.Objects.requireNonNull((path));
    java.util.Objects.requireNonNull((quantifier));
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
    java.util.Objects.requireNonNull((path));
    return new RegexSequence(path, quantifier);
  }
  
  public RegexSequence withQuantifier(hydra.query.RegexQuantifier quantifier) {
    java.util.Objects.requireNonNull((quantifier));
    return new RegexSequence(path, quantifier);
  }
}