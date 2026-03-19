// Note: this is an automatically generated file. Do not edit.

package hydra.ext.lisp.syntax;

import java.io.Serializable;

/**
 * A wildcard pattern that matches any value
 */
public class WildcardPattern implements Serializable, Comparable<WildcardPattern> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.lisp.syntax.WildcardPattern");

  public WildcardPattern () {

  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof WildcardPattern)) {
      return false;
    }
    WildcardPattern o = (WildcardPattern) other;
    return true;
  }

  @Override
  public int hashCode() {
    return 0;
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(WildcardPattern other) {
    return 0;
  }
}
