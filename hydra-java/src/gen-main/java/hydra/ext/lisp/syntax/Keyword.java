// Note: this is an automatically generated file. Do not edit.

package hydra.ext.lisp.syntax;

import java.io.Serializable;

/**
 * A keyword (self-evaluating symbol). Serializes as :name in Clojure, Emacs Lisp, and Common Lisp
 */
public class Keyword implements Serializable, Comparable<Keyword> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.lisp.syntax.Keyword");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name NAMESPACE = new hydra.core.Name("namespace");

  /**
   * The keyword name (without the leading colon)
   */
  public final String name;

  /**
   * Optional namespace (e.g. my.ns/foo in Clojure)
   */
  public final hydra.util.Maybe<String> namespace;

  public Keyword (String name, hydra.util.Maybe<String> namespace) {
    this.name = name;
    this.namespace = namespace;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Keyword)) {
      return false;
    }
    Keyword o = (Keyword) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.namespace,
      o.namespace);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(namespace);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Keyword other) {
    int cmp = 0;
    cmp = ((Comparable) name).compareTo(other.name);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) namespace).compareTo(other.namespace);
  }

  public Keyword withName(String name) {
    return new Keyword(name, namespace);
  }

  public Keyword withNamespace(hydra.util.Maybe<String> namespace) {
    return new Keyword(name, namespace);
  }
}
