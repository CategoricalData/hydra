// Note: this is an automatically generated file. Do not edit.

package hydra.lisp.syntax;

import java.io.Serializable;

/**
 * A namespace-qualified symbol. Serializes as ns/name in Clojure, pkg:name or pkg::name in Common Lisp
 */
public class QualifiedSymbol implements Serializable, Comparable<QualifiedSymbol> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.lisp.syntax.QualifiedSymbol");

  public static final hydra.core.Name NAMESPACE = new hydra.core.Name("namespace");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  /**
   * The namespace or package
   */
  public final String namespace;

  /**
   * The local name
   */
  public final String name;

  public QualifiedSymbol (String namespace, String name) {
    this.namespace = namespace;
    this.name = name;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof QualifiedSymbol)) {
      return false;
    }
    QualifiedSymbol o = (QualifiedSymbol) other;
    return java.util.Objects.equals(
      this.namespace,
      o.namespace) && java.util.Objects.equals(
      this.name,
      o.name);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(namespace) + 3 * java.util.Objects.hashCode(name);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(QualifiedSymbol other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      namespace,
      other.namespace);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      name,
      other.name);
  }

  public QualifiedSymbol withNamespace(String namespace) {
    return new QualifiedSymbol(namespace, name);
  }

  public QualifiedSymbol withName(String name) {
    return new QualifiedSymbol(namespace, name);
  }
}
