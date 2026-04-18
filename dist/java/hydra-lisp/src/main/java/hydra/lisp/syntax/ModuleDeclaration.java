// Note: this is an automatically generated file. Do not edit.

package hydra.lisp.syntax;

import java.io.Serializable;

/**
 * A module/namespace declaration. Serializes as (ns name ...) in Clojure, (provide 'name) in Emacs Lisp, (defpackage :name ... ) (in-package :name) in Common Lisp, (define-library (name) ...) in Scheme
 */
public class ModuleDeclaration implements Serializable, Comparable<ModuleDeclaration> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.lisp.syntax.ModuleDeclaration");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name DOC = new hydra.core.Name("doc");

  /**
   * The module/namespace name
   */
  public final hydra.lisp.syntax.NamespaceName name;

  /**
   * Optional module documentation
   */
  public final hydra.util.Maybe<hydra.lisp.syntax.Docstring> doc;

  public ModuleDeclaration (hydra.lisp.syntax.NamespaceName name, hydra.util.Maybe<hydra.lisp.syntax.Docstring> doc) {
    this.name = name;
    this.doc = doc;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ModuleDeclaration)) {
      return false;
    }
    ModuleDeclaration o = (ModuleDeclaration) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.doc,
      o.doc);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(doc);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ModuleDeclaration other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      name,
      other.name);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      doc,
      other.doc);
  }

  public ModuleDeclaration withName(hydra.lisp.syntax.NamespaceName name) {
    return new ModuleDeclaration(name, doc);
  }

  public ModuleDeclaration withDoc(hydra.util.Maybe<hydra.lisp.syntax.Docstring> doc) {
    return new ModuleDeclaration(name, doc);
  }
}
