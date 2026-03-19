// Note: this is an automatically generated file. Do not edit.

package hydra.ext.lisp.syntax;

import java.io.Serializable;

/**
 * An import/require declaration. Serializes as (:require [name ...]) in Clojure, (require 'name) in Emacs Lisp, (:use :name) or (:import-from :name ...) in Common Lisp, (import (name)) in Scheme
 */
public class ImportDeclaration implements Serializable, Comparable<ImportDeclaration> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.lisp.syntax.ImportDeclaration");

  public static final hydra.core.Name MODULE = new hydra.core.Name("module");

  public static final hydra.core.Name SPEC = new hydra.core.Name("spec");

  /**
   * The module being imported
   */
  public final hydra.ext.lisp.syntax.NamespaceName module;

  /**
   * Import specification
   */
  public final hydra.ext.lisp.syntax.ImportSpec spec;

  public ImportDeclaration (hydra.ext.lisp.syntax.NamespaceName module, hydra.ext.lisp.syntax.ImportSpec spec) {
    this.module = module;
    this.spec = spec;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ImportDeclaration)) {
      return false;
    }
    ImportDeclaration o = (ImportDeclaration) other;
    return java.util.Objects.equals(
      this.module,
      o.module) && java.util.Objects.equals(
      this.spec,
      o.spec);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(module) + 3 * java.util.Objects.hashCode(spec);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ImportDeclaration other) {
    int cmp = 0;
    cmp = ((Comparable) module).compareTo(other.module);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) spec).compareTo(other.spec);
  }

  public ImportDeclaration withModule(hydra.ext.lisp.syntax.NamespaceName module) {
    return new ImportDeclaration(module, spec);
  }

  public ImportDeclaration withSpec(hydra.ext.lisp.syntax.ImportSpec spec) {
    return new ImportDeclaration(module, spec);
  }
}
