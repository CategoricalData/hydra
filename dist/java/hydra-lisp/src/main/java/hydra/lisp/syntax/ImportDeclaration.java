// Note: this is an automatically generated file. Do not edit.

package hydra.lisp.syntax;

import java.io.Serializable;

/**
 * An import/require declaration. Serializes as (:require [name ...]) in Clojure, (require 'name) in Emacs Lisp, (:use :name) or (:import-from :name ...) in Common Lisp, (import (name)) in Scheme
 */
public class ImportDeclaration implements Serializable, Comparable<ImportDeclaration> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.lisp.syntax.ImportDeclaration");

  public static final hydra.core.Name MODULE = new hydra.core.Name("module");

  public static final hydra.core.Name SPEC = new hydra.core.Name("spec");

  /**
   * The module being imported
   */
  public final hydra.lisp.syntax.NamespaceName module;

  /**
   * Import specification
   */
  public final hydra.lisp.syntax.ImportSpec spec;

  public ImportDeclaration (hydra.lisp.syntax.NamespaceName module, hydra.lisp.syntax.ImportSpec spec) {
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
    cmp = hydra.util.Comparing.compare(
      module,
      other.module);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      spec,
      other.spec);
  }

  public ImportDeclaration withModule(hydra.lisp.syntax.NamespaceName module) {
    return new ImportDeclaration(module, spec);
  }

  public ImportDeclaration withSpec(hydra.lisp.syntax.ImportSpec spec) {
    return new ImportDeclaration(module, spec);
  }
}
