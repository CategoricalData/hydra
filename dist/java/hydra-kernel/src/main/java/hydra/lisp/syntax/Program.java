// Note: this is an automatically generated file. Do not edit.

package hydra.lisp.syntax;

import java.io.Serializable;

/**
 * A Lisp program, consisting of a sequence of top-level forms
 */
public class Program implements Serializable, Comparable<Program> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.lisp.syntax.Program");

  public static final hydra.core.Name DIALECT = new hydra.core.Name("dialect");

  public static final hydra.core.Name MODULE = new hydra.core.Name("module");

  public static final hydra.core.Name IMPORTS = new hydra.core.Name("imports");

  public static final hydra.core.Name EXPORTS = new hydra.core.Name("exports");

  public static final hydra.core.Name FORMS = new hydra.core.Name("forms");

  /**
   * The target Lisp dialect
   */
  public final hydra.lisp.syntax.Dialect dialect;

  /**
   * Optional module/namespace declaration
   */
  public final hydra.util.Maybe<hydra.lisp.syntax.ModuleDeclaration> module;

  /**
   * Import/require declarations
   */
  public final java.util.List<hydra.lisp.syntax.ImportDeclaration> imports;

  /**
   * Export/provide declarations
   */
  public final java.util.List<hydra.lisp.syntax.ExportDeclaration> exports;

  /**
   * The top-level forms in the program
   */
  public final java.util.List<hydra.lisp.syntax.TopLevelFormWithComments> forms;

  public Program (hydra.lisp.syntax.Dialect dialect, hydra.util.Maybe<hydra.lisp.syntax.ModuleDeclaration> module, java.util.List<hydra.lisp.syntax.ImportDeclaration> imports, java.util.List<hydra.lisp.syntax.ExportDeclaration> exports, java.util.List<hydra.lisp.syntax.TopLevelFormWithComments> forms) {
    this.dialect = dialect;
    this.module = module;
    this.imports = imports;
    this.exports = exports;
    this.forms = forms;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Program)) {
      return false;
    }
    Program o = (Program) other;
    return java.util.Objects.equals(
      this.dialect,
      o.dialect) && java.util.Objects.equals(
      this.module,
      o.module) && java.util.Objects.equals(
      this.imports,
      o.imports) && java.util.Objects.equals(
      this.exports,
      o.exports) && java.util.Objects.equals(
      this.forms,
      o.forms);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(dialect) + 3 * java.util.Objects.hashCode(module) + 5 * java.util.Objects.hashCode(imports) + 7 * java.util.Objects.hashCode(exports) + 11 * java.util.Objects.hashCode(forms);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Program other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      dialect,
      other.dialect);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      module,
      other.module);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      imports,
      other.imports);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      exports,
      other.exports);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      forms,
      other.forms);
  }

  public Program withDialect(hydra.lisp.syntax.Dialect dialect) {
    return new Program(dialect, module, imports, exports, forms);
  }

  public Program withModule(hydra.util.Maybe<hydra.lisp.syntax.ModuleDeclaration> module) {
    return new Program(dialect, module, imports, exports, forms);
  }

  public Program withImports(java.util.List<hydra.lisp.syntax.ImportDeclaration> imports) {
    return new Program(dialect, module, imports, exports, forms);
  }

  public Program withExports(java.util.List<hydra.lisp.syntax.ExportDeclaration> exports) {
    return new Program(dialect, module, imports, exports, forms);
  }

  public Program withForms(java.util.List<hydra.lisp.syntax.TopLevelFormWithComments> forms) {
    return new Program(dialect, module, imports, exports, forms);
  }
}
