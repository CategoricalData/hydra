// Note: this is an automatically generated file. Do not edit.

package hydra.ext.lisp.syntax;

import java.io.Serializable;

/**
 * A Lisp program, consisting of a sequence of top-level forms
 */
public class Program implements Serializable, Comparable<Program> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.lisp.syntax.Program");

  public static final hydra.core.Name DIALECT = new hydra.core.Name("dialect");

  public static final hydra.core.Name MODULE = new hydra.core.Name("module");

  public static final hydra.core.Name IMPORTS = new hydra.core.Name("imports");

  public static final hydra.core.Name EXPORTS = new hydra.core.Name("exports");

  public static final hydra.core.Name FORMS = new hydra.core.Name("forms");

  /**
   * The target Lisp dialect
   */
  public final hydra.ext.lisp.syntax.Dialect dialect;

  /**
   * Optional module/namespace declaration
   */
  public final hydra.util.Maybe<hydra.ext.lisp.syntax.ModuleDeclaration> module;

  /**
   * Import/require declarations
   */
  public final hydra.util.ConsList<hydra.ext.lisp.syntax.ImportDeclaration> imports;

  /**
   * Export/provide declarations
   */
  public final hydra.util.ConsList<hydra.ext.lisp.syntax.ExportDeclaration> exports;

  /**
   * The top-level forms in the program
   */
  public final hydra.util.ConsList<hydra.ext.lisp.syntax.TopLevelFormWithComments> forms;

  public Program (hydra.ext.lisp.syntax.Dialect dialect, hydra.util.Maybe<hydra.ext.lisp.syntax.ModuleDeclaration> module, hydra.util.ConsList<hydra.ext.lisp.syntax.ImportDeclaration> imports, hydra.util.ConsList<hydra.ext.lisp.syntax.ExportDeclaration> exports, hydra.util.ConsList<hydra.ext.lisp.syntax.TopLevelFormWithComments> forms) {
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
    cmp = ((Comparable) dialect).compareTo(other.dialect);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) module).compareTo(other.module);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) imports).compareTo(other.imports);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) exports).compareTo(other.exports);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) forms).compareTo(other.forms);
  }

  public Program withDialect(hydra.ext.lisp.syntax.Dialect dialect) {
    return new Program(dialect, module, imports, exports, forms);
  }

  public Program withModule(hydra.util.Maybe<hydra.ext.lisp.syntax.ModuleDeclaration> module) {
    return new Program(dialect, module, imports, exports, forms);
  }

  public Program withImports(hydra.util.ConsList<hydra.ext.lisp.syntax.ImportDeclaration> imports) {
    return new Program(dialect, module, imports, exports, forms);
  }

  public Program withExports(hydra.util.ConsList<hydra.ext.lisp.syntax.ExportDeclaration> exports) {
    return new Program(dialect, module, imports, exports, forms);
  }

  public Program withForms(hydra.util.ConsList<hydra.ext.lisp.syntax.TopLevelFormWithComments> forms) {
    return new Program(dialect, module, imports, exports, forms);
  }
}
