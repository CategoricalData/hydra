// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * A JavaScript program (module)
 */
public class Program implements Serializable, Comparable<Program> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.Program");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  public static final hydra.core.Name SOURCE_TYPE = new hydra.core.Name("sourceType");

  /**
   * The module items
   */
  public final java.util.List<hydra.javaScript.syntax.ModuleItem> body;

  /**
   * Whether this is a module or script
   */
  public final hydra.javaScript.syntax.SourceType sourceType;

  public Program (java.util.List<hydra.javaScript.syntax.ModuleItem> body, hydra.javaScript.syntax.SourceType sourceType) {
    this.body = body;
    this.sourceType = sourceType;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Program)) {
      return false;
    }
    Program o = (Program) other;
    return java.util.Objects.equals(
      this.body,
      o.body) && java.util.Objects.equals(
      this.sourceType,
      o.sourceType);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(body) + 3 * java.util.Objects.hashCode(sourceType);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Program other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      body,
      other.body);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      sourceType,
      other.sourceType);
  }

  public Program withBody(java.util.List<hydra.javaScript.syntax.ModuleItem> body) {
    return new Program(body, sourceType);
  }

  public Program withSourceType(hydra.javaScript.syntax.SourceType sourceType) {
    return new Program(body, sourceType);
  }
}
