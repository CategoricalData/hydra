// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * A default import specifier (import x from ...)
 */
public class ImportDefaultSpecifier implements Serializable, Comparable<ImportDefaultSpecifier> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.ImportDefaultSpecifier");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.javaScript.syntax.Identifier value;

  public ImportDefaultSpecifier (hydra.javaScript.syntax.Identifier value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ImportDefaultSpecifier)) {
      return false;
    }
    ImportDefaultSpecifier o = (ImportDefaultSpecifier) other;
    return java.util.Objects.equals(
      this.value,
      o.value);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ImportDefaultSpecifier other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
