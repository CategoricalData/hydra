// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * A namespace import specifier (import * as x from ...)
 */
public class ImportNamespaceSpecifier implements Serializable, Comparable<ImportNamespaceSpecifier> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.ImportNamespaceSpecifier");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.javaScript.syntax.Identifier value;

  public ImportNamespaceSpecifier (hydra.javaScript.syntax.Identifier value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ImportNamespaceSpecifier)) {
      return false;
    }
    ImportNamespaceSpecifier o = (ImportNamespaceSpecifier) other;
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
  public int compareTo(ImportNamespaceSpecifier other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
