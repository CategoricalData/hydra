// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class ImportFrom implements Serializable, Comparable<ImportFrom> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.ImportFrom");
  
  public static final hydra.core.Name FIELD_NAME_PREFIXES = new hydra.core.Name("prefixes");
  
  public static final hydra.core.Name FIELD_NAME_DOTTED_NAME = new hydra.core.Name("dottedName");
  
  public static final hydra.core.Name FIELD_NAME_TARGETS = new hydra.core.Name("targets");
  
  public final java.util.List<hydra.ext.python.syntax.RelativeImportPrefix> prefixes;
  
  public final hydra.util.Maybe<hydra.ext.python.syntax.DottedName> dottedName;
  
  public final hydra.ext.python.syntax.ImportFromTargets targets;
  
  public ImportFrom (java.util.List<hydra.ext.python.syntax.RelativeImportPrefix> prefixes, hydra.util.Maybe<hydra.ext.python.syntax.DottedName> dottedName, hydra.ext.python.syntax.ImportFromTargets targets) {
    this.prefixes = prefixes;
    this.dottedName = dottedName;
    this.targets = targets;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ImportFrom)) {
      return false;
    }
    ImportFrom o = (ImportFrom) other;
    return java.util.Objects.equals(
      this.prefixes,
      o.prefixes) && java.util.Objects.equals(
      this.dottedName,
      o.dottedName) && java.util.Objects.equals(
      this.targets,
      o.targets);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(prefixes) + 3 * java.util.Objects.hashCode(dottedName) + 5 * java.util.Objects.hashCode(targets);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ImportFrom other) {
    int cmp = 0;
    cmp = Integer.compare(
      prefixes.hashCode(),
      other.prefixes.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      dottedName.hashCode(),
      other.dottedName.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) targets).compareTo(other.targets);
  }
  
  public ImportFrom withPrefixes(java.util.List<hydra.ext.python.syntax.RelativeImportPrefix> prefixes) {
    return new ImportFrom(prefixes, dottedName, targets);
  }
  
  public ImportFrom withDottedName(hydra.util.Maybe<hydra.ext.python.syntax.DottedName> dottedName) {
    return new ImportFrom(prefixes, dottedName, targets);
  }
  
  public ImportFrom withTargets(hydra.ext.python.syntax.ImportFromTargets targets) {
    return new ImportFrom(prefixes, dottedName, targets);
  }
}
