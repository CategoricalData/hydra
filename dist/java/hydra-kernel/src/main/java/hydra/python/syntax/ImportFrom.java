// Note: this is an automatically generated file. Do not edit.

package hydra.python.syntax;

import java.io.Serializable;

public class ImportFrom implements Serializable, Comparable<ImportFrom> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.python.syntax.ImportFrom");

  public static final hydra.core.Name PREFIXES = new hydra.core.Name("prefixes");

  public static final hydra.core.Name DOTTED_NAME = new hydra.core.Name("dottedName");

  public static final hydra.core.Name TARGETS = new hydra.core.Name("targets");

  public final java.util.List<hydra.python.syntax.RelativeImportPrefix> prefixes;

  public final hydra.util.Maybe<hydra.python.syntax.DottedName> dottedName;

  public final hydra.python.syntax.ImportFromTargets targets;

  public ImportFrom (java.util.List<hydra.python.syntax.RelativeImportPrefix> prefixes, hydra.util.Maybe<hydra.python.syntax.DottedName> dottedName, hydra.python.syntax.ImportFromTargets targets) {
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
    cmp = hydra.util.Comparing.compare(
      prefixes,
      other.prefixes);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      dottedName,
      other.dottedName);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      targets,
      other.targets);
  }

  public ImportFrom withPrefixes(java.util.List<hydra.python.syntax.RelativeImportPrefix> prefixes) {
    return new ImportFrom(prefixes, dottedName, targets);
  }

  public ImportFrom withDottedName(hydra.util.Maybe<hydra.python.syntax.DottedName> dottedName) {
    return new ImportFrom(prefixes, dottedName, targets);
  }

  public ImportFrom withTargets(hydra.python.syntax.ImportFromTargets targets) {
    return new ImportFrom(prefixes, dottedName, targets);
  }
}
