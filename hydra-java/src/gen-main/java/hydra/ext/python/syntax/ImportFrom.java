// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class ImportFrom implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.ImportFrom");
  
  public static final hydra.core.Name FIELD_NAME_PREFIXES = new hydra.core.Name("prefixes");
  
  public static final hydra.core.Name FIELD_NAME_DOTTED_NAME = new hydra.core.Name("dottedName");
  
  public static final hydra.core.Name FIELD_NAME_TARGETS = new hydra.core.Name("targets");
  
  public final java.util.List<hydra.ext.python.syntax.RelativeImportPrefix> prefixes;
  
  public final hydra.util.Opt<hydra.ext.python.syntax.DottedName> dottedName;
  
  public final hydra.ext.python.syntax.ImportFromTargets targets;
  
  public ImportFrom (java.util.List<hydra.ext.python.syntax.RelativeImportPrefix> prefixes, hydra.util.Opt<hydra.ext.python.syntax.DottedName> dottedName, hydra.ext.python.syntax.ImportFromTargets targets) {
    java.util.Objects.requireNonNull((prefixes));
    java.util.Objects.requireNonNull((dottedName));
    java.util.Objects.requireNonNull((targets));
    this.prefixes = prefixes;
    this.dottedName = dottedName;
    this.targets = targets;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ImportFrom)) {
      return false;
    }
    ImportFrom o = (ImportFrom) (other);
    return prefixes.equals(o.prefixes) && dottedName.equals(o.dottedName) && targets.equals(o.targets);
  }
  
  @Override
  public int hashCode() {
    return 2 * prefixes.hashCode() + 3 * dottedName.hashCode() + 5 * targets.hashCode();
  }
  
  public ImportFrom withPrefixes(java.util.List<hydra.ext.python.syntax.RelativeImportPrefix> prefixes) {
    java.util.Objects.requireNonNull((prefixes));
    return new ImportFrom(prefixes, dottedName, targets);
  }
  
  public ImportFrom withDottedName(hydra.util.Opt<hydra.ext.python.syntax.DottedName> dottedName) {
    java.util.Objects.requireNonNull((dottedName));
    return new ImportFrom(prefixes, dottedName, targets);
  }
  
  public ImportFrom withTargets(hydra.ext.python.syntax.ImportFromTargets targets) {
    java.util.Objects.requireNonNull((targets));
    return new ImportFrom(prefixes, dottedName, targets);
  }
}