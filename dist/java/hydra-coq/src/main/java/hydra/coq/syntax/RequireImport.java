// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

/**
 * A Require Import/Export command
 */
public class RequireImport implements Serializable, Comparable<RequireImport> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.RequireImport");

  public static final hydra.core.Name FROM = new hydra.core.Name("from");

  public static final hydra.core.Name REQUIRE = new hydra.core.Name("require");

  public static final hydra.core.Name QUALIFICATION = new hydra.core.Name("qualification");

  public static final hydra.core.Name MODULES = new hydra.core.Name("modules");

  public final hydra.util.Maybe<hydra.coq.syntax.Qualid> from;

  public final Boolean require;

  public final hydra.util.Maybe<hydra.coq.syntax.ImportQualification> qualification;

  public final java.util.List<hydra.coq.syntax.Qualid> modules;

  public RequireImport (hydra.util.Maybe<hydra.coq.syntax.Qualid> from, Boolean require, hydra.util.Maybe<hydra.coq.syntax.ImportQualification> qualification, java.util.List<hydra.coq.syntax.Qualid> modules) {
    this.from = from;
    this.require = require;
    this.qualification = qualification;
    this.modules = modules;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RequireImport)) {
      return false;
    }
    RequireImport o = (RequireImport) other;
    return java.util.Objects.equals(
      this.from,
      o.from) && java.util.Objects.equals(
      this.require,
      o.require) && java.util.Objects.equals(
      this.qualification,
      o.qualification) && java.util.Objects.equals(
      this.modules,
      o.modules);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(from) + 3 * java.util.Objects.hashCode(require) + 5 * java.util.Objects.hashCode(qualification) + 7 * java.util.Objects.hashCode(modules);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(RequireImport other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      from,
      other.from);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      require,
      other.require);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      qualification,
      other.qualification);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      modules,
      other.modules);
  }

  public RequireImport withFrom(hydra.util.Maybe<hydra.coq.syntax.Qualid> from) {
    return new RequireImport(from, require, qualification, modules);
  }

  public RequireImport withRequire(Boolean require) {
    return new RequireImport(from, require, qualification, modules);
  }

  public RequireImport withQualification(hydra.util.Maybe<hydra.coq.syntax.ImportQualification> qualification) {
    return new RequireImport(from, require, qualification, modules);
  }

  public RequireImport withModules(java.util.List<hydra.coq.syntax.Qualid> modules) {
    return new RequireImport(from, require, qualification, modules);
  }
}
