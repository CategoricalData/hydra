// Note: this is an automatically generated file. Do not edit.

package hydra.packaging;

import java.io.Serializable;

/**
 * A package, which is a named collection of modules with metadata and dependencies
 */
public class Package_ implements Serializable, Comparable<Package_> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.packaging.Package");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name MODULES = new hydra.core.Name("modules");

  public static final hydra.core.Name DEPENDENCIES = new hydra.core.Name("dependencies");

  public static final hydra.core.Name DESCRIPTION = new hydra.core.Name("description");

  /**
   * The name of the package
   */
  public final hydra.packaging.PackageName name;

  /**
   * The modules in this package
   */
  public final hydra.util.ConsList<hydra.packaging.Module> modules;

  /**
   * The packages which this package depends on
   */
  public final hydra.util.ConsList<hydra.packaging.PackageName> dependencies;

  /**
   * An optional human-readable description of the package
   */
  public final hydra.util.Maybe<String> description;

  public Package_ (hydra.packaging.PackageName name, hydra.util.ConsList<hydra.packaging.Module> modules, hydra.util.ConsList<hydra.packaging.PackageName> dependencies, hydra.util.Maybe<String> description) {
    this.name = name;
    this.modules = modules;
    this.dependencies = dependencies;
    this.description = description;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Package_)) {
      return false;
    }
    Package_ o = (Package_) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.modules,
      o.modules) && java.util.Objects.equals(
      this.dependencies,
      o.dependencies) && java.util.Objects.equals(
      this.description,
      o.description);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(modules) + 5 * java.util.Objects.hashCode(dependencies) + 7 * java.util.Objects.hashCode(description);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Package_ other) {
    int cmp = 0;
    cmp = ((Comparable) name).compareTo(other.name);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) modules).compareTo(other.modules);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) dependencies).compareTo(other.dependencies);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) description).compareTo(other.description);
  }

  public Package_ withName(hydra.packaging.PackageName name) {
    return new Package_(name, modules, dependencies, description);
  }

  public Package_ withModules(hydra.util.ConsList<hydra.packaging.Module> modules) {
    return new Package_(name, modules, dependencies, description);
  }

  public Package_ withDependencies(hydra.util.ConsList<hydra.packaging.PackageName> dependencies) {
    return new Package_(name, modules, dependencies, description);
  }

  public Package_ withDescription(hydra.util.Maybe<String> description) {
    return new Package_(name, modules, dependencies, description);
  }
}
