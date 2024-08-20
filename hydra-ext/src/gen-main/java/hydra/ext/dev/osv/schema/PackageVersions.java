// Note: this is an automatically generated file. Do not edit.

package hydra.ext.dev.osv.schema;

import java.io.Serializable;

public class PackageVersions implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/dev/osv/schema.PackageVersions");
  
  public static final hydra.core.Name FIELD_NAME_PACKAGE = new hydra.core.Name("package");
  
  public static final hydra.core.Name FIELD_NAME_RANGES = new hydra.core.Name("ranges");
  
  public static final hydra.core.Name FIELD_NAME_VERSIONS = new hydra.core.Name("versions");
  
  public final hydra.ext.dev.osv.schema.Package_ package_;
  
  public final hydra.util.Opt<java.util.List<hydra.ext.dev.osv.schema.VersionRange>> ranges;
  
  public final hydra.util.Opt<java.util.List<hydra.ext.dev.osv.schema.Version>> versions;
  
  public PackageVersions (hydra.ext.dev.osv.schema.Package_ package_, hydra.util.Opt<java.util.List<hydra.ext.dev.osv.schema.VersionRange>> ranges, hydra.util.Opt<java.util.List<hydra.ext.dev.osv.schema.Version>> versions) {
    java.util.Objects.requireNonNull((package_));
    java.util.Objects.requireNonNull((ranges));
    java.util.Objects.requireNonNull((versions));
    this.package_ = package_;
    this.ranges = ranges;
    this.versions = versions;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PackageVersions)) {
      return false;
    }
    PackageVersions o = (PackageVersions) (other);
    return package_.equals(o.package_) && ranges.equals(o.ranges) && versions.equals(o.versions);
  }
  
  @Override
  public int hashCode() {
    return 2 * package_.hashCode() + 3 * ranges.hashCode() + 5 * versions.hashCode();
  }
  
  public PackageVersions withPackage(hydra.ext.dev.osv.schema.Package_ package_) {
    java.util.Objects.requireNonNull((package_));
    return new PackageVersions(package_, ranges, versions);
  }
  
  public PackageVersions withRanges(hydra.util.Opt<java.util.List<hydra.ext.dev.osv.schema.VersionRange>> ranges) {
    java.util.Objects.requireNonNull((ranges));
    return new PackageVersions(package_, ranges, versions);
  }
  
  public PackageVersions withVersions(hydra.util.Opt<java.util.List<hydra.ext.dev.osv.schema.Version>> versions) {
    java.util.Objects.requireNonNull((versions));
    return new PackageVersions(package_, ranges, versions);
  }
}