// Note: this is an automatically generated file. Do not edit.

package hydra.ext.dev.osv.schema;

import java.io.Serializable;

/**
 * One of a limited set of defined ecosystems, currently Go, npm, OSS-Fuzz, PyPI, RubyGems, crates.io, Packagist, Maven, NuGet, Linux, Debian, Hex, Android, GitHub Actions, or Pub
 */
public class Ecosystem implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/dev/osv/schema.Ecosystem");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final String value;
  
  public Ecosystem (String value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Ecosystem)) {
      return false;
    }
    Ecosystem o = (Ecosystem) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}