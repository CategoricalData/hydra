// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Pat_Interpolate implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.scala.meta.Pat_Interpolate");
  
  public static final hydra.core.Name FIELD_NAME_PREFIX = new hydra.core.Name("prefix");
  
  public static final hydra.core.Name FIELD_NAME_PARTS = new hydra.core.Name("parts");
  
  public final hydra.ext.scala.meta.Data_Name prefix;
  
  public final java.util.List<hydra.ext.scala.meta.Lit> parts;
  
  public Pat_Interpolate (hydra.ext.scala.meta.Data_Name prefix, java.util.List<hydra.ext.scala.meta.Lit> parts) {
    java.util.Objects.requireNonNull((prefix));
    java.util.Objects.requireNonNull((parts));
    this.prefix = prefix;
    this.parts = parts;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Pat_Interpolate)) {
      return false;
    }
    Pat_Interpolate o = (Pat_Interpolate) (other);
    return prefix.equals(o.prefix) && parts.equals(o.parts);
  }
  
  @Override
  public int hashCode() {
    return 2 * prefix.hashCode() + 3 * parts.hashCode();
  }
  
  public Pat_Interpolate withPrefix(hydra.ext.scala.meta.Data_Name prefix) {
    java.util.Objects.requireNonNull((prefix));
    return new Pat_Interpolate(prefix, parts);
  }
  
  public Pat_Interpolate withParts(java.util.List<hydra.ext.scala.meta.Lit> parts) {
    java.util.Objects.requireNonNull((parts));
    return new Pat_Interpolate(prefix, parts);
  }
}