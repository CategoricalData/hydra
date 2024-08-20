// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Data_Interpolate implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/scala/meta.Data.Interpolate");
  
  public static final hydra.core.Name FIELD_NAME_PREFIX = new hydra.core.Name("prefix");
  
  public static final hydra.core.Name FIELD_NAME_PARTS = new hydra.core.Name("parts");
  
  public static final hydra.core.Name FIELD_NAME_ARGS = new hydra.core.Name("args");
  
  public final hydra.ext.scala.meta.Data_Name prefix;
  
  public final java.util.List<hydra.ext.scala.meta.Lit> parts;
  
  public final java.util.List<hydra.ext.scala.meta.Data> args;
  
  public Data_Interpolate (hydra.ext.scala.meta.Data_Name prefix, java.util.List<hydra.ext.scala.meta.Lit> parts, java.util.List<hydra.ext.scala.meta.Data> args) {
    java.util.Objects.requireNonNull((prefix));
    java.util.Objects.requireNonNull((parts));
    java.util.Objects.requireNonNull((args));
    this.prefix = prefix;
    this.parts = parts;
    this.args = args;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_Interpolate)) {
      return false;
    }
    Data_Interpolate o = (Data_Interpolate) (other);
    return prefix.equals(o.prefix) && parts.equals(o.parts) && args.equals(o.args);
  }
  
  @Override
  public int hashCode() {
    return 2 * prefix.hashCode() + 3 * parts.hashCode() + 5 * args.hashCode();
  }
  
  public Data_Interpolate withPrefix(hydra.ext.scala.meta.Data_Name prefix) {
    java.util.Objects.requireNonNull((prefix));
    return new Data_Interpolate(prefix, parts, args);
  }
  
  public Data_Interpolate withParts(java.util.List<hydra.ext.scala.meta.Lit> parts) {
    java.util.Objects.requireNonNull((parts));
    return new Data_Interpolate(prefix, parts, args);
  }
  
  public Data_Interpolate withArgs(java.util.List<hydra.ext.scala.meta.Data> args) {
    java.util.Objects.requireNonNull((args));
    return new Data_Interpolate(prefix, parts, args);
  }
}
