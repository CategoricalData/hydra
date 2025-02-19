// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Pat_Xml implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.scala.meta.Pat_Xml");
  
  public static final hydra.core.Name FIELD_NAME_PARTS = new hydra.core.Name("parts");
  
  public static final hydra.core.Name FIELD_NAME_ARGS = new hydra.core.Name("args");
  
  public final java.util.List<hydra.ext.scala.meta.Lit> parts;
  
  public final java.util.List<hydra.ext.scala.meta.Pat> args;
  
  public Pat_Xml (java.util.List<hydra.ext.scala.meta.Lit> parts, java.util.List<hydra.ext.scala.meta.Pat> args) {
    java.util.Objects.requireNonNull((parts));
    java.util.Objects.requireNonNull((args));
    this.parts = parts;
    this.args = args;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Pat_Xml)) {
      return false;
    }
    Pat_Xml o = (Pat_Xml) (other);
    return parts.equals(o.parts) && args.equals(o.args);
  }
  
  @Override
  public int hashCode() {
    return 2 * parts.hashCode() + 3 * args.hashCode();
  }
  
  public Pat_Xml withParts(java.util.List<hydra.ext.scala.meta.Lit> parts) {
    java.util.Objects.requireNonNull((parts));
    return new Pat_Xml(parts, args);
  }
  
  public Pat_Xml withArgs(java.util.List<hydra.ext.scala.meta.Pat> args) {
    java.util.Objects.requireNonNull((args));
    return new Pat_Xml(parts, args);
  }
}