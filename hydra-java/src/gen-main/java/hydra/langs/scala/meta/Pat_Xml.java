// Note: this is an automatically generated file. Do not edit.

package hydra.langs.scala.meta;

import java.io.Serializable;

public class Pat_Xml implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/scala/meta.Pat.Xml");
  
  public final java.util.List<hydra.langs.scala.meta.Lit> parts;
  
  public final java.util.List<hydra.langs.scala.meta.Pat> args;
  
  public Pat_Xml (java.util.List<hydra.langs.scala.meta.Lit> parts, java.util.List<hydra.langs.scala.meta.Pat> args) {
    if (parts == null) {
      throw new IllegalArgumentException("null value for 'parts' argument");
    }
    if (args == null) {
      throw new IllegalArgumentException("null value for 'args' argument");
    }
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
  
  public Pat_Xml withParts(java.util.List<hydra.langs.scala.meta.Lit> parts) {
    if (parts == null) {
      throw new IllegalArgumentException("null value for 'parts' argument");
    }
    return new Pat_Xml(parts, args);
  }
  
  public Pat_Xml withArgs(java.util.List<hydra.langs.scala.meta.Pat> args) {
    if (args == null) {
      throw new IllegalArgumentException("null value for 'args' argument");
    }
    return new Pat_Xml(parts, args);
  }
}