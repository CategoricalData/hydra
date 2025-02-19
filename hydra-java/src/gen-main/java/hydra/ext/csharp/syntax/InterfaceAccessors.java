// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class InterfaceAccessors implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.InterfaceAccessors");
  
  public static final hydra.core.Name FIELD_NAME_ATTRIBUTES = new hydra.core.Name("attributes");
  
  public static final hydra.core.Name FIELD_NAME_GET = new hydra.core.Name("get");
  
  public static final hydra.core.Name FIELD_NAME_SET = new hydra.core.Name("set");
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> get;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> set;
  
  public InterfaceAccessors (hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes, hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> get, hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> set) {
    java.util.Objects.requireNonNull((attributes));
    java.util.Objects.requireNonNull((get));
    java.util.Objects.requireNonNull((set));
    this.attributes = attributes;
    this.get = get;
    this.set = set;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InterfaceAccessors)) {
      return false;
    }
    InterfaceAccessors o = (InterfaceAccessors) (other);
    return attributes.equals(o.attributes) && get.equals(o.get) && set.equals(o.set);
  }
  
  @Override
  public int hashCode() {
    return 2 * attributes.hashCode() + 3 * get.hashCode() + 5 * set.hashCode();
  }
  
  public InterfaceAccessors withAttributes(hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes) {
    java.util.Objects.requireNonNull((attributes));
    return new InterfaceAccessors(attributes, get, set);
  }
  
  public InterfaceAccessors withGet(hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> get) {
    java.util.Objects.requireNonNull((get));
    return new InterfaceAccessors(attributes, get, set);
  }
  
  public InterfaceAccessors withSet(hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> set) {
    java.util.Objects.requireNonNull((set));
    return new InterfaceAccessors(attributes, get, set);
  }
}