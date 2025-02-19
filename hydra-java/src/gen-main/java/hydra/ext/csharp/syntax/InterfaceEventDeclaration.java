// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class InterfaceEventDeclaration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.InterfaceEventDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_ATTRIBUTES = new hydra.core.Name("attributes");
  
  public static final hydra.core.Name FIELD_NAME_NEW = new hydra.core.Name("new");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes;
  
  public final Boolean new_;
  
  public final hydra.ext.csharp.syntax.Type type;
  
  public final hydra.ext.csharp.syntax.Identifier name;
  
  public InterfaceEventDeclaration (hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes, Boolean new_, hydra.ext.csharp.syntax.Type type, hydra.ext.csharp.syntax.Identifier name) {
    java.util.Objects.requireNonNull((attributes));
    java.util.Objects.requireNonNull((new_));
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((name));
    this.attributes = attributes;
    this.new_ = new_;
    this.type = type;
    this.name = name;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InterfaceEventDeclaration)) {
      return false;
    }
    InterfaceEventDeclaration o = (InterfaceEventDeclaration) (other);
    return attributes.equals(o.attributes) && new_.equals(o.new_) && type.equals(o.type) && name.equals(o.name);
  }
  
  @Override
  public int hashCode() {
    return 2 * attributes.hashCode() + 3 * new_.hashCode() + 5 * type.hashCode() + 7 * name.hashCode();
  }
  
  public InterfaceEventDeclaration withAttributes(hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes) {
    java.util.Objects.requireNonNull((attributes));
    return new InterfaceEventDeclaration(attributes, new_, type, name);
  }
  
  public InterfaceEventDeclaration withNew(Boolean new_) {
    java.util.Objects.requireNonNull((new_));
    return new InterfaceEventDeclaration(attributes, new_, type, name);
  }
  
  public InterfaceEventDeclaration withType(hydra.ext.csharp.syntax.Type type) {
    java.util.Objects.requireNonNull((type));
    return new InterfaceEventDeclaration(attributes, new_, type, name);
  }
  
  public InterfaceEventDeclaration withName(hydra.ext.csharp.syntax.Identifier name) {
    java.util.Objects.requireNonNull((name));
    return new InterfaceEventDeclaration(attributes, new_, type, name);
  }
}