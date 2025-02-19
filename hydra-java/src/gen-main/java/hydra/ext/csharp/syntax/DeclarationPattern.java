// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class DeclarationPattern implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.DeclarationPattern");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_DESIGNATION = new hydra.core.Name("designation");
  
  public final hydra.ext.csharp.syntax.Type type;
  
  public final hydra.ext.csharp.syntax.Designation designation;
  
  public DeclarationPattern (hydra.ext.csharp.syntax.Type type, hydra.ext.csharp.syntax.Designation designation) {
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((designation));
    this.type = type;
    this.designation = designation;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DeclarationPattern)) {
      return false;
    }
    DeclarationPattern o = (DeclarationPattern) (other);
    return type.equals(o.type) && designation.equals(o.designation);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * designation.hashCode();
  }
  
  public DeclarationPattern withType(hydra.ext.csharp.syntax.Type type) {
    java.util.Objects.requireNonNull((type));
    return new DeclarationPattern(type, designation);
  }
  
  public DeclarationPattern withDesignation(hydra.ext.csharp.syntax.Designation designation) {
    java.util.Objects.requireNonNull((designation));
    return new DeclarationPattern(type, designation);
  }
}