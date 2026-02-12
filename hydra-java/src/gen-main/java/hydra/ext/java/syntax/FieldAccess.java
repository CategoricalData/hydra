// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class FieldAccess implements Serializable, Comparable<FieldAccess> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.FieldAccess");
  
  public static final hydra.core.Name FIELD_NAME_QUALIFIER = new hydra.core.Name("qualifier");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public final hydra.ext.java.syntax.FieldAccess_Qualifier qualifier;
  
  public final hydra.ext.java.syntax.Identifier identifier;
  
  public FieldAccess (hydra.ext.java.syntax.FieldAccess_Qualifier qualifier, hydra.ext.java.syntax.Identifier identifier) {
    this.qualifier = qualifier;
    this.identifier = identifier;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FieldAccess)) {
      return false;
    }
    FieldAccess o = (FieldAccess) other;
    return java.util.Objects.equals(
      this.qualifier,
      o.qualifier) && java.util.Objects.equals(
      this.identifier,
      o.identifier);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(qualifier) + 3 * java.util.Objects.hashCode(identifier);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(FieldAccess other) {
    int cmp = 0;
    cmp = ((Comparable) qualifier).compareTo(other.qualifier);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) identifier).compareTo(other.identifier);
  }
  
  public FieldAccess withQualifier(hydra.ext.java.syntax.FieldAccess_Qualifier qualifier) {
    return new FieldAccess(qualifier, identifier);
  }
  
  public FieldAccess withIdentifier(hydra.ext.java.syntax.Identifier identifier) {
    return new FieldAccess(qualifier, identifier);
  }
}
