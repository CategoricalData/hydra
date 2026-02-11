// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * An ordinary (positional) data constructor
 */
public class OrdinaryConstructor implements Serializable, Comparable<OrdinaryConstructor> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.OrdinaryConstructor");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_FIELDS = new hydra.core.Name("fields");
  
  /**
   * The name of the constructor
   */
  public final hydra.ext.haskell.ast.Name name;
  
  /**
   * The types of the positional fields
   */
  public final java.util.List<hydra.ext.haskell.ast.Type> fields;
  
  public OrdinaryConstructor (hydra.ext.haskell.ast.Name name, java.util.List<hydra.ext.haskell.ast.Type> fields) {
    this.name = name;
    this.fields = fields;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof OrdinaryConstructor)) {
      return false;
    }
    OrdinaryConstructor o = (OrdinaryConstructor) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.fields,
      o.fields);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(fields);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(OrdinaryConstructor other) {
    int cmp = 0;
    cmp = ((Comparable) name).compareTo(other.name);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      fields.hashCode(),
      other.fields.hashCode());
  }
  
  public OrdinaryConstructor withName(hydra.ext.haskell.ast.Name name) {
    return new OrdinaryConstructor(name, fields);
  }
  
  public OrdinaryConstructor withFields(java.util.List<hydra.ext.haskell.ast.Type> fields) {
    return new OrdinaryConstructor(name, fields);
  }
}
