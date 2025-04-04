// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * An ordinary (positional) data constructor
 */
public class OrdinaryConstructor implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.OrdinaryConstructor");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_FIELDS = new hydra.core.Name("fields");
  
  public final hydra.ext.haskell.ast.Name name;
  
  public final java.util.List<hydra.ext.haskell.ast.Type> fields;
  
  public OrdinaryConstructor (hydra.ext.haskell.ast.Name name, java.util.List<hydra.ext.haskell.ast.Type> fields) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((fields));
    this.name = name;
    this.fields = fields;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof OrdinaryConstructor)) {
      return false;
    }
    OrdinaryConstructor o = (OrdinaryConstructor) (other);
    return name.equals(o.name) && fields.equals(o.fields);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * fields.hashCode();
  }
  
  public OrdinaryConstructor withName(hydra.ext.haskell.ast.Name name) {
    java.util.Objects.requireNonNull((name));
    return new OrdinaryConstructor(name, fields);
  }
  
  public OrdinaryConstructor withFields(java.util.List<hydra.ext.haskell.ast.Type> fields) {
    java.util.Objects.requireNonNull((fields));
    return new OrdinaryConstructor(name, fields);
  }
}