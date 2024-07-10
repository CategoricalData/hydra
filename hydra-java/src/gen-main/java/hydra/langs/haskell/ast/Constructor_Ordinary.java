// Note: this is an automatically generated file. Do not edit.

package hydra.langs.haskell.ast;

import java.io.Serializable;

/**
 * An ordinary (positional) data constructor
 */
public class Constructor_Ordinary implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/haskell/ast.Constructor.Ordinary");
  
  public final hydra.langs.haskell.ast.Name name;
  
  public final java.util.List<hydra.langs.haskell.ast.Type> fields;
  
  public Constructor_Ordinary (hydra.langs.haskell.ast.Name name, java.util.List<hydra.langs.haskell.ast.Type> fields) {
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    if (fields == null) {
      throw new IllegalArgumentException("null value for 'fields' argument");
    }
    this.name = name;
    this.fields = fields;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Constructor_Ordinary)) {
      return false;
    }
    Constructor_Ordinary o = (Constructor_Ordinary) (other);
    return name.equals(o.name) && fields.equals(o.fields);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * fields.hashCode();
  }
  
  public Constructor_Ordinary withName(hydra.langs.haskell.ast.Name name) {
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    return new Constructor_Ordinary(name, fields);
  }
  
  public Constructor_Ordinary withFields(java.util.List<hydra.langs.haskell.ast.Type> fields) {
    if (fields == null) {
      throw new IllegalArgumentException("null value for 'fields' argument");
    }
    return new Constructor_Ordinary(name, fields);
  }
}