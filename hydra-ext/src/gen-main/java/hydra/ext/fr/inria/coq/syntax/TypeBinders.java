// Note: this is an automatically generated file. Do not edit.

package hydra.ext.fr.inria.coq.syntax;

import java.io.Serializable;

public class TypeBinders implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.fr.inria.coq.syntax.TypeBinders");
  
  public static final hydra.core.Name FIELD_NAME_NAMES = new hydra.core.Name("names");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public final java.util.List<hydra.ext.fr.inria.coq.syntax.Name> names;
  
  public final hydra.ext.fr.inria.coq.syntax.Type type;
  
  public TypeBinders (java.util.List<hydra.ext.fr.inria.coq.syntax.Name> names, hydra.ext.fr.inria.coq.syntax.Type type) {
    java.util.Objects.requireNonNull((names));
    java.util.Objects.requireNonNull((type));
    this.names = names;
    this.type = type;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeBinders)) {
      return false;
    }
    TypeBinders o = (TypeBinders) (other);
    return names.equals(o.names) && type.equals(o.type);
  }
  
  @Override
  public int hashCode() {
    return 2 * names.hashCode() + 3 * type.hashCode();
  }
  
  public TypeBinders withNames(java.util.List<hydra.ext.fr.inria.coq.syntax.Name> names) {
    java.util.Objects.requireNonNull((names));
    return new TypeBinders(names, type);
  }
  
  public TypeBinders withType(hydra.ext.fr.inria.coq.syntax.Type type) {
    java.util.Objects.requireNonNull((type));
    return new TypeBinders(names, type);
  }
}