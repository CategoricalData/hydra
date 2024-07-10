// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public class TypeVariable implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/java/syntax.TypeVariable");
  
  public final java.util.List<hydra.langs.java.syntax.Annotation> annotations;
  
  public final hydra.langs.java.syntax.TypeIdentifier identifier;
  
  public TypeVariable (java.util.List<hydra.langs.java.syntax.Annotation> annotations, hydra.langs.java.syntax.TypeIdentifier identifier) {
    if (annotations == null) {
      throw new IllegalArgumentException("null value for 'annotations' argument");
    }
    if (identifier == null) {
      throw new IllegalArgumentException("null value for 'identifier' argument");
    }
    this.annotations = annotations;
    this.identifier = identifier;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeVariable)) {
      return false;
    }
    TypeVariable o = (TypeVariable) (other);
    return annotations.equals(o.annotations) && identifier.equals(o.identifier);
  }
  
  @Override
  public int hashCode() {
    return 2 * annotations.hashCode() + 3 * identifier.hashCode();
  }
  
  public TypeVariable withAnnotations(java.util.List<hydra.langs.java.syntax.Annotation> annotations) {
    if (annotations == null) {
      throw new IllegalArgumentException("null value for 'annotations' argument");
    }
    return new TypeVariable(annotations, identifier);
  }
  
  public TypeVariable withIdentifier(hydra.langs.java.syntax.TypeIdentifier identifier) {
    if (identifier == null) {
      throw new IllegalArgumentException("null value for 'identifier' argument");
    }
    return new TypeVariable(annotations, identifier);
  }
}