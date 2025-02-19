// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class NamedEntity implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.NamedEntity");
  
  public static final hydra.core.Name FIELD_NAME_TARGET = new hydra.core.Name("target");
  
  public static final hydra.core.Name FIELD_NAME_PARTS = new hydra.core.Name("parts");
  
  public final hydra.ext.csharp.syntax.NamedEntityTarget target;
  
  public final java.util.List<hydra.ext.csharp.syntax.NamedEntityPart> parts;
  
  public NamedEntity (hydra.ext.csharp.syntax.NamedEntityTarget target, java.util.List<hydra.ext.csharp.syntax.NamedEntityPart> parts) {
    java.util.Objects.requireNonNull((target));
    java.util.Objects.requireNonNull((parts));
    this.target = target;
    this.parts = parts;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NamedEntity)) {
      return false;
    }
    NamedEntity o = (NamedEntity) (other);
    return target.equals(o.target) && parts.equals(o.parts);
  }
  
  @Override
  public int hashCode() {
    return 2 * target.hashCode() + 3 * parts.hashCode();
  }
  
  public NamedEntity withTarget(hydra.ext.csharp.syntax.NamedEntityTarget target) {
    java.util.Objects.requireNonNull((target));
    return new NamedEntity(target, parts);
  }
  
  public NamedEntity withParts(java.util.List<hydra.ext.csharp.syntax.NamedEntityPart> parts) {
    java.util.Objects.requireNonNull((parts));
    return new NamedEntity(target, parts);
  }
}