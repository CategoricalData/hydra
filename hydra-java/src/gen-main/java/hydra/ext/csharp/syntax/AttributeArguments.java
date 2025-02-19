// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class AttributeArguments implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.AttributeArguments");
  
  public static final hydra.core.Name FIELD_NAME_POSITONAL = new hydra.core.Name("positonal");
  
  public static final hydra.core.Name FIELD_NAME_NAMED = new hydra.core.Name("named");
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.PositionalArgumentList> positonal;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.NamedArgumentList> named;
  
  public AttributeArguments (hydra.util.Opt<hydra.ext.csharp.syntax.PositionalArgumentList> positonal, hydra.util.Opt<hydra.ext.csharp.syntax.NamedArgumentList> named) {
    java.util.Objects.requireNonNull((positonal));
    java.util.Objects.requireNonNull((named));
    this.positonal = positonal;
    this.named = named;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AttributeArguments)) {
      return false;
    }
    AttributeArguments o = (AttributeArguments) (other);
    return positonal.equals(o.positonal) && named.equals(o.named);
  }
  
  @Override
  public int hashCode() {
    return 2 * positonal.hashCode() + 3 * named.hashCode();
  }
  
  public AttributeArguments withPositonal(hydra.util.Opt<hydra.ext.csharp.syntax.PositionalArgumentList> positonal) {
    java.util.Objects.requireNonNull((positonal));
    return new AttributeArguments(positonal, named);
  }
  
  public AttributeArguments withNamed(hydra.util.Opt<hydra.ext.csharp.syntax.NamedArgumentList> named) {
    java.util.Objects.requireNonNull((named));
    return new AttributeArguments(positonal, named);
  }
}