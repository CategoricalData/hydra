// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class TupleTypeElement implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.TupleTypeElement");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public final hydra.ext.csharp.syntax.Type type;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.Identifier> identifier;
  
  public TupleTypeElement (hydra.ext.csharp.syntax.Type type, hydra.util.Opt<hydra.ext.csharp.syntax.Identifier> identifier) {
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((identifier));
    this.type = type;
    this.identifier = identifier;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TupleTypeElement)) {
      return false;
    }
    TupleTypeElement o = (TupleTypeElement) (other);
    return type.equals(o.type) && identifier.equals(o.identifier);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * identifier.hashCode();
  }
  
  public TupleTypeElement withType(hydra.ext.csharp.syntax.Type type) {
    java.util.Objects.requireNonNull((type));
    return new TupleTypeElement(type, identifier);
  }
  
  public TupleTypeElement withIdentifier(hydra.util.Opt<hydra.ext.csharp.syntax.Identifier> identifier) {
    java.util.Objects.requireNonNull((identifier));
    return new TupleTypeElement(type, identifier);
  }
}