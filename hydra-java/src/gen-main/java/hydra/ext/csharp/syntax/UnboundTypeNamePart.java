// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class UnboundTypeNamePart implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.UnboundTypeNamePart");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public static final hydra.core.Name FIELD_NAME_ALIASED = new hydra.core.Name("aliased");
  
  public static final hydra.core.Name FIELD_NAME_DIMENSION = new hydra.core.Name("dimension");
  
  public final hydra.ext.csharp.syntax.Identifier identifier;
  
  public final Boolean aliased;
  
  public final hydra.util.Opt<Integer> dimension;
  
  public UnboundTypeNamePart (hydra.ext.csharp.syntax.Identifier identifier, Boolean aliased, hydra.util.Opt<Integer> dimension) {
    java.util.Objects.requireNonNull((identifier));
    java.util.Objects.requireNonNull((aliased));
    java.util.Objects.requireNonNull((dimension));
    this.identifier = identifier;
    this.aliased = aliased;
    this.dimension = dimension;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UnboundTypeNamePart)) {
      return false;
    }
    UnboundTypeNamePart o = (UnboundTypeNamePart) (other);
    return identifier.equals(o.identifier) && aliased.equals(o.aliased) && dimension.equals(o.dimension);
  }
  
  @Override
  public int hashCode() {
    return 2 * identifier.hashCode() + 3 * aliased.hashCode() + 5 * dimension.hashCode();
  }
  
  public UnboundTypeNamePart withIdentifier(hydra.ext.csharp.syntax.Identifier identifier) {
    java.util.Objects.requireNonNull((identifier));
    return new UnboundTypeNamePart(identifier, aliased, dimension);
  }
  
  public UnboundTypeNamePart withAliased(Boolean aliased) {
    java.util.Objects.requireNonNull((aliased));
    return new UnboundTypeNamePart(identifier, aliased, dimension);
  }
  
  public UnboundTypeNamePart withDimension(hydra.util.Opt<Integer> dimension) {
    java.util.Objects.requireNonNull((dimension));
    return new UnboundTypeNamePart(identifier, aliased, dimension);
  }
}