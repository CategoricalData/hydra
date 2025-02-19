// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class FromClause implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.FromClause");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public static final hydra.core.Name FIELD_NAME_IN = new hydra.core.Name("in");
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.Type> type;
  
  public final hydra.ext.csharp.syntax.Identifier identifier;
  
  public final hydra.ext.csharp.syntax.Expression in;
  
  public FromClause (hydra.util.Opt<hydra.ext.csharp.syntax.Type> type, hydra.ext.csharp.syntax.Identifier identifier, hydra.ext.csharp.syntax.Expression in) {
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((identifier));
    java.util.Objects.requireNonNull((in));
    this.type = type;
    this.identifier = identifier;
    this.in = in;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FromClause)) {
      return false;
    }
    FromClause o = (FromClause) (other);
    return type.equals(o.type) && identifier.equals(o.identifier) && in.equals(o.in);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * identifier.hashCode() + 5 * in.hashCode();
  }
  
  public FromClause withType(hydra.util.Opt<hydra.ext.csharp.syntax.Type> type) {
    java.util.Objects.requireNonNull((type));
    return new FromClause(type, identifier, in);
  }
  
  public FromClause withIdentifier(hydra.ext.csharp.syntax.Identifier identifier) {
    java.util.Objects.requireNonNull((identifier));
    return new FromClause(type, identifier, in);
  }
  
  public FromClause withIn(hydra.ext.csharp.syntax.Expression in) {
    java.util.Objects.requireNonNull((in));
    return new FromClause(type, identifier, in);
  }
}