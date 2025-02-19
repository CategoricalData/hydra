// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class JoinClause implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.JoinClause");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public static final hydra.core.Name FIELD_NAME_IN = new hydra.core.Name("in");
  
  public static final hydra.core.Name FIELD_NAME_ON = new hydra.core.Name("on");
  
  public static final hydra.core.Name FIELD_NAME_EQUALS = new hydra.core.Name("equals");
  
  public static final hydra.core.Name FIELD_NAME_INTO = new hydra.core.Name("into");
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.Type> type;
  
  public final hydra.ext.csharp.syntax.Identifier identifier;
  
  public final hydra.ext.csharp.syntax.Expression in;
  
  public final hydra.ext.csharp.syntax.Expression on;
  
  public final hydra.ext.csharp.syntax.Expression equals;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.Identifier> into;
  
  public JoinClause (hydra.util.Opt<hydra.ext.csharp.syntax.Type> type, hydra.ext.csharp.syntax.Identifier identifier, hydra.ext.csharp.syntax.Expression in, hydra.ext.csharp.syntax.Expression on, hydra.ext.csharp.syntax.Expression equals, hydra.util.Opt<hydra.ext.csharp.syntax.Identifier> into) {
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((identifier));
    java.util.Objects.requireNonNull((in));
    java.util.Objects.requireNonNull((on));
    java.util.Objects.requireNonNull((equals));
    java.util.Objects.requireNonNull((into));
    this.type = type;
    this.identifier = identifier;
    this.in = in;
    this.on = on;
    this.equals = equals;
    this.into = into;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof JoinClause)) {
      return false;
    }
    JoinClause o = (JoinClause) (other);
    return type.equals(o.type) && identifier.equals(o.identifier) && in.equals(o.in) && on.equals(o.on) && equals.equals(o.equals) && into.equals(o.into);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * identifier.hashCode() + 5 * in.hashCode() + 7 * on.hashCode() + 11 * equals.hashCode() + 13 * into.hashCode();
  }
  
  public JoinClause withType(hydra.util.Opt<hydra.ext.csharp.syntax.Type> type) {
    java.util.Objects.requireNonNull((type));
    return new JoinClause(type, identifier, in, on, equals, into);
  }
  
  public JoinClause withIdentifier(hydra.ext.csharp.syntax.Identifier identifier) {
    java.util.Objects.requireNonNull((identifier));
    return new JoinClause(type, identifier, in, on, equals, into);
  }
  
  public JoinClause withIn(hydra.ext.csharp.syntax.Expression in) {
    java.util.Objects.requireNonNull((in));
    return new JoinClause(type, identifier, in, on, equals, into);
  }
  
  public JoinClause withOn(hydra.ext.csharp.syntax.Expression on) {
    java.util.Objects.requireNonNull((on));
    return new JoinClause(type, identifier, in, on, equals, into);
  }
  
  public JoinClause withEquals(hydra.ext.csharp.syntax.Expression equals) {
    java.util.Objects.requireNonNull((equals));
    return new JoinClause(type, identifier, in, on, equals, into);
  }
  
  public JoinClause withInto(hydra.util.Opt<hydra.ext.csharp.syntax.Identifier> into) {
    java.util.Objects.requireNonNull((into));
    return new JoinClause(type, identifier, in, on, equals, into);
  }
}