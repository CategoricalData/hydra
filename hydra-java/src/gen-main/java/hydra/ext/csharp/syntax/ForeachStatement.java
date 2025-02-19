// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class ForeachStatement implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.ForeachStatement");
  
  public static final hydra.core.Name FIELD_NAME_KIND = new hydra.core.Name("kind");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.RefKind> kind;
  
  public final hydra.ext.csharp.syntax.LocalVariableType type;
  
  public final hydra.ext.csharp.syntax.Identifier identifier;
  
  public final hydra.ext.csharp.syntax.Expression expression;
  
  public final hydra.ext.csharp.syntax.EmbeddedStatement body;
  
  public ForeachStatement (hydra.util.Opt<hydra.ext.csharp.syntax.RefKind> kind, hydra.ext.csharp.syntax.LocalVariableType type, hydra.ext.csharp.syntax.Identifier identifier, hydra.ext.csharp.syntax.Expression expression, hydra.ext.csharp.syntax.EmbeddedStatement body) {
    java.util.Objects.requireNonNull((kind));
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((identifier));
    java.util.Objects.requireNonNull((expression));
    java.util.Objects.requireNonNull((body));
    this.kind = kind;
    this.type = type;
    this.identifier = identifier;
    this.expression = expression;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ForeachStatement)) {
      return false;
    }
    ForeachStatement o = (ForeachStatement) (other);
    return kind.equals(o.kind) && type.equals(o.type) && identifier.equals(o.identifier) && expression.equals(o.expression) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * kind.hashCode() + 3 * type.hashCode() + 5 * identifier.hashCode() + 7 * expression.hashCode() + 11 * body.hashCode();
  }
  
  public ForeachStatement withKind(hydra.util.Opt<hydra.ext.csharp.syntax.RefKind> kind) {
    java.util.Objects.requireNonNull((kind));
    return new ForeachStatement(kind, type, identifier, expression, body);
  }
  
  public ForeachStatement withType(hydra.ext.csharp.syntax.LocalVariableType type) {
    java.util.Objects.requireNonNull((type));
    return new ForeachStatement(kind, type, identifier, expression, body);
  }
  
  public ForeachStatement withIdentifier(hydra.ext.csharp.syntax.Identifier identifier) {
    java.util.Objects.requireNonNull((identifier));
    return new ForeachStatement(kind, type, identifier, expression, body);
  }
  
  public ForeachStatement withExpression(hydra.ext.csharp.syntax.Expression expression) {
    java.util.Objects.requireNonNull((expression));
    return new ForeachStatement(kind, type, identifier, expression, body);
  }
  
  public ForeachStatement withBody(hydra.ext.csharp.syntax.EmbeddedStatement body) {
    java.util.Objects.requireNonNull((body));
    return new ForeachStatement(kind, type, identifier, expression, body);
  }
}