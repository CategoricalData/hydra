// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class FixedStatement implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.FixedStatement");
  
  public static final hydra.core.Name FIELD_NAME_POINTER_TYPE = new hydra.core.Name("pointerType");
  
  public static final hydra.core.Name FIELD_NAME_DECLARATORS = new hydra.core.Name("declarators");
  
  public static final hydra.core.Name FIELD_NAME_STATEMENT = new hydra.core.Name("statement");
  
  public final hydra.ext.csharp.syntax.PointerType pointerType;
  
  public final java.util.List<hydra.ext.csharp.syntax.FixedPointerDeclarator> declarators;
  
  public final hydra.ext.csharp.syntax.EmbeddedStatement statement;
  
  public FixedStatement (hydra.ext.csharp.syntax.PointerType pointerType, java.util.List<hydra.ext.csharp.syntax.FixedPointerDeclarator> declarators, hydra.ext.csharp.syntax.EmbeddedStatement statement) {
    java.util.Objects.requireNonNull((pointerType));
    java.util.Objects.requireNonNull((declarators));
    java.util.Objects.requireNonNull((statement));
    this.pointerType = pointerType;
    this.declarators = declarators;
    this.statement = statement;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FixedStatement)) {
      return false;
    }
    FixedStatement o = (FixedStatement) (other);
    return pointerType.equals(o.pointerType) && declarators.equals(o.declarators) && statement.equals(o.statement);
  }
  
  @Override
  public int hashCode() {
    return 2 * pointerType.hashCode() + 3 * declarators.hashCode() + 5 * statement.hashCode();
  }
  
  public FixedStatement withPointerType(hydra.ext.csharp.syntax.PointerType pointerType) {
    java.util.Objects.requireNonNull((pointerType));
    return new FixedStatement(pointerType, declarators, statement);
  }
  
  public FixedStatement withDeclarators(java.util.List<hydra.ext.csharp.syntax.FixedPointerDeclarator> declarators) {
    java.util.Objects.requireNonNull((declarators));
    return new FixedStatement(pointerType, declarators, statement);
  }
  
  public FixedStatement withStatement(hydra.ext.csharp.syntax.EmbeddedStatement statement) {
    java.util.Objects.requireNonNull((statement));
    return new FixedStatement(pointerType, declarators, statement);
  }
}