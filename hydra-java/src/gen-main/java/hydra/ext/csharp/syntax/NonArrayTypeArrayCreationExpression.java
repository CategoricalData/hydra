// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class NonArrayTypeArrayCreationExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.NonArrayTypeArrayCreationExpression");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSIONS = new hydra.core.Name("expressions");
  
  public static final hydra.core.Name FIELD_NAME_RANK_SPECIFIERS = new hydra.core.Name("rankSpecifiers");
  
  public static final hydra.core.Name FIELD_NAME_INITIALIZER = new hydra.core.Name("initializer");
  
  public final hydra.ext.csharp.syntax.NonArrayType type;
  
  public final hydra.ext.csharp.syntax.ExpressionList expressions;
  
  public final java.util.List<hydra.ext.csharp.syntax.RankSpecifier> rankSpecifiers;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.ArrayInitializer> initializer;
  
  public NonArrayTypeArrayCreationExpression (hydra.ext.csharp.syntax.NonArrayType type, hydra.ext.csharp.syntax.ExpressionList expressions, java.util.List<hydra.ext.csharp.syntax.RankSpecifier> rankSpecifiers, hydra.util.Opt<hydra.ext.csharp.syntax.ArrayInitializer> initializer) {
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((expressions));
    java.util.Objects.requireNonNull((rankSpecifiers));
    java.util.Objects.requireNonNull((initializer));
    this.type = type;
    this.expressions = expressions;
    this.rankSpecifiers = rankSpecifiers;
    this.initializer = initializer;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NonArrayTypeArrayCreationExpression)) {
      return false;
    }
    NonArrayTypeArrayCreationExpression o = (NonArrayTypeArrayCreationExpression) (other);
    return type.equals(o.type) && expressions.equals(o.expressions) && rankSpecifiers.equals(o.rankSpecifiers) && initializer.equals(o.initializer);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * expressions.hashCode() + 5 * rankSpecifiers.hashCode() + 7 * initializer.hashCode();
  }
  
  public NonArrayTypeArrayCreationExpression withType(hydra.ext.csharp.syntax.NonArrayType type) {
    java.util.Objects.requireNonNull((type));
    return new NonArrayTypeArrayCreationExpression(type, expressions, rankSpecifiers, initializer);
  }
  
  public NonArrayTypeArrayCreationExpression withExpressions(hydra.ext.csharp.syntax.ExpressionList expressions) {
    java.util.Objects.requireNonNull((expressions));
    return new NonArrayTypeArrayCreationExpression(type, expressions, rankSpecifiers, initializer);
  }
  
  public NonArrayTypeArrayCreationExpression withRankSpecifiers(java.util.List<hydra.ext.csharp.syntax.RankSpecifier> rankSpecifiers) {
    java.util.Objects.requireNonNull((rankSpecifiers));
    return new NonArrayTypeArrayCreationExpression(type, expressions, rankSpecifiers, initializer);
  }
  
  public NonArrayTypeArrayCreationExpression withInitializer(hydra.util.Opt<hydra.ext.csharp.syntax.ArrayInitializer> initializer) {
    java.util.Objects.requireNonNull((initializer));
    return new NonArrayTypeArrayCreationExpression(type, expressions, rankSpecifiers, initializer);
  }
}