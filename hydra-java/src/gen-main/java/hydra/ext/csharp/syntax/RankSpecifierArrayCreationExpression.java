// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class RankSpecifierArrayCreationExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.RankSpecifierArrayCreationExpression");
  
  public static final hydra.core.Name FIELD_NAME_RANK_SPECIFIER = new hydra.core.Name("rankSpecifier");
  
  public static final hydra.core.Name FIELD_NAME_INITIALIZER = new hydra.core.Name("initializer");
  
  public final hydra.ext.csharp.syntax.RankSpecifier rankSpecifier;
  
  public final hydra.ext.csharp.syntax.ArrayInitializer initializer;
  
  public RankSpecifierArrayCreationExpression (hydra.ext.csharp.syntax.RankSpecifier rankSpecifier, hydra.ext.csharp.syntax.ArrayInitializer initializer) {
    java.util.Objects.requireNonNull((rankSpecifier));
    java.util.Objects.requireNonNull((initializer));
    this.rankSpecifier = rankSpecifier;
    this.initializer = initializer;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RankSpecifierArrayCreationExpression)) {
      return false;
    }
    RankSpecifierArrayCreationExpression o = (RankSpecifierArrayCreationExpression) (other);
    return rankSpecifier.equals(o.rankSpecifier) && initializer.equals(o.initializer);
  }
  
  @Override
  public int hashCode() {
    return 2 * rankSpecifier.hashCode() + 3 * initializer.hashCode();
  }
  
  public RankSpecifierArrayCreationExpression withRankSpecifier(hydra.ext.csharp.syntax.RankSpecifier rankSpecifier) {
    java.util.Objects.requireNonNull((rankSpecifier));
    return new RankSpecifierArrayCreationExpression(rankSpecifier, initializer);
  }
  
  public RankSpecifierArrayCreationExpression withInitializer(hydra.ext.csharp.syntax.ArrayInitializer initializer) {
    java.util.Objects.requireNonNull((initializer));
    return new RankSpecifierArrayCreationExpression(rankSpecifier, initializer);
  }
}