// Note: this is an automatically generated file. Do not edit.

package hydra.ext.graphql.syntax;

import java.io.Serializable;

public class InlineFragment implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/graphql/syntax.InlineFragment");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_CONDITION = new hydra.core.Name("typeCondition");
  
  public static final hydra.core.Name FIELD_NAME_DIRECTIVES = new hydra.core.Name("directives");
  
  public static final hydra.core.Name FIELD_NAME_SELECTION_SET = new hydra.core.Name("selectionSet");
  
  public final hydra.util.Opt<hydra.ext.graphql.syntax.TypeCondition> typeCondition;
  
  public final hydra.util.Opt<hydra.ext.graphql.syntax.Directives> directives;
  
  public final hydra.ext.graphql.syntax.SelectionSet selectionSet;
  
  public InlineFragment (hydra.util.Opt<hydra.ext.graphql.syntax.TypeCondition> typeCondition, hydra.util.Opt<hydra.ext.graphql.syntax.Directives> directives, hydra.ext.graphql.syntax.SelectionSet selectionSet) {
    java.util.Objects.requireNonNull((typeCondition));
    java.util.Objects.requireNonNull((directives));
    java.util.Objects.requireNonNull((selectionSet));
    this.typeCondition = typeCondition;
    this.directives = directives;
    this.selectionSet = selectionSet;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InlineFragment)) {
      return false;
    }
    InlineFragment o = (InlineFragment) (other);
    return typeCondition.equals(o.typeCondition) && directives.equals(o.directives) && selectionSet.equals(o.selectionSet);
  }
  
  @Override
  public int hashCode() {
    return 2 * typeCondition.hashCode() + 3 * directives.hashCode() + 5 * selectionSet.hashCode();
  }
  
  public InlineFragment withTypeCondition(hydra.util.Opt<hydra.ext.graphql.syntax.TypeCondition> typeCondition) {
    java.util.Objects.requireNonNull((typeCondition));
    return new InlineFragment(typeCondition, directives, selectionSet);
  }
  
  public InlineFragment withDirectives(hydra.util.Opt<hydra.ext.graphql.syntax.Directives> directives) {
    java.util.Objects.requireNonNull((directives));
    return new InlineFragment(typeCondition, directives, selectionSet);
  }
  
  public InlineFragment withSelectionSet(hydra.ext.graphql.syntax.SelectionSet selectionSet) {
    java.util.Objects.requireNonNull((selectionSet));
    return new InlineFragment(typeCondition, directives, selectionSet);
  }
}
