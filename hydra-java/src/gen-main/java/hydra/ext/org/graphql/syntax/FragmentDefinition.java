// Note: this is an automatically generated file. Do not edit.

package hydra.ext.graphql.syntax;

import java.io.Serializable;

public class FragmentDefinition implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/graphql/syntax.FragmentDefinition");
  
  public static final hydra.core.Name FIELD_NAME_FRAGMENT_NAME = new hydra.core.Name("fragmentName");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_CONDITION = new hydra.core.Name("typeCondition");
  
  public static final hydra.core.Name FIELD_NAME_DIRECTIVES = new hydra.core.Name("directives");
  
  public static final hydra.core.Name FIELD_NAME_SELECTION_SET = new hydra.core.Name("selectionSet");
  
  public final hydra.ext.graphql.syntax.FragmentName fragmentName;
  
  public final hydra.ext.graphql.syntax.TypeCondition typeCondition;
  
  public final hydra.util.Opt<hydra.ext.graphql.syntax.Directives> directives;
  
  public final hydra.ext.graphql.syntax.SelectionSet selectionSet;
  
  public FragmentDefinition (hydra.ext.graphql.syntax.FragmentName fragmentName, hydra.ext.graphql.syntax.TypeCondition typeCondition, hydra.util.Opt<hydra.ext.graphql.syntax.Directives> directives, hydra.ext.graphql.syntax.SelectionSet selectionSet) {
    java.util.Objects.requireNonNull((fragmentName));
    java.util.Objects.requireNonNull((typeCondition));
    java.util.Objects.requireNonNull((directives));
    java.util.Objects.requireNonNull((selectionSet));
    this.fragmentName = fragmentName;
    this.typeCondition = typeCondition;
    this.directives = directives;
    this.selectionSet = selectionSet;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FragmentDefinition)) {
      return false;
    }
    FragmentDefinition o = (FragmentDefinition) (other);
    return fragmentName.equals(o.fragmentName) && typeCondition.equals(o.typeCondition) && directives.equals(o.directives) && selectionSet.equals(o.selectionSet);
  }
  
  @Override
  public int hashCode() {
    return 2 * fragmentName.hashCode() + 3 * typeCondition.hashCode() + 5 * directives.hashCode() + 7 * selectionSet.hashCode();
  }
  
  public FragmentDefinition withFragmentName(hydra.ext.graphql.syntax.FragmentName fragmentName) {
    java.util.Objects.requireNonNull((fragmentName));
    return new FragmentDefinition(fragmentName, typeCondition, directives, selectionSet);
  }
  
  public FragmentDefinition withTypeCondition(hydra.ext.graphql.syntax.TypeCondition typeCondition) {
    java.util.Objects.requireNonNull((typeCondition));
    return new FragmentDefinition(fragmentName, typeCondition, directives, selectionSet);
  }
  
  public FragmentDefinition withDirectives(hydra.util.Opt<hydra.ext.graphql.syntax.Directives> directives) {
    java.util.Objects.requireNonNull((directives));
    return new FragmentDefinition(fragmentName, typeCondition, directives, selectionSet);
  }
  
  public FragmentDefinition withSelectionSet(hydra.ext.graphql.syntax.SelectionSet selectionSet) {
    java.util.Objects.requireNonNull((selectionSet));
    return new FragmentDefinition(fragmentName, typeCondition, directives, selectionSet);
  }
}
