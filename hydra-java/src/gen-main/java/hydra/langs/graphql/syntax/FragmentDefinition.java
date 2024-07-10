// Note: this is an automatically generated file. Do not edit.

package hydra.langs.graphql.syntax;

import java.io.Serializable;

public class FragmentDefinition implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/graphql/syntax.FragmentDefinition");
  
  public final hydra.langs.graphql.syntax.FragmentName fragmentName;
  
  public final hydra.langs.graphql.syntax.TypeCondition typeCondition;
  
  public final java.util.Optional<hydra.langs.graphql.syntax.Directives> directives;
  
  public final hydra.langs.graphql.syntax.SelectionSet selectionSet;
  
  public FragmentDefinition (hydra.langs.graphql.syntax.FragmentName fragmentName, hydra.langs.graphql.syntax.TypeCondition typeCondition, java.util.Optional<hydra.langs.graphql.syntax.Directives> directives, hydra.langs.graphql.syntax.SelectionSet selectionSet) {
    if (fragmentName == null) {
      throw new IllegalArgumentException("null value for 'fragmentName' argument");
    }
    if (typeCondition == null) {
      throw new IllegalArgumentException("null value for 'typeCondition' argument");
    }
    if (directives == null) {
      throw new IllegalArgumentException("null value for 'directives' argument");
    }
    if (selectionSet == null) {
      throw new IllegalArgumentException("null value for 'selectionSet' argument");
    }
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
  
  public FragmentDefinition withFragmentName(hydra.langs.graphql.syntax.FragmentName fragmentName) {
    if (fragmentName == null) {
      throw new IllegalArgumentException("null value for 'fragmentName' argument");
    }
    return new FragmentDefinition(fragmentName, typeCondition, directives, selectionSet);
  }
  
  public FragmentDefinition withTypeCondition(hydra.langs.graphql.syntax.TypeCondition typeCondition) {
    if (typeCondition == null) {
      throw new IllegalArgumentException("null value for 'typeCondition' argument");
    }
    return new FragmentDefinition(fragmentName, typeCondition, directives, selectionSet);
  }
  
  public FragmentDefinition withDirectives(java.util.Optional<hydra.langs.graphql.syntax.Directives> directives) {
    if (directives == null) {
      throw new IllegalArgumentException("null value for 'directives' argument");
    }
    return new FragmentDefinition(fragmentName, typeCondition, directives, selectionSet);
  }
  
  public FragmentDefinition withSelectionSet(hydra.langs.graphql.syntax.SelectionSet selectionSet) {
    if (selectionSet == null) {
      throw new IllegalArgumentException("null value for 'selectionSet' argument");
    }
    return new FragmentDefinition(fragmentName, typeCondition, directives, selectionSet);
  }
}