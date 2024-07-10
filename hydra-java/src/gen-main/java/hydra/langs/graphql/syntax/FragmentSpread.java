// Note: this is an automatically generated file. Do not edit.

package hydra.langs.graphql.syntax;

import java.io.Serializable;

public class FragmentSpread implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/graphql/syntax.FragmentSpread");
  
  public final hydra.langs.graphql.syntax.FragmentName fragmentName;
  
  public final java.util.Optional<hydra.langs.graphql.syntax.Directives> directives;
  
  public FragmentSpread (hydra.langs.graphql.syntax.FragmentName fragmentName, java.util.Optional<hydra.langs.graphql.syntax.Directives> directives) {
    if (fragmentName == null) {
      throw new IllegalArgumentException("null value for 'fragmentName' argument");
    }
    if (directives == null) {
      throw new IllegalArgumentException("null value for 'directives' argument");
    }
    this.fragmentName = fragmentName;
    this.directives = directives;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FragmentSpread)) {
      return false;
    }
    FragmentSpread o = (FragmentSpread) (other);
    return fragmentName.equals(o.fragmentName) && directives.equals(o.directives);
  }
  
  @Override
  public int hashCode() {
    return 2 * fragmentName.hashCode() + 3 * directives.hashCode();
  }
  
  public FragmentSpread withFragmentName(hydra.langs.graphql.syntax.FragmentName fragmentName) {
    if (fragmentName == null) {
      throw new IllegalArgumentException("null value for 'fragmentName' argument");
    }
    return new FragmentSpread(fragmentName, directives);
  }
  
  public FragmentSpread withDirectives(java.util.Optional<hydra.langs.graphql.syntax.Directives> directives) {
    if (directives == null) {
      throw new IllegalArgumentException("null value for 'directives' argument");
    }
    return new FragmentSpread(fragmentName, directives);
  }
}