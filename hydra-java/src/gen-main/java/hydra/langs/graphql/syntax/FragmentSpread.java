// Note: this is an automatically generated file. Do not edit.

package hydra.langs.graphql.syntax;

import java.io.Serializable;

public class FragmentSpread implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/graphql/syntax.FragmentSpread");
  
  public static final hydra.core.Name FIELD_NAME_FRAGMENT_NAME = new hydra.core.Name("fragmentName");
  
  public static final hydra.core.Name FIELD_NAME_DIRECTIVES = new hydra.core.Name("directives");
  
  public final hydra.langs.graphql.syntax.FragmentName fragmentName;
  
  public final hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives;
  
  public FragmentSpread (hydra.langs.graphql.syntax.FragmentName fragmentName, hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives) {
    java.util.Objects.requireNonNull((fragmentName));
    java.util.Objects.requireNonNull((directives));
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
    java.util.Objects.requireNonNull((fragmentName));
    return new FragmentSpread(fragmentName, directives);
  }
  
  public FragmentSpread withDirectives(hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives) {
    java.util.Objects.requireNonNull((directives));
    return new FragmentSpread(fragmentName, directives);
  }
}