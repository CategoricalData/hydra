package hydra.ext.graphql.syntax;

public class FragmentSpread {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/graphql/syntax.FragmentSpread");
  
  public final hydra.ext.graphql.syntax.FragmentName fragmentName;
  
  public final java.util.Optional<hydra.ext.graphql.syntax.Directives> directives;
  
  public FragmentSpread (hydra.ext.graphql.syntax.FragmentName fragmentName, java.util.Optional<hydra.ext.graphql.syntax.Directives> directives) {
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
  
  public FragmentSpread withFragmentName(hydra.ext.graphql.syntax.FragmentName fragmentName) {
    return new FragmentSpread(fragmentName, directives);
  }
  
  public FragmentSpread withDirectives(java.util.Optional<hydra.ext.graphql.syntax.Directives> directives) {
    return new FragmentSpread(fragmentName, directives);
  }
}