// Note: this is an automatically generated file. Do not edit.

package hydra.langs.graphql.syntax;

import java.io.Serializable;

public class Field implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/graphql/syntax.Field");
  
  public final hydra.util.Opt<hydra.langs.graphql.syntax.Alias> alias;
  
  public final hydra.langs.graphql.syntax.Name name;
  
  public final hydra.util.Opt<hydra.langs.graphql.syntax.Arguments> arguments;
  
  public final hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives;
  
  public final hydra.util.Opt<hydra.langs.graphql.syntax.SelectionSet> selectionSet;
  
  public Field (hydra.util.Opt<hydra.langs.graphql.syntax.Alias> alias, hydra.langs.graphql.syntax.Name name, hydra.util.Opt<hydra.langs.graphql.syntax.Arguments> arguments, hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives, hydra.util.Opt<hydra.langs.graphql.syntax.SelectionSet> selectionSet) {
    java.util.Objects.requireNonNull((alias));
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((arguments));
    java.util.Objects.requireNonNull((directives));
    java.util.Objects.requireNonNull((selectionSet));
    this.alias = alias;
    this.name = name;
    this.arguments = arguments;
    this.directives = directives;
    this.selectionSet = selectionSet;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Field)) {
      return false;
    }
    Field o = (Field) (other);
    return alias.equals(o.alias) && name.equals(o.name) && arguments.equals(o.arguments) && directives.equals(o.directives) && selectionSet.equals(o.selectionSet);
  }
  
  @Override
  public int hashCode() {
    return 2 * alias.hashCode() + 3 * name.hashCode() + 5 * arguments.hashCode() + 7 * directives.hashCode() + 11 * selectionSet.hashCode();
  }
  
  public Field withAlias(hydra.util.Opt<hydra.langs.graphql.syntax.Alias> alias) {
    java.util.Objects.requireNonNull((alias));
    return new Field(alias, name, arguments, directives, selectionSet);
  }
  
  public Field withName(hydra.langs.graphql.syntax.Name name) {
    java.util.Objects.requireNonNull((name));
    return new Field(alias, name, arguments, directives, selectionSet);
  }
  
  public Field withArguments(hydra.util.Opt<hydra.langs.graphql.syntax.Arguments> arguments) {
    java.util.Objects.requireNonNull((arguments));
    return new Field(alias, name, arguments, directives, selectionSet);
  }
  
  public Field withDirectives(hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives) {
    java.util.Objects.requireNonNull((directives));
    return new Field(alias, name, arguments, directives, selectionSet);
  }
  
  public Field withSelectionSet(hydra.util.Opt<hydra.langs.graphql.syntax.SelectionSet> selectionSet) {
    java.util.Objects.requireNonNull((selectionSet));
    return new Field(alias, name, arguments, directives, selectionSet);
  }
}