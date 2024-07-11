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
    if (alias == null) {
      throw new IllegalArgumentException("null value for 'alias' argument");
    }
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    if (arguments == null) {
      throw new IllegalArgumentException("null value for 'arguments' argument");
    }
    if (directives == null) {
      throw new IllegalArgumentException("null value for 'directives' argument");
    }
    if (selectionSet == null) {
      throw new IllegalArgumentException("null value for 'selectionSet' argument");
    }
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
    if (alias == null) {
      throw new IllegalArgumentException("null value for 'alias' argument");
    }
    return new Field(alias, name, arguments, directives, selectionSet);
  }
  
  public Field withName(hydra.langs.graphql.syntax.Name name) {
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    return new Field(alias, name, arguments, directives, selectionSet);
  }
  
  public Field withArguments(hydra.util.Opt<hydra.langs.graphql.syntax.Arguments> arguments) {
    if (arguments == null) {
      throw new IllegalArgumentException("null value for 'arguments' argument");
    }
    return new Field(alias, name, arguments, directives, selectionSet);
  }
  
  public Field withDirectives(hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives) {
    if (directives == null) {
      throw new IllegalArgumentException("null value for 'directives' argument");
    }
    return new Field(alias, name, arguments, directives, selectionSet);
  }
  
  public Field withSelectionSet(hydra.util.Opt<hydra.langs.graphql.syntax.SelectionSet> selectionSet) {
    if (selectionSet == null) {
      throw new IllegalArgumentException("null value for 'selectionSet' argument");
    }
    return new Field(alias, name, arguments, directives, selectionSet);
  }
}