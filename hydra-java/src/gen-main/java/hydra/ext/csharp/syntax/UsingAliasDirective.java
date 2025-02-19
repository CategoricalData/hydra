// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class UsingAliasDirective implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.UsingAliasDirective");
  
  public static final hydra.core.Name FIELD_NAME_ALIAS = new hydra.core.Name("alias");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public final hydra.ext.csharp.syntax.Identifier alias;
  
  public final hydra.ext.csharp.syntax.NamespaceOrTypeName name;
  
  public UsingAliasDirective (hydra.ext.csharp.syntax.Identifier alias, hydra.ext.csharp.syntax.NamespaceOrTypeName name) {
    java.util.Objects.requireNonNull((alias));
    java.util.Objects.requireNonNull((name));
    this.alias = alias;
    this.name = name;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UsingAliasDirective)) {
      return false;
    }
    UsingAliasDirective o = (UsingAliasDirective) (other);
    return alias.equals(o.alias) && name.equals(o.name);
  }
  
  @Override
  public int hashCode() {
    return 2 * alias.hashCode() + 3 * name.hashCode();
  }
  
  public UsingAliasDirective withAlias(hydra.ext.csharp.syntax.Identifier alias) {
    java.util.Objects.requireNonNull((alias));
    return new UsingAliasDirective(alias, name);
  }
  
  public UsingAliasDirective withName(hydra.ext.csharp.syntax.NamespaceOrTypeName name) {
    java.util.Objects.requireNonNull((name));
    return new UsingAliasDirective(alias, name);
  }
}