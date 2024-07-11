// Note: this is an automatically generated file. Do not edit.

package hydra.langs.graphql.syntax;

import java.io.Serializable;

public class SchemaDefinition implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/graphql/syntax.SchemaDefinition");
  
  public final hydra.util.Opt<hydra.langs.graphql.syntax.Description> description;
  
  public final hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives;
  
  public final hydra.langs.graphql.syntax.RootOperationTypeDefinition rootOperationTypeDefinition;
  
  public SchemaDefinition (hydra.util.Opt<hydra.langs.graphql.syntax.Description> description, hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives, hydra.langs.graphql.syntax.RootOperationTypeDefinition rootOperationTypeDefinition) {
    if (description == null) {
      throw new IllegalArgumentException("null value for 'description' argument");
    }
    if (directives == null) {
      throw new IllegalArgumentException("null value for 'directives' argument");
    }
    if (rootOperationTypeDefinition == null) {
      throw new IllegalArgumentException("null value for 'rootOperationTypeDefinition' argument");
    }
    this.description = description;
    this.directives = directives;
    this.rootOperationTypeDefinition = rootOperationTypeDefinition;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SchemaDefinition)) {
      return false;
    }
    SchemaDefinition o = (SchemaDefinition) (other);
    return description.equals(o.description) && directives.equals(o.directives) && rootOperationTypeDefinition.equals(o.rootOperationTypeDefinition);
  }
  
  @Override
  public int hashCode() {
    return 2 * description.hashCode() + 3 * directives.hashCode() + 5 * rootOperationTypeDefinition.hashCode();
  }
  
  public SchemaDefinition withDescription(hydra.util.Opt<hydra.langs.graphql.syntax.Description> description) {
    if (description == null) {
      throw new IllegalArgumentException("null value for 'description' argument");
    }
    return new SchemaDefinition(description, directives, rootOperationTypeDefinition);
  }
  
  public SchemaDefinition withDirectives(hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives) {
    if (directives == null) {
      throw new IllegalArgumentException("null value for 'directives' argument");
    }
    return new SchemaDefinition(description, directives, rootOperationTypeDefinition);
  }
  
  public SchemaDefinition withRootOperationTypeDefinition(hydra.langs.graphql.syntax.RootOperationTypeDefinition rootOperationTypeDefinition) {
    if (rootOperationTypeDefinition == null) {
      throw new IllegalArgumentException("null value for 'rootOperationTypeDefinition' argument");
    }
    return new SchemaDefinition(description, directives, rootOperationTypeDefinition);
  }
}