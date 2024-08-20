// Note: this is an automatically generated file. Do not edit.

package hydra.ext.graphql.syntax;

import java.io.Serializable;

public class SchemaDefinition implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/graphql/syntax.SchemaDefinition");
  
  public static final hydra.core.Name FIELD_NAME_DESCRIPTION = new hydra.core.Name("description");
  
  public static final hydra.core.Name FIELD_NAME_DIRECTIVES = new hydra.core.Name("directives");
  
  public static final hydra.core.Name FIELD_NAME_ROOT_OPERATION_TYPE_DEFINITION = new hydra.core.Name("rootOperationTypeDefinition");
  
  public final hydra.util.Opt<hydra.ext.graphql.syntax.Description> description;
  
  public final hydra.util.Opt<hydra.ext.graphql.syntax.Directives> directives;
  
  public final hydra.ext.graphql.syntax.RootOperationTypeDefinition rootOperationTypeDefinition;
  
  public SchemaDefinition (hydra.util.Opt<hydra.ext.graphql.syntax.Description> description, hydra.util.Opt<hydra.ext.graphql.syntax.Directives> directives, hydra.ext.graphql.syntax.RootOperationTypeDefinition rootOperationTypeDefinition) {
    java.util.Objects.requireNonNull((description));
    java.util.Objects.requireNonNull((directives));
    java.util.Objects.requireNonNull((rootOperationTypeDefinition));
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
  
  public SchemaDefinition withDescription(hydra.util.Opt<hydra.ext.graphql.syntax.Description> description) {
    java.util.Objects.requireNonNull((description));
    return new SchemaDefinition(description, directives, rootOperationTypeDefinition);
  }
  
  public SchemaDefinition withDirectives(hydra.util.Opt<hydra.ext.graphql.syntax.Directives> directives) {
    java.util.Objects.requireNonNull((directives));
    return new SchemaDefinition(description, directives, rootOperationTypeDefinition);
  }
  
  public SchemaDefinition withRootOperationTypeDefinition(hydra.ext.graphql.syntax.RootOperationTypeDefinition rootOperationTypeDefinition) {
    java.util.Objects.requireNonNull((rootOperationTypeDefinition));
    return new SchemaDefinition(description, directives, rootOperationTypeDefinition);
  }
}
