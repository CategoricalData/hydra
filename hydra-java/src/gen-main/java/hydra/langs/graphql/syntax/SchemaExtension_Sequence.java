// Note: this is an automatically generated file. Do not edit.

package hydra.langs.graphql.syntax;

import java.io.Serializable;

public class SchemaExtension_Sequence implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/graphql/syntax.SchemaExtension.Sequence");
  
  public final java.util.Optional<hydra.langs.graphql.syntax.Directives> directives;
  
  public final hydra.langs.graphql.syntax.RootOperationTypeDefinition rootOperationTypeDefinition;
  
  public SchemaExtension_Sequence (java.util.Optional<hydra.langs.graphql.syntax.Directives> directives, hydra.langs.graphql.syntax.RootOperationTypeDefinition rootOperationTypeDefinition) {
    if (directives == null) {
      throw new IllegalArgumentException("null value for 'directives' argument");
    }
    if (rootOperationTypeDefinition == null) {
      throw new IllegalArgumentException("null value for 'rootOperationTypeDefinition' argument");
    }
    this.directives = directives;
    this.rootOperationTypeDefinition = rootOperationTypeDefinition;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SchemaExtension_Sequence)) {
      return false;
    }
    SchemaExtension_Sequence o = (SchemaExtension_Sequence) (other);
    return directives.equals(o.directives) && rootOperationTypeDefinition.equals(o.rootOperationTypeDefinition);
  }
  
  @Override
  public int hashCode() {
    return 2 * directives.hashCode() + 3 * rootOperationTypeDefinition.hashCode();
  }
  
  public SchemaExtension_Sequence withDirectives(java.util.Optional<hydra.langs.graphql.syntax.Directives> directives) {
    if (directives == null) {
      throw new IllegalArgumentException("null value for 'directives' argument");
    }
    return new SchemaExtension_Sequence(directives, rootOperationTypeDefinition);
  }
  
  public SchemaExtension_Sequence withRootOperationTypeDefinition(hydra.langs.graphql.syntax.RootOperationTypeDefinition rootOperationTypeDefinition) {
    if (rootOperationTypeDefinition == null) {
      throw new IllegalArgumentException("null value for 'rootOperationTypeDefinition' argument");
    }
    return new SchemaExtension_Sequence(directives, rootOperationTypeDefinition);
  }
}