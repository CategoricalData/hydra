// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.graphql.syntax;

import java.io.Serializable;

public class RootOperationTypeDefinition implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.graphql.syntax.RootOperationTypeDefinition");
  
  public static final hydra.core.Name FIELD_NAME_OPERATION_TYPE = new hydra.core.Name("operationType");
  
  public static final hydra.core.Name FIELD_NAME_NAMED_TYPE = new hydra.core.Name("namedType");
  
  public final hydra.ext.org.graphql.syntax.OperationType operationType;
  
  public final hydra.ext.org.graphql.syntax.NamedType namedType;
  
  public RootOperationTypeDefinition (hydra.ext.org.graphql.syntax.OperationType operationType, hydra.ext.org.graphql.syntax.NamedType namedType) {
    java.util.Objects.requireNonNull((operationType));
    java.util.Objects.requireNonNull((namedType));
    this.operationType = operationType;
    this.namedType = namedType;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RootOperationTypeDefinition)) {
      return false;
    }
    RootOperationTypeDefinition o = (RootOperationTypeDefinition) (other);
    return operationType.equals(o.operationType) && namedType.equals(o.namedType);
  }
  
  @Override
  public int hashCode() {
    return 2 * operationType.hashCode() + 3 * namedType.hashCode();
  }
  
  public RootOperationTypeDefinition withOperationType(hydra.ext.org.graphql.syntax.OperationType operationType) {
    java.util.Objects.requireNonNull((operationType));
    return new RootOperationTypeDefinition(operationType, namedType);
  }
  
  public RootOperationTypeDefinition withNamedType(hydra.ext.org.graphql.syntax.NamedType namedType) {
    java.util.Objects.requireNonNull((namedType));
    return new RootOperationTypeDefinition(operationType, namedType);
  }
}