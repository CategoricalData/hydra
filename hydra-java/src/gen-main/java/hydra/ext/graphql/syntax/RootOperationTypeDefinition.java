package hydra.ext.graphql.syntax;

public class RootOperationTypeDefinition {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/graphql/syntax.RootOperationTypeDefinition");
  
  public final hydra.ext.graphql.syntax.OperationType operationType;
  
  public final hydra.ext.graphql.syntax.NamedType namedType;
  
  public RootOperationTypeDefinition (hydra.ext.graphql.syntax.OperationType operationType, hydra.ext.graphql.syntax.NamedType namedType) {
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
  
  public RootOperationTypeDefinition withOperationType(hydra.ext.graphql.syntax.OperationType operationType) {
    return new RootOperationTypeDefinition(operationType, namedType);
  }
  
  public RootOperationTypeDefinition withNamedType(hydra.ext.graphql.syntax.NamedType namedType) {
    return new RootOperationTypeDefinition(operationType, namedType);
  }
}