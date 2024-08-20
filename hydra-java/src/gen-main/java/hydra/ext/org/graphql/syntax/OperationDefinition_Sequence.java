// Note: this is an automatically generated file. Do not edit.

package hydra.ext.graphql.syntax;

import java.io.Serializable;

public class OperationDefinition_Sequence implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/graphql/syntax.OperationDefinition.Sequence");
  
  public static final hydra.core.Name FIELD_NAME_OPERATION_TYPE = new hydra.core.Name("operationType");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_VARIABLES_DEFINITION = new hydra.core.Name("variablesDefinition");
  
  public static final hydra.core.Name FIELD_NAME_DIRECTIVES = new hydra.core.Name("directives");
  
  public static final hydra.core.Name FIELD_NAME_SELECTION_SET = new hydra.core.Name("selectionSet");
  
  public final hydra.ext.graphql.syntax.OperationType operationType;
  
  public final hydra.util.Opt<hydra.ext.graphql.syntax.Name> name;
  
  public final hydra.util.Opt<hydra.ext.graphql.syntax.VariablesDefinition> variablesDefinition;
  
  public final hydra.util.Opt<hydra.ext.graphql.syntax.Directives> directives;
  
  public final hydra.ext.graphql.syntax.SelectionSet selectionSet;
  
  public OperationDefinition_Sequence (hydra.ext.graphql.syntax.OperationType operationType, hydra.util.Opt<hydra.ext.graphql.syntax.Name> name, hydra.util.Opt<hydra.ext.graphql.syntax.VariablesDefinition> variablesDefinition, hydra.util.Opt<hydra.ext.graphql.syntax.Directives> directives, hydra.ext.graphql.syntax.SelectionSet selectionSet) {
    java.util.Objects.requireNonNull((operationType));
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((variablesDefinition));
    java.util.Objects.requireNonNull((directives));
    java.util.Objects.requireNonNull((selectionSet));
    this.operationType = operationType;
    this.name = name;
    this.variablesDefinition = variablesDefinition;
    this.directives = directives;
    this.selectionSet = selectionSet;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof OperationDefinition_Sequence)) {
      return false;
    }
    OperationDefinition_Sequence o = (OperationDefinition_Sequence) (other);
    return operationType.equals(o.operationType) && name.equals(o.name) && variablesDefinition.equals(o.variablesDefinition) && directives.equals(o.directives) && selectionSet.equals(o.selectionSet);
  }
  
  @Override
  public int hashCode() {
    return 2 * operationType.hashCode() + 3 * name.hashCode() + 5 * variablesDefinition.hashCode() + 7 * directives.hashCode() + 11 * selectionSet.hashCode();
  }
  
  public OperationDefinition_Sequence withOperationType(hydra.ext.graphql.syntax.OperationType operationType) {
    java.util.Objects.requireNonNull((operationType));
    return new OperationDefinition_Sequence(operationType, name, variablesDefinition, directives, selectionSet);
  }
  
  public OperationDefinition_Sequence withName(hydra.util.Opt<hydra.ext.graphql.syntax.Name> name) {
    java.util.Objects.requireNonNull((name));
    return new OperationDefinition_Sequence(operationType, name, variablesDefinition, directives, selectionSet);
  }
  
  public OperationDefinition_Sequence withVariablesDefinition(hydra.util.Opt<hydra.ext.graphql.syntax.VariablesDefinition> variablesDefinition) {
    java.util.Objects.requireNonNull((variablesDefinition));
    return new OperationDefinition_Sequence(operationType, name, variablesDefinition, directives, selectionSet);
  }
  
  public OperationDefinition_Sequence withDirectives(hydra.util.Opt<hydra.ext.graphql.syntax.Directives> directives) {
    java.util.Objects.requireNonNull((directives));
    return new OperationDefinition_Sequence(operationType, name, variablesDefinition, directives, selectionSet);
  }
  
  public OperationDefinition_Sequence withSelectionSet(hydra.ext.graphql.syntax.SelectionSet selectionSet) {
    java.util.Objects.requireNonNull((selectionSet));
    return new OperationDefinition_Sequence(operationType, name, variablesDefinition, directives, selectionSet);
  }
}
