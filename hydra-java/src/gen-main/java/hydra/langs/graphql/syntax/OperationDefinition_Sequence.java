// Note: this is an automatically generated file. Do not edit.

package hydra.langs.graphql.syntax;

import java.io.Serializable;

public class OperationDefinition_Sequence implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/graphql/syntax.OperationDefinition.Sequence");
  
  public final hydra.langs.graphql.syntax.OperationType operationType;
  
  public final java.util.Optional<hydra.langs.graphql.syntax.Name> name;
  
  public final java.util.Optional<hydra.langs.graphql.syntax.VariablesDefinition> variablesDefinition;
  
  public final java.util.Optional<hydra.langs.graphql.syntax.Directives> directives;
  
  public final hydra.langs.graphql.syntax.SelectionSet selectionSet;
  
  public OperationDefinition_Sequence (hydra.langs.graphql.syntax.OperationType operationType, java.util.Optional<hydra.langs.graphql.syntax.Name> name, java.util.Optional<hydra.langs.graphql.syntax.VariablesDefinition> variablesDefinition, java.util.Optional<hydra.langs.graphql.syntax.Directives> directives, hydra.langs.graphql.syntax.SelectionSet selectionSet) {
    if (operationType == null) {
      throw new IllegalArgumentException("null value for 'operationType' argument");
    }
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    if (variablesDefinition == null) {
      throw new IllegalArgumentException("null value for 'variablesDefinition' argument");
    }
    if (directives == null) {
      throw new IllegalArgumentException("null value for 'directives' argument");
    }
    if (selectionSet == null) {
      throw new IllegalArgumentException("null value for 'selectionSet' argument");
    }
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
  
  public OperationDefinition_Sequence withOperationType(hydra.langs.graphql.syntax.OperationType operationType) {
    if (operationType == null) {
      throw new IllegalArgumentException("null value for 'operationType' argument");
    }
    return new OperationDefinition_Sequence(operationType, name, variablesDefinition, directives, selectionSet);
  }
  
  public OperationDefinition_Sequence withName(java.util.Optional<hydra.langs.graphql.syntax.Name> name) {
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    return new OperationDefinition_Sequence(operationType, name, variablesDefinition, directives, selectionSet);
  }
  
  public OperationDefinition_Sequence withVariablesDefinition(java.util.Optional<hydra.langs.graphql.syntax.VariablesDefinition> variablesDefinition) {
    if (variablesDefinition == null) {
      throw new IllegalArgumentException("null value for 'variablesDefinition' argument");
    }
    return new OperationDefinition_Sequence(operationType, name, variablesDefinition, directives, selectionSet);
  }
  
  public OperationDefinition_Sequence withDirectives(java.util.Optional<hydra.langs.graphql.syntax.Directives> directives) {
    if (directives == null) {
      throw new IllegalArgumentException("null value for 'directives' argument");
    }
    return new OperationDefinition_Sequence(operationType, name, variablesDefinition, directives, selectionSet);
  }
  
  public OperationDefinition_Sequence withSelectionSet(hydra.langs.graphql.syntax.SelectionSet selectionSet) {
    if (selectionSet == null) {
      throw new IllegalArgumentException("null value for 'selectionSet' argument");
    }
    return new OperationDefinition_Sequence(operationType, name, variablesDefinition, directives, selectionSet);
  }
}