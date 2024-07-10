// Note: this is an automatically generated file. Do not edit.

package hydra.langs.sql.ansi;

import java.io.Serializable;

public class TableDefinition implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/sql/ansi.TableDefinition");
  
  public final java.util.Optional<hydra.langs.sql.ansi.TableScope> scope;
  
  public final hydra.langs.sql.ansi.TableName name;
  
  public final hydra.langs.sql.ansi.TableContentsSource source;
  
  public final java.util.Optional<hydra.langs.sql.ansi.TableCommitAction> commitActions;
  
  public TableDefinition (java.util.Optional<hydra.langs.sql.ansi.TableScope> scope, hydra.langs.sql.ansi.TableName name, hydra.langs.sql.ansi.TableContentsSource source, java.util.Optional<hydra.langs.sql.ansi.TableCommitAction> commitActions) {
    if (scope == null) {
      throw new IllegalArgumentException("null value for 'scope' argument");
    }
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    if (source == null) {
      throw new IllegalArgumentException("null value for 'source' argument");
    }
    if (commitActions == null) {
      throw new IllegalArgumentException("null value for 'commitActions' argument");
    }
    this.scope = scope;
    this.name = name;
    this.source = source;
    this.commitActions = commitActions;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TableDefinition)) {
      return false;
    }
    TableDefinition o = (TableDefinition) (other);
    return scope.equals(o.scope) && name.equals(o.name) && source.equals(o.source) && commitActions.equals(o.commitActions);
  }
  
  @Override
  public int hashCode() {
    return 2 * scope.hashCode() + 3 * name.hashCode() + 5 * source.hashCode() + 7 * commitActions.hashCode();
  }
  
  public TableDefinition withScope(java.util.Optional<hydra.langs.sql.ansi.TableScope> scope) {
    if (scope == null) {
      throw new IllegalArgumentException("null value for 'scope' argument");
    }
    return new TableDefinition(scope, name, source, commitActions);
  }
  
  public TableDefinition withName(hydra.langs.sql.ansi.TableName name) {
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    return new TableDefinition(scope, name, source, commitActions);
  }
  
  public TableDefinition withSource(hydra.langs.sql.ansi.TableContentsSource source) {
    if (source == null) {
      throw new IllegalArgumentException("null value for 'source' argument");
    }
    return new TableDefinition(scope, name, source, commitActions);
  }
  
  public TableDefinition withCommitActions(java.util.Optional<hydra.langs.sql.ansi.TableCommitAction> commitActions) {
    if (commitActions == null) {
      throw new IllegalArgumentException("null value for 'commitActions' argument");
    }
    return new TableDefinition(scope, name, source, commitActions);
  }
}