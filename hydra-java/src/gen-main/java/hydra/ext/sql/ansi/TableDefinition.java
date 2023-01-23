package hydra.ext.sql.ansi;

public class TableDefinition {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.TableDefinition");
  
  public final java.util.Optional<hydra.ext.sql.ansi.TableScope> scope;
  
  public final hydra.ext.sql.ansi.TableName name;
  
  public final hydra.ext.sql.ansi.TableContentsSource source;
  
  public final java.util.Optional<hydra.ext.sql.ansi.TableCommitAction> commitActions;
  
  public TableDefinition (java.util.Optional<hydra.ext.sql.ansi.TableScope> scope, hydra.ext.sql.ansi.TableName name, hydra.ext.sql.ansi.TableContentsSource source, java.util.Optional<hydra.ext.sql.ansi.TableCommitAction> commitActions) {
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
  
  public TableDefinition withScope(java.util.Optional<hydra.ext.sql.ansi.TableScope> scope) {
    return new TableDefinition(scope, name, source, commitActions);
  }
  
  public TableDefinition withName(hydra.ext.sql.ansi.TableName name) {
    return new TableDefinition(scope, name, source, commitActions);
  }
  
  public TableDefinition withSource(hydra.ext.sql.ansi.TableContentsSource source) {
    return new TableDefinition(scope, name, source, commitActions);
  }
  
  public TableDefinition withCommitActions(java.util.Optional<hydra.ext.sql.ansi.TableCommitAction> commitActions) {
    return new TableDefinition(scope, name, source, commitActions);
  }
}