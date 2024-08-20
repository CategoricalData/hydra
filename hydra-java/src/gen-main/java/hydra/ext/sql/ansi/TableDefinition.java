// Note: this is an automatically generated file. Do not edit.

package hydra.ext.sql.ansi;

import java.io.Serializable;

public class TableDefinition implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/sql/ansi.TableDefinition");
  
  public static final hydra.core.Name FIELD_NAME_SCOPE = new hydra.core.Name("scope");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_SOURCE = new hydra.core.Name("source");
  
  public static final hydra.core.Name FIELD_NAME_COMMIT_ACTIONS = new hydra.core.Name("commitActions");
  
  public final hydra.util.Opt<hydra.ext.sql.ansi.TableScope> scope;
  
  public final hydra.ext.sql.ansi.TableName name;
  
  public final hydra.ext.sql.ansi.TableContentsSource source;
  
  public final hydra.util.Opt<hydra.ext.sql.ansi.TableCommitAction> commitActions;
  
  public TableDefinition (hydra.util.Opt<hydra.ext.sql.ansi.TableScope> scope, hydra.ext.sql.ansi.TableName name, hydra.ext.sql.ansi.TableContentsSource source, hydra.util.Opt<hydra.ext.sql.ansi.TableCommitAction> commitActions) {
    java.util.Objects.requireNonNull((scope));
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((source));
    java.util.Objects.requireNonNull((commitActions));
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
  
  public TableDefinition withScope(hydra.util.Opt<hydra.ext.sql.ansi.TableScope> scope) {
    java.util.Objects.requireNonNull((scope));
    return new TableDefinition(scope, name, source, commitActions);
  }
  
  public TableDefinition withName(hydra.ext.sql.ansi.TableName name) {
    java.util.Objects.requireNonNull((name));
    return new TableDefinition(scope, name, source, commitActions);
  }
  
  public TableDefinition withSource(hydra.ext.sql.ansi.TableContentsSource source) {
    java.util.Objects.requireNonNull((source));
    return new TableDefinition(scope, name, source, commitActions);
  }
  
  public TableDefinition withCommitActions(hydra.util.Opt<hydra.ext.sql.ansi.TableCommitAction> commitActions) {
    java.util.Objects.requireNonNull((commitActions));
    return new TableDefinition(scope, name, source, commitActions);
  }
}
