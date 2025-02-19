// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class GroupClause implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.GroupClause");
  
  public static final hydra.core.Name FIELD_NAME_GROUPED = new hydra.core.Name("grouped");
  
  public static final hydra.core.Name FIELD_NAME_BY = new hydra.core.Name("by");
  
  public final hydra.ext.csharp.syntax.Expression grouped;
  
  public final hydra.ext.csharp.syntax.Expression by;
  
  public GroupClause (hydra.ext.csharp.syntax.Expression grouped, hydra.ext.csharp.syntax.Expression by) {
    java.util.Objects.requireNonNull((grouped));
    java.util.Objects.requireNonNull((by));
    this.grouped = grouped;
    this.by = by;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof GroupClause)) {
      return false;
    }
    GroupClause o = (GroupClause) (other);
    return grouped.equals(o.grouped) && by.equals(o.by);
  }
  
  @Override
  public int hashCode() {
    return 2 * grouped.hashCode() + 3 * by.hashCode();
  }
  
  public GroupClause withGrouped(hydra.ext.csharp.syntax.Expression grouped) {
    java.util.Objects.requireNonNull((grouped));
    return new GroupClause(grouped, by);
  }
  
  public GroupClause withBy(hydra.ext.csharp.syntax.Expression by) {
    java.util.Objects.requireNonNull((by));
    return new GroupClause(grouped, by);
  }
}