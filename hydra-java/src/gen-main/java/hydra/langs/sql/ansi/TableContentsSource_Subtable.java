// Note: this is an automatically generated file. Do not edit.

package hydra.langs.sql.ansi;

import java.io.Serializable;

public class TableContentsSource_Subtable implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/sql/ansi.TableContentsSource.Subtable");
  
  public final hydra.langs.sql.ansi.PathResolvedUserDefinedTypeName type;
  
  public final hydra.util.Opt<hydra.langs.sql.ansi.SubtableClause> subtable;
  
  public final hydra.util.Opt<hydra.langs.sql.ansi.TableElementList> elements;
  
  public TableContentsSource_Subtable (hydra.langs.sql.ansi.PathResolvedUserDefinedTypeName type, hydra.util.Opt<hydra.langs.sql.ansi.SubtableClause> subtable, hydra.util.Opt<hydra.langs.sql.ansi.TableElementList> elements) {
    if (type == null) {
      throw new IllegalArgumentException("null value for 'type' argument");
    }
    if (subtable == null) {
      throw new IllegalArgumentException("null value for 'subtable' argument");
    }
    if (elements == null) {
      throw new IllegalArgumentException("null value for 'elements' argument");
    }
    this.type = type;
    this.subtable = subtable;
    this.elements = elements;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TableContentsSource_Subtable)) {
      return false;
    }
    TableContentsSource_Subtable o = (TableContentsSource_Subtable) (other);
    return type.equals(o.type) && subtable.equals(o.subtable) && elements.equals(o.elements);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * subtable.hashCode() + 5 * elements.hashCode();
  }
  
  public TableContentsSource_Subtable withType(hydra.langs.sql.ansi.PathResolvedUserDefinedTypeName type) {
    if (type == null) {
      throw new IllegalArgumentException("null value for 'type' argument");
    }
    return new TableContentsSource_Subtable(type, subtable, elements);
  }
  
  public TableContentsSource_Subtable withSubtable(hydra.util.Opt<hydra.langs.sql.ansi.SubtableClause> subtable) {
    if (subtable == null) {
      throw new IllegalArgumentException("null value for 'subtable' argument");
    }
    return new TableContentsSource_Subtable(type, subtable, elements);
  }
  
  public TableContentsSource_Subtable withElements(hydra.util.Opt<hydra.langs.sql.ansi.TableElementList> elements) {
    if (elements == null) {
      throw new IllegalArgumentException("null value for 'elements' argument");
    }
    return new TableContentsSource_Subtable(type, subtable, elements);
  }
}