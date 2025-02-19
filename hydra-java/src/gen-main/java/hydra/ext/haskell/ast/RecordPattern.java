// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

public class RecordPattern implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.RecordPattern");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_FIELDS = new hydra.core.Name("fields");
  
  public final hydra.ext.haskell.ast.Name name;
  
  public final java.util.List<hydra.ext.haskell.ast.PatternField> fields;
  
  public RecordPattern (hydra.ext.haskell.ast.Name name, java.util.List<hydra.ext.haskell.ast.PatternField> fields) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((fields));
    this.name = name;
    this.fields = fields;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RecordPattern)) {
      return false;
    }
    RecordPattern o = (RecordPattern) (other);
    return name.equals(o.name) && fields.equals(o.fields);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * fields.hashCode();
  }
  
  public RecordPattern withName(hydra.ext.haskell.ast.Name name) {
    java.util.Objects.requireNonNull((name));
    return new RecordPattern(name, fields);
  }
  
  public RecordPattern withFields(java.util.List<hydra.ext.haskell.ast.PatternField> fields) {
    java.util.Objects.requireNonNull((fields));
    return new RecordPattern(name, fields);
  }
}