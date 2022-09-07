package hydra.core;

/**
 * A labeled record or union type
 */
public class RowType<M> {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/core.RowType");
  
  public final hydra.core.Name typeName;
  
  public final java.util.List<hydra.core.FieldType<M>> fields;
  
  public RowType (hydra.core.Name typeName, java.util.List<hydra.core.FieldType<M>> fields) {
    this.typeName = typeName;
    this.fields = fields;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RowType)) {
      return false;
    }
    RowType o = (RowType) (other);
    return typeName.equals(o.typeName) && fields.equals(o.fields);
  }
  
  @Override
  public int hashCode() {
    return 2 * typeName.hashCode() + 3 * fields.hashCode();
  }
  
  public RowType withTypeName(hydra.core.Name typeName) {
    return new RowType(typeName, fields);
  }
  
  public RowType withFields(java.util.List<hydra.core.FieldType<M>> fields) {
    return new RowType(typeName, fields);
  }
}