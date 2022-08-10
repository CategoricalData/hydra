package hydra.core;

/**
 * A labeled record or union type
 */
public class RowType<M> {
  public final Name typeName;
  
  public final java.util.List<FieldType<M>> fields;
  
  public RowType (Name typeName, java.util.List<FieldType<M>> fields) {
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
  
  public RowType withTypeName(Name typeName) {
    return new RowType(typeName, fields);
  }
  
  public RowType withFields(java.util.List<FieldType<M>> fields) {
    return new RowType(typeName, fields);
  }
}