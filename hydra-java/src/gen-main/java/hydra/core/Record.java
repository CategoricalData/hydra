package hydra.core;

/**
 * A record, or labeled tuple; a map of field names to terms
 */
public class Record<M> {
  public final Name typeName;
  
  public final java.util.List<Field<M>> fields;
  
  public Record (Name typeName, java.util.List<Field<M>> fields) {
    this.typeName = typeName;
    this.fields = fields;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Record)) {
      return false;
    }
    Record o = (Record) (other);
    return typeName.equals(o.typeName) && fields.equals(o.fields);
  }
  
  @Override
  public int hashCode() {
    return 2 * typeName.hashCode() + 3 * fields.hashCode();
  }
  
  public Record withTypeName(Name typeName) {
    return new Record(typeName, fields);
  }
  
  public Record withFields(java.util.List<Field<M>> fields) {
    return new Record(typeName, fields);
  }
}