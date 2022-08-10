package hydra.ext.haskell.ast;

/**
 * A record-style data constructor
 */
public class Constructor_Record {
  public final Name name;
  
  public final java.util.List<FieldWithComments> fields;
  
  public Constructor_Record (Name name, java.util.List<FieldWithComments> fields) {
    this.name = name;
    this.fields = fields;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Constructor_Record)) {
      return false;
    }
    Constructor_Record o = (Constructor_Record) (other);
    return name.equals(o.name) && fields.equals(o.fields);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * fields.hashCode();
  }
  
  public Constructor_Record withName(Name name) {
    return new Constructor_Record(name, fields);
  }
  
  public Constructor_Record withFields(java.util.List<FieldWithComments> fields) {
    return new Constructor_Record(name, fields);
  }
}