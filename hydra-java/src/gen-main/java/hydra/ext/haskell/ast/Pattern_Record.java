package hydra.ext.haskell.ast;

public class Pattern_Record {
  public final Name name;
  
  public final java.util.List<PatternField> fields;
  
  public Pattern_Record (Name name, java.util.List<PatternField> fields) {
    this.name = name;
    this.fields = fields;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Pattern_Record)) {
      return false;
    }
    Pattern_Record o = (Pattern_Record) (other);
    return name.equals(o.name) && fields.equals(o.fields);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * fields.hashCode();
  }
  
  public Pattern_Record withName(Name name) {
    return new Pattern_Record(name, fields);
  }
  
  public Pattern_Record withFields(java.util.List<PatternField> fields) {
    return new Pattern_Record(name, fields);
  }
}