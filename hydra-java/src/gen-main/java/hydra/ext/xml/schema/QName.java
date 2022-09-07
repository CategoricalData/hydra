package hydra.ext.xml.schema;

public class QName {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/xml/schema.QName");
  
  public final String value;
  
  public QName (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof QName)) {
      return false;
    }
    QName o = (QName) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}