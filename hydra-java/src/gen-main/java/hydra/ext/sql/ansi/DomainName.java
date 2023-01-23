package hydra.ext.sql.ansi;

public class DomainName {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.DomainName");
  
  public final String value;
  
  public DomainName (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DomainName)) {
      return false;
    }
    DomainName o = (DomainName) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}