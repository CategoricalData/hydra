package hydra.ext.rdf.syntax;

/**
 * Stand-in for rdfs:Class
 */
public class RdfsClass {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/rdf/syntax.RdfsClass");
  
  public RdfsClass () {
  
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RdfsClass)) {
      return false;
    }
    RdfsClass o = (RdfsClass) (other);
    return true;
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
}