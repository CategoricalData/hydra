package hydra.ext.rdf.syntax;

/**
 * Stand-in for rdfs:Class
 */
public class RdfsClass {
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