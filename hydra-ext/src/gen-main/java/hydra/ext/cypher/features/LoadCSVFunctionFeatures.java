// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * Load CSV functions
 */
public class LoadCSVFunctionFeatures implements Serializable, Comparable<LoadCSVFunctionFeatures> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.features.LoadCSVFunctionFeatures");
  
  public static final hydra.core.Name FILE = new hydra.core.Name("file");
  
  public static final hydra.core.Name LINENUMBER = new hydra.core.Name("linenumber");
  
  /**
   * The file() function. Returns the absolute path of the file that LOAD CSV is using.
   */
  public final Boolean file;
  
  /**
   * The linenumber() function. Returns the line number that LOAD CSV is currently using.
   */
  public final Boolean linenumber;
  
  public LoadCSVFunctionFeatures (Boolean file, Boolean linenumber) {
    this.file = file;
    this.linenumber = linenumber;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LoadCSVFunctionFeatures)) {
      return false;
    }
    LoadCSVFunctionFeatures o = (LoadCSVFunctionFeatures) other;
    return java.util.Objects.equals(
      this.file,
      o.file) && java.util.Objects.equals(
      this.linenumber,
      o.linenumber);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(file) + 3 * java.util.Objects.hashCode(linenumber);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(LoadCSVFunctionFeatures other) {
    int cmp = 0;
    cmp = ((Comparable) file).compareTo(other.file);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) linenumber).compareTo(other.linenumber);
  }
  
  public LoadCSVFunctionFeatures withFile(Boolean file) {
    return new LoadCSVFunctionFeatures(file, linenumber);
  }
  
  public LoadCSVFunctionFeatures withLinenumber(Boolean linenumber) {
    return new LoadCSVFunctionFeatures(file, linenumber);
  }
}
