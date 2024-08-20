// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * Load CSV functions
 */
public class LoadCSVFunctionFeatures implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/cypher/features.LoadCSVFunctionFeatures");
  
  public static final hydra.core.Name FIELD_NAME_FILE = new hydra.core.Name("file");
  
  public static final hydra.core.Name FIELD_NAME_LINENUMBER = new hydra.core.Name("linenumber");
  
  /**
   * The file() function. Returns the absolute path of the file that LOAD CSV is using.
   */
  public final Boolean file;
  
  /**
   * The linenumber() function. Returns the line number that LOAD CSV is currently using.
   */
  public final Boolean linenumber;
  
  public LoadCSVFunctionFeatures (Boolean file, Boolean linenumber) {
    java.util.Objects.requireNonNull((file));
    java.util.Objects.requireNonNull((linenumber));
    this.file = file;
    this.linenumber = linenumber;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LoadCSVFunctionFeatures)) {
      return false;
    }
    LoadCSVFunctionFeatures o = (LoadCSVFunctionFeatures) (other);
    return file.equals(o.file) && linenumber.equals(o.linenumber);
  }
  
  @Override
  public int hashCode() {
    return 2 * file.hashCode() + 3 * linenumber.hashCode();
  }
  
  public LoadCSVFunctionFeatures withFile(Boolean file) {
    java.util.Objects.requireNonNull((file));
    return new LoadCSVFunctionFeatures(file, linenumber);
  }
  
  public LoadCSVFunctionFeatures withLinenumber(Boolean linenumber) {
    java.util.Objects.requireNonNull((linenumber));
    return new LoadCSVFunctionFeatures(file, linenumber);
  }
}