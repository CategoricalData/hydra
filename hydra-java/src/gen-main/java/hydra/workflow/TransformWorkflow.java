// Note: this is an automatically generated file. Do not edit.

package hydra.workflow;

import java.io.Serializable;

/**
 * The specification of a workflow which takes a schema specification, reads data from a directory, and writes data to another directory
 */
public class TransformWorkflow implements Serializable, Comparable<TransformWorkflow> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.workflow.TransformWorkflow");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_SCHEMA_SPEC = new hydra.core.Name("schemaSpec");
  
  public static final hydra.core.Name FIELD_NAME_SRC_DIR = new hydra.core.Name("srcDir");
  
  public static final hydra.core.Name FIELD_NAME_DEST_DIR = new hydra.core.Name("destDir");
  
  /**
   * A descriptive name for the workflow
   */
  public final String name;
  
  /**
   * The schema specification
   */
  public final hydra.workflow.SchemaSpec schemaSpec;
  
  /**
   * The source directory
   */
  public final String srcDir;
  
  /**
   * The destination directory
   */
  public final String destDir;
  
  public TransformWorkflow (String name, hydra.workflow.SchemaSpec schemaSpec, String srcDir, String destDir) {
    this.name = name;
    this.schemaSpec = schemaSpec;
    this.srcDir = srcDir;
    this.destDir = destDir;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TransformWorkflow)) {
      return false;
    }
    TransformWorkflow o = (TransformWorkflow) (other);
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.schemaSpec,
      o.schemaSpec) && java.util.Objects.equals(
      this.srcDir,
      o.srcDir) && java.util.Objects.equals(
      this.destDir,
      o.destDir);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(schemaSpec) + 5 * java.util.Objects.hashCode(srcDir) + 7 * java.util.Objects.hashCode(destDir);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TransformWorkflow other) {
    int cmp = 0;
    cmp = ((Comparable) (name)).compareTo(other.name);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) (schemaSpec)).compareTo(other.schemaSpec);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) (srcDir)).compareTo(other.srcDir);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) (destDir)).compareTo(other.destDir);
  }
  
  public TransformWorkflow withName(String name) {
    return new TransformWorkflow(name, schemaSpec, srcDir, destDir);
  }
  
  public TransformWorkflow withSchemaSpec(hydra.workflow.SchemaSpec schemaSpec) {
    return new TransformWorkflow(name, schemaSpec, srcDir, destDir);
  }
  
  public TransformWorkflow withSrcDir(String srcDir) {
    return new TransformWorkflow(name, schemaSpec, srcDir, destDir);
  }
  
  public TransformWorkflow withDestDir(String destDir) {
    return new TransformWorkflow(name, schemaSpec, srcDir, destDir);
  }
}
