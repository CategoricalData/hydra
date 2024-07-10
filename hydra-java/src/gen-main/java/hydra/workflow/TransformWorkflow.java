// Note: this is an automatically generated file. Do not edit.

package hydra.workflow;

import java.io.Serializable;

/**
 * The specification of a workflow which takes a schema specification, reads data from a directory, and writes data to another directory
 */
public class TransformWorkflow implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/workflow.TransformWorkflow");
  
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
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    if (schemaSpec == null) {
      throw new IllegalArgumentException("null value for 'schemaSpec' argument");
    }
    if (srcDir == null) {
      throw new IllegalArgumentException("null value for 'srcDir' argument");
    }
    if (destDir == null) {
      throw new IllegalArgumentException("null value for 'destDir' argument");
    }
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
    return name.equals(o.name) && schemaSpec.equals(o.schemaSpec) && srcDir.equals(o.srcDir) && destDir.equals(o.destDir);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * schemaSpec.hashCode() + 5 * srcDir.hashCode() + 7 * destDir.hashCode();
  }
  
  public TransformWorkflow withName(String name) {
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    return new TransformWorkflow(name, schemaSpec, srcDir, destDir);
  }
  
  public TransformWorkflow withSchemaSpec(hydra.workflow.SchemaSpec schemaSpec) {
    if (schemaSpec == null) {
      throw new IllegalArgumentException("null value for 'schemaSpec' argument");
    }
    return new TransformWorkflow(name, schemaSpec, srcDir, destDir);
  }
  
  public TransformWorkflow withSrcDir(String srcDir) {
    if (srcDir == null) {
      throw new IllegalArgumentException("null value for 'srcDir' argument");
    }
    return new TransformWorkflow(name, schemaSpec, srcDir, destDir);
  }
  
  public TransformWorkflow withDestDir(String destDir) {
    if (destDir == null) {
      throw new IllegalArgumentException("null value for 'destDir' argument");
    }
    return new TransformWorkflow(name, schemaSpec, srcDir, destDir);
  }
}