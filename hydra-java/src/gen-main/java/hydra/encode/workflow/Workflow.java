// Note: this is an automatically generated file. Do not edit.

package hydra.encode.workflow;

/**
 * Term encoders for hydra.workflow
 */
public interface Workflow {
  static hydra.core.Term hydraSchemaSpec(hydra.workflow.HydraSchemaSpec x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.workflow.HydraSchemaSpec"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("modules"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
        (hydra.encode.module.Module::module),
        ((x)).modules))),
      new hydra.core.Field(new hydra.core.Name("typeName"), hydra.encode.core.Core.name(((x)).typeName)))));
  }
  
  static hydra.core.Term schemaSpec(hydra.workflow.SchemaSpec v1) {
    return ((v1)).accept(new hydra.workflow.SchemaSpec.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.workflow.SchemaSpec.Hydra y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.workflow.SchemaSpec"), new hydra.core.Field(new hydra.core.Name("hydra"), hydra.encode.workflow.Workflow.hydraSchemaSpec(((y)).value))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.workflow.SchemaSpec.File y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.workflow.SchemaSpec"), new hydra.core.Field(new hydra.core.Name("file"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(((y)).value)))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.workflow.SchemaSpec.Provided y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.workflow.SchemaSpec"), new hydra.core.Field(new hydra.core.Name("provided"), new hydra.core.Term.Unit())));
      }
    });
  }
  
  static hydra.core.Term transformWorkflow(hydra.workflow.TransformWorkflow x) {
    return new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.workflow.TransformWorkflow"), java.util.List.of(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(((x)).name))),
      new hydra.core.Field(new hydra.core.Name("schemaSpec"), hydra.encode.workflow.Workflow.schemaSpec(((x)).schemaSpec)),
      new hydra.core.Field(new hydra.core.Name("srcDir"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(((x)).srcDir))),
      new hydra.core.Field(new hydra.core.Name("destDir"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(((x)).destDir))))));
  }
}
