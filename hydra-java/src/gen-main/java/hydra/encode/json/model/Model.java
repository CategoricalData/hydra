// Note: this is an automatically generated file. Do not edit.

package hydra.encode.json.model;

/**
 * Term encoders for hydra.json.model
 */
public interface Model {
  static hydra.core.Term value(hydra.json.model.Value v1) {
    return ((v1)).accept(new hydra.json.model.Value.PartialVisitor<>() {
      @Override
      public hydra.core.Term visit(hydra.json.model.Value.Array y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.json.model.Value"), new hydra.core.Field(new hydra.core.Name("array"), new hydra.core.Term.List(hydra.lib.lists.Map.apply(
          (hydra.encode.json.model.Model::value),
          ((y)).value)))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.json.model.Value.Boolean_ y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.json.model.Value"), new hydra.core.Field(new hydra.core.Name("boolean"), new hydra.core.Term.Literal(new hydra.core.Literal.Boolean_(((y)).value)))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.json.model.Value.Null y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.json.model.Value"), new hydra.core.Field(new hydra.core.Name("null"), new hydra.core.Term.Unit(true))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.json.model.Value.Number_ y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.json.model.Value"), new hydra.core.Field(new hydra.core.Name("number"), new hydra.core.Term.Literal(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Bigfloat(((y)).value))))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.json.model.Value.Object_ y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.json.model.Value"), new hydra.core.Field(new hydra.core.Name("object"), new hydra.core.Term.Map(hydra.lib.maps.Bimap.apply(
          (java.util.function.Function<String, hydra.core.Term>) (x -> new hydra.core.Term.Literal(new hydra.core.Literal.String_((x)))),
          (hydra.encode.json.model.Model::value),
          ((y)).value)))));
      }
      
      @Override
      public hydra.core.Term visit(hydra.json.model.Value.String_ y) {
        return new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.json.model.Value"), new hydra.core.Field(new hydra.core.Name("string"), new hydra.core.Term.Literal(new hydra.core.Literal.String_(((y)).value)))));
      }
    });
  }
}
