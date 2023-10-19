package hydra.dsl;

import hydra.core.FieldType;
import hydra.core.RowType;
import hydra.core.Type;
import hydra.graph.Element;
import hydra.module.Module;
import hydra.module.Namespace;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import static hydra.dsl.Modules.*;


public class ModuleBuilder<A> {
  private final Namespace namespace;
  private final Optional<String> description;
  private final List<Element<A>> elements = new ArrayList<Element<A>>();

  public ModuleBuilder(Namespace namespace, Optional<String> description) {
    this.namespace = namespace;
    this.description = description;
  }

  public ModuleBuilder(Namespace namespace, String description) {
    this(namespace, Optional.of(description));
  }

  public ModuleBuilder(Namespace namespace) {
    this(namespace, Optional.empty());
  }

  public ModuleBuilder(String namespace) {
    this(new Namespace(namespace));
  }

  public ModuleBuilder(String namespace, String description) {
    this(new Namespace(namespace), description);
  }

  public ModuleBuilder<A> recordType(String localName, FieldType<A>... fields) {
    return type(localName, new Type.Record<>(
        new RowType<>(qname(namespace, localName), Optional.empty(), Arrays.asList(fields))));
  }

  public ModuleBuilder<A> type(String localName, Type<A> type) {
    elements.add(element(qname(namespace, localName), type));
    return this;
  }

  public ModuleBuilder<A> unionType(String localName, FieldType<A>... fields) {
    return type(localName, new Type.Union<>(
        new RowType<>(qname(namespace, localName), Optional.empty(), Arrays.asList(fields))));
  }

  public Module<A> build() {
    return new Module<A>(namespace, elements, Collections.emptyList(), Collections.emptyList(), description);
  }
}
