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

import hydra.util.Opt;

import static hydra.dsl.Modules.element;
import static hydra.dsl.Modules.qname;


public class ModuleBuilder {
    private final Namespace namespace;
    private final Opt<String> description;
    private final List<Element> elements = new ArrayList<Element>();

    public ModuleBuilder(Namespace namespace, Opt<String> description) {
        this.namespace = namespace;
        this.description = description;
    }

    public ModuleBuilder(Namespace namespace, String description) {
        this(namespace, Opt.of(description));
    }

    public ModuleBuilder(Namespace namespace) {
        this(namespace, Opt.empty());
    }

    public ModuleBuilder(String namespace) {
        this(new Namespace(namespace));
    }

    public ModuleBuilder(String namespace, String description) {
        this(new Namespace(namespace), description);
    }

    public ModuleBuilder recordType(String localName, FieldType... fields) {
        return type(localName, new Type.Record(
                new RowType(qname(namespace, localName), Arrays.asList(fields))));
    }

    public ModuleBuilder type(String localName, Type type) {
        elements.add(element(qname(namespace, localName), type));
        return this;
    }

    public ModuleBuilder unionType(String localName, FieldType... fields) {
        return type(localName, new Type.Union(
                new RowType(qname(namespace, localName), Arrays.asList(fields))));
    }

    public Module build() {
        return new Module(namespace, elements, Collections.emptyList(), Collections.emptyList(), description);
    }
}
