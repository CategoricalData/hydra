package hydra.dsl;

import hydra.core.FieldType;
import hydra.core.RowType;
import hydra.core.Type;
// import hydra.graph.Element; // TODO: restore when kernel terms modules are generated
import hydra.module.Module;
import hydra.module.Namespace;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import hydra.util.Opt;

// import static hydra.dsl.Modules.element; // TODO: restore when kernel terms modules are generated
// import static hydra.dsl.Modules.qname; // TODO: restore when kernel terms modules are generated


/**
 * A builder for creating Hydra modules.
 * TODO: restore when Element and Modules.element/qname are available
 */
public class ModuleBuilder {
    private final Namespace namespace;
    private final Opt<String> description;
    // private final List<Element> elements = new ArrayList<Element>();

    /**
     * Create a module builder with a namespace and optional description.
     *
     * @param namespace the namespace for the module
     * @param description the optional description for the module
     */
    public ModuleBuilder(Namespace namespace, Opt<String> description) {
        this.namespace = namespace;
        this.description = description;
    }

    /**
     * Create a module builder with a namespace and description.
     *
     * @param namespace the namespace for the module
     * @param description the description for the module
     */
    public ModuleBuilder(Namespace namespace, String description) {
        this(namespace, Opt.of(description));
    }

    /**
     * Create a module builder with a namespace and no description.
     *
     * @param namespace the namespace for the module
     */
    public ModuleBuilder(Namespace namespace) {
        this(namespace, Opt.empty());
    }

    /**
     * Create a module builder from a namespace string.
     *
     * @param namespace the namespace string for the module
     */
    public ModuleBuilder(String namespace) {
        this(new Namespace(namespace));
    }

    /**
     * Create a module builder from a namespace string and description.
     *
     * @param namespace the namespace string for the module
     * @param description the description for the module
     */
    public ModuleBuilder(String namespace, String description) {
        this(new Namespace(namespace), description);
    }

//    public ModuleBuilder recordType(String localName, FieldType... fields) {
//        return type(localName, new Type.Record(
//                new RowType(qname(namespace, localName), Arrays.asList(fields))));
//    }
//
//    public ModuleBuilder type(String localName, Type type) {
//        elements.add(element(qname(namespace, localName), type));
//        return this;
//    }
//
//    public ModuleBuilder unionType(String localName, FieldType... fields) {
//        return type(localName, new Type.Union(
//                new RowType(qname(namespace, localName), Arrays.asList(fields))));
//    }
//
//    public Module build() {
//        return new Module(namespace, elements, Collections.emptyList(), Collections.emptyList(), description);
//    }
}
