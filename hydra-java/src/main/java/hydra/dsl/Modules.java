package hydra.dsl;

// import hydra.tier1.Tier1; // TODO: restore when kernel terms modules are generated
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.Type;
// import hydra.graph.Element; // TODO: restore when kernel terms modules are generated
import hydra.module.Module;
import hydra.module.Namespace;
import hydra.module.QualifiedName;
import hydra.util.Opt;

// import static hydra.coreEncoding.CoreEncoding.coreEncodeType; // TODO: restore when kernel terms modules are generated


// TODO: restore when Element, Tier1, and coreEncoding are generated
public class Modules {
//    public static Element element(Module module, String localName, Term data) {
//        Name name = Tier1.unqualifyName(new QualifiedName(Opt.of(module.namespace), localName));
//        return new Element(name, data);
//    }
//
//    public static Element element(Module module, String localName, Type type) {
//        return element(module, localName, coreEncodeType(type));
//    }
//
//    public static Element element(Name name, Type type) {
//        return new Element(name, coreEncodeType(type));
//    }

    public static ModuleBuilder module(Namespace namespace) {
        return new ModuleBuilder(namespace);
    }

    public static ModuleBuilder module(Namespace namespace, String description) {
        return new ModuleBuilder(namespace, description);
    }

//    public static Name qname(Module module, String localName) {
//        return qname(module.namespace, localName);
//    }
//
//    public static Name qname(Namespace ns, String localName) {
//        return Tier1.unqualifyName(new QualifiedName(Opt.of(ns), localName));
//    }
}
