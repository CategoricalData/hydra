package hydra.dsl;

import hydra.tier1.Tier1;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.Type;
import hydra.graph.Element;
import hydra.module.Module;
import hydra.module.Namespace;
import hydra.module.QualifiedName;
import java.util.Optional;

import static hydra.coreEncoding.CoreEncoding.coreEncodeType;


public class Modules {
  public static <A> Element<A> element(Module<A> module, String localName, Term<A> data) {
    Name name = Tier1.unqualifyName(new QualifiedName(Optional.of(module.namespace), localName));
    return new Element<>(name, data);
  }

  public static <A> Element<A> element(Module<A> module, String localName, Type<A> type) {
    return element(module, localName, coreEncodeType(type));
  }

  public static <A> Element<A> element(Name name, Type<A> type) {
    return new Element<>(name, coreEncodeType(type));
  }

  public static <A> ModuleBuilder<A> module(Namespace namespace) {
    return new ModuleBuilder<>(namespace);
  }

  public static <A> ModuleBuilder<A> module(Namespace namespace, String description) {
    return new ModuleBuilder<>(namespace, description);
  }

  public static <A> Name qname(Module<A> module, String localName) {
    return qname(module.namespace, localName);
  }

  public static Name qname(Namespace ns, String localName) {
    return Tier1.unqualifyName(new QualifiedName(Optional.of(ns), localName));
  }
}
