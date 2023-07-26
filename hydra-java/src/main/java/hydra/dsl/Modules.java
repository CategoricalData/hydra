package hydra.dsl;

import hydra.basics.Basics;
import hydra.core.FieldType;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.Type;
import hydra.graph.Element;
import hydra.module.Namespace;
import hydra.module.Module;
import hydra.module.QualifiedName;
import java.util.Arrays;
import java.util.Collections;
import java.util.Optional;
import static hydra.coreEncoding.CoreEncoding.*;


public class Modules {
  public static <A> Element<A> element(Module<A> module, String localName, Term<A> data) {
    Name name = Basics.unqualifyName(new QualifiedName(Optional.of(module.namespace), localName));
    return new Element<>(name, data);
  }

  public static <A> Element<A> element(Module<A> module, String localName, Type<A> type) {
    return element(module, localName, coreEncodeType(type));
  }

  public static <A> Element<A> element(Name name, Type<A> type) {
    return new Element<>(name, coreEncodeType(type));
  }

  public static <A> Module<A> emptyModule(String ns, String description, Module<A>... dependencies) {
    return new Module<>(new Namespace(ns), Collections.emptyList(), Arrays.asList(dependencies), Optional.of(description));
  }

  public static <A> Name qname(Module<A> module, String localName) {
    return Basics.unqualifyName(new QualifiedName(Optional.of(module.namespace), localName));
  }
  public static <A> Element<A> recordElement(Module<A> module, String localName, FieldType<A>... fields) {
    Name name = qname(module, localName);
    return element(name, Types.record(name, fields));
  }
}
