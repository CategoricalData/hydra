// Note: this is an automatically generated file. Do not edit.

package hydra.adapt.utils;

/**
 * Additional adapter utilities, above and beyond the generated ones.
 */
public interface Utils {
  static <T0, T1> hydra.compute.Coder<T1, T1, T0, T0> bidirectional(java.util.function.Function<hydra.coders.CoderDirection, java.util.function.Function<T0, hydra.compute.Flow<T1, T0>>> f) {
    return (hydra.compute.Coder<T1, T1, T0, T0>) ((hydra.compute.Coder<T1, T1, T0, T0>) ((hydra.compute.Coder<T1, T1, T0, T0>) ((hydra.compute.Coder<T1, T1, T0, T0>) (new hydra.compute.Coder<T1, T1, T0, T0>((java.util.function.Function<T0, hydra.compute.Flow<T1, T0>>) (v1 -> (((f)).apply(new hydra.coders.CoderDirection.Encode(true))).apply((v1))), (java.util.function.Function<T0, hydra.compute.Flow<T1, T0>>) (v1 -> (((f)).apply(new hydra.coders.CoderDirection.Decode(true))).apply((v1))))))));
  }
  
  static <T0, T1, T2, T3, T4> hydra.compute.Flow<T1, hydra.compute.Adapter<T2, T3, T0, T0, T4, T4>> chooseAdapter(java.util.function.Function<T0, hydra.compute.Flow<T1, java.util.List<hydra.compute.Adapter<T2, T3, T0, T0, T4, T4>>>> alts, java.util.function.Function<T0, Boolean> supported, java.util.function.Function<T0, String> show, java.util.function.Function<T0, String> describe, T0 typ) {
    return hydra.lib.logic.IfElse.apply(
      ((supported)).apply((typ)),
      hydra.lib.flows.Pure.apply((hydra.compute.Adapter<T2, T3, T0, T0, T4, T4>) ((hydra.compute.Adapter<T2, T3, T0, T0, T4, T4>) ((hydra.compute.Adapter<T2, T3, T0, T0, T4, T4>) ((hydra.compute.Adapter<T2, T3, T0, T0, T4, T4>) ((hydra.compute.Adapter<T2, T3, T0, T0, T4, T4>) ((hydra.compute.Adapter<T2, T3, T0, T0, T4, T4>) (new hydra.compute.Adapter<T2, T3, T0, T0, T4, T4>(false, (typ), (typ), hydra.adapt.utils.Utils.<T2, T3, T4>idCoder())))))))),
      hydra.lib.flows.Bind.apply(
        ((alts)).apply((typ)),
        (java.util.function.Function<java.util.List<hydra.compute.Adapter<T2, T3, T0, T0, T4, T4>>, hydra.compute.Flow<T1, hydra.compute.Adapter<T2, T3, T0, T0, T4, T4>>>) (raw -> hydra.lib.logic.IfElse.apply(
          hydra.lib.lists.Null.apply(hydra.adapt.utils.Utils.<T2, T3, T0, T0, T4, T4>chooseAdapter_candidates(
            (raw),
            (supported))),
          hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat.apply(java.util.List.of(
            "no adapters found for ",
            ((describe)).apply((typ)),
            hydra.lib.logic.IfElse.apply(
              hydra.lib.lists.Null.apply((raw)),
              "",
              hydra.lib.strings.Cat.apply(java.util.List.of(
                " (discarded ",
                hydra.lib.literals.ShowInt32.apply(hydra.lib.lists.Length.apply((raw))),
                " unsupported candidate types: ",
                hydra.show.core.Core.<T0>list(
                  (show),
                  hydra.lib.lists.Map.apply(
                    (java.util.function.Function<hydra.compute.Adapter<T2, T3, T0, T0, T4, T4>, T0>) ((java.util.function.Function<hydra.compute.Adapter<T2, T3, T0, T0, T4, T4>, T0>) ((java.util.function.Function<hydra.compute.Adapter<T2, T3, T0, T0, T4, T4>, T0>) ((java.util.function.Function<hydra.compute.Adapter<T2, T3, T0, T0, T4, T4>, T0>) ((java.util.function.Function<hydra.compute.Adapter<T2, T3, T0, T0, T4, T4>, T0>) ((java.util.function.Function<hydra.compute.Adapter<T2, T3, T0, T0, T4, T4>, T0>) (projected -> projected.target)))))),
                    (raw))),
                ")"))),
            ". Original type: ",
            ((show)).apply((typ))))),
          hydra.lib.flows.Pure.apply(hydra.lib.lists.Head.apply(hydra.adapt.utils.Utils.<T2, T3, T0, T0, T4, T4>chooseAdapter_candidates(
            (raw),
            (supported))))))));
  }
  
  static <T0, T1, T2, T3, T4, T5> java.util.List<hydra.compute.Adapter<T0, T1, T2, T3, T4, T5>> chooseAdapter_candidates(java.util.List<hydra.compute.Adapter<T0, T1, T2, T3, T4, T5>> raw, java.util.function.Function<T3, Boolean> supported) {
    return hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.compute.Adapter<T0, T1, T2, T3, T4, T5>, Boolean>) (adapter -> ((supported)).apply(((java.util.function.Function<hydra.compute.Adapter<T0, T1, T2, T3, T4, T5>, T3>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, T2, T3, T4, T5>, T3>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, T2, T3, T4, T5>, T3>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, T2, T3, T4, T5>, T3>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, T2, T3, T4, T5>, T3>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, T2, T3, T4, T5>, T3>) (projected -> projected.target))))))).apply((adapter)))),
      (raw));
  }
  
  static <T0, T1, T2, T3, T4> hydra.compute.Coder<T0, T1, T2, T4> composeCoders(hydra.compute.Coder<T0, T1, T2, T3> c1, hydra.compute.Coder<T0, T1, T3, T4> c2) {
    return (hydra.compute.Coder<T0, T1, T2, T4>) ((hydra.compute.Coder<T0, T1, T2, T4>) ((hydra.compute.Coder<T0, T1, T2, T4>) ((hydra.compute.Coder<T0, T1, T2, T4>) (new hydra.compute.Coder<T0, T1, T2, T4>((java.util.function.Function<T2, hydra.compute.Flow<T0, T4>>) (a -> hydra.lib.flows.Bind.apply(
      (((java.util.function.Function<hydra.compute.Coder<T0, T1, T2, T3>, java.util.function.Function<T2, hydra.compute.Flow<T0, T3>>>) ((java.util.function.Function<hydra.compute.Coder<T0, T1, T2, T3>, java.util.function.Function<T2, hydra.compute.Flow<T0, T3>>>) ((java.util.function.Function<hydra.compute.Coder<T0, T1, T2, T3>, java.util.function.Function<T2, hydra.compute.Flow<T0, T3>>>) ((java.util.function.Function<hydra.compute.Coder<T0, T1, T2, T3>, java.util.function.Function<T2, hydra.compute.Flow<T0, T3>>>) (projected -> projected.encode))))).apply((c1))).apply((a)),
      (java.util.function.Function<T3, hydra.compute.Flow<T0, T4>>) (b1 -> (((java.util.function.Function<hydra.compute.Coder<T0, T1, T3, T4>, java.util.function.Function<T3, hydra.compute.Flow<T0, T4>>>) ((java.util.function.Function<hydra.compute.Coder<T0, T1, T3, T4>, java.util.function.Function<T3, hydra.compute.Flow<T0, T4>>>) ((java.util.function.Function<hydra.compute.Coder<T0, T1, T3, T4>, java.util.function.Function<T3, hydra.compute.Flow<T0, T4>>>) ((java.util.function.Function<hydra.compute.Coder<T0, T1, T3, T4>, java.util.function.Function<T3, hydra.compute.Flow<T0, T4>>>) (projected -> projected.encode))))).apply((c2))).apply((b1))))), (java.util.function.Function<T4, hydra.compute.Flow<T1, T2>>) (c -> hydra.lib.flows.Bind.apply(
      (((java.util.function.Function<hydra.compute.Coder<T0, T1, T3, T4>, java.util.function.Function<T4, hydra.compute.Flow<T1, T3>>>) ((java.util.function.Function<hydra.compute.Coder<T0, T1, T3, T4>, java.util.function.Function<T4, hydra.compute.Flow<T1, T3>>>) ((java.util.function.Function<hydra.compute.Coder<T0, T1, T3, T4>, java.util.function.Function<T4, hydra.compute.Flow<T1, T3>>>) ((java.util.function.Function<hydra.compute.Coder<T0, T1, T3, T4>, java.util.function.Function<T4, hydra.compute.Flow<T1, T3>>>) (projected -> projected.decode))))).apply((c2))).apply((c)),
      (java.util.function.Function<T3, hydra.compute.Flow<T1, T2>>) (b2 -> (((java.util.function.Function<hydra.compute.Coder<T0, T1, T2, T3>, java.util.function.Function<T3, hydra.compute.Flow<T1, T2>>>) ((java.util.function.Function<hydra.compute.Coder<T0, T1, T2, T3>, java.util.function.Function<T3, hydra.compute.Flow<T1, T2>>>) ((java.util.function.Function<hydra.compute.Coder<T0, T1, T2, T3>, java.util.function.Function<T3, hydra.compute.Flow<T1, T2>>>) ((java.util.function.Function<hydra.compute.Coder<T0, T1, T2, T3>, java.util.function.Function<T3, hydra.compute.Flow<T1, T2>>>) (projected -> projected.decode))))).apply((c1))).apply((b2))))))))));
  }
  
  static <T0, T1> hydra.compute.Flow<T0, T1> encodeDecode(hydra.coders.CoderDirection dir, hydra.compute.Coder<T0, T0, T1, T1> coder, T1 term) {
    return ((dir)).accept(new hydra.coders.CoderDirection.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, T1> visit(hydra.coders.CoderDirection.Encode ignored) {
        return (((java.util.function.Function<hydra.compute.Coder<T0, T0, T1, T1>, java.util.function.Function<T1, hydra.compute.Flow<T0, T1>>>) ((java.util.function.Function<hydra.compute.Coder<T0, T0, T1, T1>, java.util.function.Function<T1, hydra.compute.Flow<T0, T1>>>) ((java.util.function.Function<hydra.compute.Coder<T0, T0, T1, T1>, java.util.function.Function<T1, hydra.compute.Flow<T0, T1>>>) ((java.util.function.Function<hydra.compute.Coder<T0, T0, T1, T1>, java.util.function.Function<T1, hydra.compute.Flow<T0, T1>>>) (projected -> projected.encode))))).apply((coder))).apply((term));
      }
      
      @Override
      public hydra.compute.Flow<T0, T1> visit(hydra.coders.CoderDirection.Decode ignored) {
        return (((java.util.function.Function<hydra.compute.Coder<T0, T0, T1, T1>, java.util.function.Function<T1, hydra.compute.Flow<T0, T1>>>) ((java.util.function.Function<hydra.compute.Coder<T0, T0, T1, T1>, java.util.function.Function<T1, hydra.compute.Flow<T0, T1>>>) ((java.util.function.Function<hydra.compute.Coder<T0, T0, T1, T1>, java.util.function.Function<T1, hydra.compute.Flow<T0, T1>>>) ((java.util.function.Function<hydra.compute.Coder<T0, T0, T1, T1>, java.util.function.Function<T1, hydra.compute.Flow<T0, T1>>>) (projected -> projected.decode))))).apply((coder))).apply((term));
      }
    });
  }
  
  static Boolean floatTypeIsSupported(hydra.coders.LanguageConstraints constraints, hydra.core.FloatType ft) {
    return hydra.lib.sets.Member.apply(
      (ft),
      ((constraints)).floatTypes);
  }
  
  static <T0, T1, T2, T3> hydra.compute.Adapter<T1, T2, T0, T0, T3, T3> idAdapter(T0 t) {
    return (hydra.compute.Adapter<T1, T2, T0, T0, T3, T3>) ((hydra.compute.Adapter<T1, T2, T0, T0, T3, T3>) ((hydra.compute.Adapter<T1, T2, T0, T0, T3, T3>) ((hydra.compute.Adapter<T1, T2, T0, T0, T3, T3>) ((hydra.compute.Adapter<T1, T2, T0, T0, T3, T3>) ((hydra.compute.Adapter<T1, T2, T0, T0, T3, T3>) (new hydra.compute.Adapter<T1, T2, T0, T0, T3, T3>(false, (t), (t), hydra.adapt.utils.Utils.<T1, T2, T3>idCoder())))))));
  }
  
  static <T0, T1, T2> hydra.compute.Coder<T0, T1, T2, T2> idCoder() {
    return (hydra.compute.Coder<T0, T1, T2, T2>) ((hydra.compute.Coder<T0, T1, T2, T2>) ((hydra.compute.Coder<T0, T1, T2, T2>) ((hydra.compute.Coder<T0, T1, T2, T2>) (new hydra.compute.Coder<T0, T1, T2, T2>((java.util.function.Function<T2, hydra.compute.Flow<T0, T2>>) ((java.util.function.Function<T2, hydra.compute.Flow<T0, T2>>) ((hydra.lib.flows.Pure::apply))), (java.util.function.Function<T2, hydra.compute.Flow<T1, T2>>) ((java.util.function.Function<T2, hydra.compute.Flow<T1, T2>>) ((hydra.lib.flows.Pure::apply))))))));
  }
  
  static Boolean integerTypeIsSupported(hydra.coders.LanguageConstraints constraints, hydra.core.IntegerType it) {
    return hydra.lib.sets.Member.apply(
      (it),
      ((constraints)).integerTypes);
  }
  
  static Boolean literalTypeIsSupported(hydra.coders.LanguageConstraints constraints, hydra.core.LiteralType lt) {
    java.util.function.Function<hydra.core.LiteralType, Boolean> isSupported = (java.util.function.Function<hydra.core.LiteralType, Boolean>) (lt2 -> ((lt2)).accept(new hydra.core.LiteralType.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.core.LiteralType instance) {
        return true;
      }
      
      @Override
      public Boolean visit(hydra.core.LiteralType.Float_ ft) {
        return hydra.adapt.utils.Utils.floatTypeIsSupported(
          (constraints),
          ((ft)).value);
      }
      
      @Override
      public Boolean visit(hydra.core.LiteralType.Integer_ it) {
        return hydra.adapt.utils.Utils.integerTypeIsSupported(
          (constraints),
          ((it)).value);
      }
    }));
    return hydra.lib.logic.And.apply(
      hydra.lib.sets.Member.apply(
        hydra.reflect.Reflect.literalTypeVariant((lt)),
        ((constraints)).literalVariants),
      ((isSupported)).apply((lt)));
  }
  
  static String nameToFilePath(hydra.util.CaseConvention nsConv, hydra.util.CaseConvention localConv, hydra.module.FileExtension ext, hydra.core.Name name) {
    hydra.module.QualifiedName qualName = hydra.names.Names.qualifyName((name));
    String local = ((qualName)).local;
    hydra.util.Maybe<hydra.module.Namespace> ns = ((qualName)).namespace;
    java.util.function.Function<hydra.module.Namespace, String> nsToFilePath = (java.util.function.Function<hydra.module.Namespace, String>) (ns2 -> hydra.lib.strings.Intercalate.apply(
      "/",
      hydra.lib.lists.Map.apply(
        (java.util.function.Function<String, String>) (part -> hydra.formatting.Formatting.convertCase(
          new hydra.util.CaseConvention.Camel(true),
          (nsConv),
          (part))),
        hydra.lib.strings.SplitOn.apply(
          ".",
          ((ns2)).value))));
    String prefix = hydra.lib.maybes.Maybe.apply(
      "",
      (java.util.function.Function<hydra.module.Namespace, String>) (n -> hydra.lib.strings.Cat2.apply(
        ((nsToFilePath)).apply((n)),
        "/")),
      (ns));
    String suffix = hydra.formatting.Formatting.convertCase(
      new hydra.util.CaseConvention.Pascal(true),
      (localConv),
      (local));
    return hydra.lib.strings.Cat.apply(java.util.List.of(
      (prefix),
      (suffix),
      ".",
      ((ext)).value));
  }
  
  static Boolean typeIsSupported(hydra.coders.LanguageConstraints constraints, hydra.core.Type t) {
    hydra.core.Type base = hydra.rewriting.Rewriting.deannotateType((t));
    java.util.function.Function<hydra.core.Type, Boolean> isSupported = (java.util.function.Function<hydra.core.Type, Boolean>) (base2 -> ((base2)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public Boolean visit(hydra.core.Type.Annotated at) {
        return hydra.adapt.utils.Utils.typeIsSupported(
          (constraints),
          (((at)).value).body);
      }
      
      @Override
      public Boolean visit(hydra.core.Type.Application app) {
        return hydra.lib.logic.And.apply(
          hydra.adapt.utils.Utils.typeIsSupported(
            (constraints),
            (((app)).value).function),
          hydra.adapt.utils.Utils.typeIsSupported(
            (constraints),
            (((app)).value).argument));
      }
      
      @Override
      public Boolean visit(hydra.core.Type.Either et) {
        return hydra.lib.logic.And.apply(
          hydra.adapt.utils.Utils.typeIsSupported(
            (constraints),
            (((et)).value).left),
          hydra.adapt.utils.Utils.typeIsSupported(
            (constraints),
            (((et)).value).right));
      }
      
      @Override
      public Boolean visit(hydra.core.Type.Forall ft) {
        return hydra.adapt.utils.Utils.typeIsSupported(
          (constraints),
          (((ft)).value).body);
      }
      
      @Override
      public Boolean visit(hydra.core.Type.Function ft) {
        return hydra.lib.logic.And.apply(
          hydra.adapt.utils.Utils.typeIsSupported(
            (constraints),
            (((ft)).value).domain),
          hydra.adapt.utils.Utils.typeIsSupported(
            (constraints),
            (((ft)).value).codomain));
      }
      
      @Override
      public Boolean visit(hydra.core.Type.List lt) {
        return hydra.adapt.utils.Utils.typeIsSupported(
          (constraints),
          ((lt)).value);
      }
      
      @Override
      public Boolean visit(hydra.core.Type.Literal at) {
        return hydra.adapt.utils.Utils.literalTypeIsSupported(
          (constraints),
          ((at)).value);
      }
      
      @Override
      public Boolean visit(hydra.core.Type.Map mt) {
        return hydra.lib.logic.And.apply(
          hydra.adapt.utils.Utils.typeIsSupported(
            (constraints),
            (((mt)).value).keys),
          hydra.adapt.utils.Utils.typeIsSupported(
            (constraints),
            (((mt)).value).values));
      }
      
      @Override
      public Boolean visit(hydra.core.Type.Maybe ot) {
        return hydra.adapt.utils.Utils.typeIsSupported(
          (constraints),
          ((ot)).value);
      }
      
      @Override
      public Boolean visit(hydra.core.Type.Pair pt) {
        return hydra.lib.logic.And.apply(
          hydra.adapt.utils.Utils.typeIsSupported(
            (constraints),
            (((pt)).value).first),
          hydra.adapt.utils.Utils.typeIsSupported(
            (constraints),
            (((pt)).value).second));
      }
      
      @Override
      public Boolean visit(hydra.core.Type.Record rt) {
        return hydra.lib.lists.Foldl.apply(
          (java.util.function.Function<Boolean, java.util.function.Function<Boolean, Boolean>>) (p0 -> p1 -> hydra.lib.logic.And.apply(
            (p0),
            (p1))),
          true,
          hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.core.FieldType, Boolean>) (field -> hydra.adapt.utils.Utils.typeIsSupported(
              (constraints),
              ((field)).type)),
            (((rt)).value).fields));
      }
      
      @Override
      public Boolean visit(hydra.core.Type.Set st) {
        return hydra.adapt.utils.Utils.typeIsSupported(
          (constraints),
          ((st)).value);
      }
      
      @Override
      public Boolean visit(hydra.core.Type.Union rt) {
        return hydra.lib.lists.Foldl.apply(
          (java.util.function.Function<Boolean, java.util.function.Function<Boolean, Boolean>>) (p0 -> p1 -> hydra.lib.logic.And.apply(
            (p0),
            (p1))),
          true,
          hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.core.FieldType, Boolean>) (field -> hydra.adapt.utils.Utils.typeIsSupported(
              (constraints),
              ((field)).type)),
            (((rt)).value).fields));
      }
      
      @Override
      public Boolean visit(hydra.core.Type.Unit ignored) {
        return true;
      }
      
      @Override
      public Boolean visit(hydra.core.Type.Wrap wt) {
        return hydra.adapt.utils.Utils.typeIsSupported(
          (constraints),
          (((wt)).value).body);
      }
      
      @Override
      public Boolean visit(hydra.core.Type.Variable ignored) {
        return true;
      }
    }));
    java.util.function.Function<hydra.variants.TypeVariant, Boolean> isVariable = (java.util.function.Function<hydra.variants.TypeVariant, Boolean>) (v -> ((v)).accept(new hydra.variants.TypeVariant.PartialVisitor<>() {
      @Override
      public Boolean otherwise(hydra.variants.TypeVariant instance) {
        return false;
      }
      
      @Override
      public Boolean visit(hydra.variants.TypeVariant.Variable ignored) {
        return true;
      }
    }));
    java.util.function.Function<hydra.variants.TypeVariant, Boolean> isSupportedVariant = (java.util.function.Function<hydra.variants.TypeVariant, Boolean>) (v -> hydra.lib.logic.Or.apply(
      ((isVariable)).apply((v)),
      hydra.lib.sets.Member.apply(
        (v),
        ((constraints)).typeVariants)));
    return hydra.lib.logic.And.apply(
      (((constraints)).types).apply((base)),
      hydra.lib.logic.And.apply(
        ((isSupportedVariant)).apply(hydra.reflect.Reflect.typeVariant((base))),
        ((isSupported)).apply((base))));
  }
  
  static <T0, T1, T2, T3> hydra.compute.Coder<T1, T3, T0, T2> unidirectionalCoder(java.util.function.Function<T0, hydra.compute.Flow<T1, T2>> m) {
    return (hydra.compute.Coder<T1, T3, T0, T2>) ((hydra.compute.Coder<T1, T3, T0, T2>) ((hydra.compute.Coder<T1, T3, T0, T2>) ((hydra.compute.Coder<T1, T3, T0, T2>) (new hydra.compute.Coder<T1, T3, T0, T2>((m), (java.util.function.Function<T2, hydra.compute.Flow<T3, T0>>) (ignored -> hydra.lib.flows.Fail.apply("inbound mapping is unsupported")))))));
  }
}
