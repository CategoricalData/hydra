// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala;

/**
 * Utility functions for constructing Scala AST nodes
 */
public interface Utils {
  static <T0> hydra.util.Maybe<hydra.core.Name> nameOfType(T0 cx, hydra.core.Type t) {
    return hydra.Rewriting.deannotateType(t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Maybe<hydra.core.Name> otherwise(hydra.core.Type instance) {
        return (hydra.util.Maybe<hydra.core.Name>) (hydra.util.Maybe.<hydra.core.Name>nothing());
      }

      @Override
      public hydra.util.Maybe<hydra.core.Name> visit(hydra.core.Type.Variable name) {
        return hydra.util.Maybe.just((name).value);
      }

      @Override
      public hydra.util.Maybe<hydra.core.Name> visit(hydra.core.Type.Forall ft) {
        return hydra.ext.scala.Utils.<T0>nameOfType(
          cx,
          (ft).value.body);
      }
    });
  }

  static String qualifyUnionFieldName(String dlft, hydra.util.Maybe<hydra.core.Name> sname, hydra.core.Name fname) {
    return hydra.lib.strings.Cat2.apply(
      hydra.lib.maybes.Maybe.applyLazy(
        () -> dlft,
        (java.util.function.Function<hydra.core.Name, String>) (n -> hydra.lib.strings.Cat2.apply(
          hydra.ext.scala.Utils.scalaTypeName(
            true,
            n),
          ".")),
        sname),
      hydra.ext.scala.Utils.scalaEscapeName((fname).value));
  }

  static String scalaTypeName(Boolean qualify, hydra.core.Name name) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.logic.Or.apply(
        qualify,
        hydra.lib.sets.Member.apply(
          hydra.Names.localNameOf(name),
          hydra.ext.scala.Utils.scalaReservedWords())),
      () -> (name).value,
      () -> hydra.Names.localNameOf(name));
  }

  static String scalaEscapeName(String s) {
    hydra.util.Lazy<String> sanitized = new hydra.util.Lazy<>(() -> hydra.lib.strings.FromList.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<Integer, Integer>) (c -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.equality.Equal.apply(
          c,
          39),
        () -> 95,
        () -> c)),
      hydra.lib.strings.ToList.apply(s))));
    hydra.util.Lazy<String> sanitized2 = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Equal.apply(
        sanitized.get(),
        "_"),
      () -> "_x",
      () -> sanitized.get()));
    hydra.util.Lazy<String> sanitized3 = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      hydra.lib.equality.Equal.apply(
        sanitized2.get(),
        "toString"),
      () -> "toString_",
      () -> sanitized2.get()));
    hydra.util.Lazy<Boolean> needsBackticks = new hydra.util.Lazy<>(() -> hydra.lib.logic.Or.apply(
      hydra.lib.sets.Member.apply(
        sanitized3.get(),
        hydra.ext.scala.Utils.scalaReservedWords()),
      hydra.lib.logic.And.apply(
        hydra.lib.equality.Gt.apply(
          hydra.lib.strings.Length.apply(sanitized3.get()),
          0),
        hydra.lib.equality.Equal.apply(
          hydra.lib.strings.CharAt.apply(
            hydra.lib.math.Sub.apply(
              hydra.lib.strings.Length.apply(sanitized3.get()),
              1),
            sanitized3.get()),
          95))));
    return hydra.lib.logic.IfElse.lazy(
      needsBackticks.get(),
      () -> hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
        "`",
        sanitized3.get(),
        "`")),
      () -> sanitized3.get());
  }

  static hydra.ext.scala.syntax.Data sapply(hydra.ext.scala.syntax.Data fun, hydra.util.ConsList<hydra.ext.scala.syntax.Data> args) {
    return new hydra.ext.scala.syntax.Data.Apply(new hydra.ext.scala.syntax.Data_Apply(fun, args));
  }

  static hydra.ext.scala.syntax.Data sassign(hydra.ext.scala.syntax.Data lhs, hydra.ext.scala.syntax.Data rhs) {
    return new hydra.ext.scala.syntax.Data.Assign(new hydra.ext.scala.syntax.Data_Assign(lhs, rhs));
  }

  static hydra.ext.scala.syntax.Data slambda(String v, hydra.ext.scala.syntax.Data body, hydra.util.Maybe<hydra.ext.scala.syntax.Type> sdom) {
    return new hydra.ext.scala.syntax.Data.FunctionData(new hydra.ext.scala.syntax.Data_FunctionData.Function(new hydra.ext.scala.syntax.Data_Function(hydra.util.ConsList.of(new hydra.ext.scala.syntax.Data_Param((hydra.util.ConsList<hydra.ext.scala.syntax.Mod>) (hydra.util.ConsList.<hydra.ext.scala.syntax.Mod>empty()), new hydra.ext.scala.syntax.Name.Value(v), sdom, (hydra.util.Maybe<hydra.ext.scala.syntax.Data>) (hydra.util.Maybe.<hydra.ext.scala.syntax.Data>nothing()))), body)));
  }

  static hydra.ext.scala.syntax.Data sname(String s) {
    return new hydra.ext.scala.syntax.Data.Ref(new hydra.ext.scala.syntax.Data_Ref.Name(new hydra.ext.scala.syntax.Data_Name(new hydra.ext.scala.syntax.PredefString(s))));
  }

  static hydra.ext.scala.syntax.Data sprim(hydra.core.Name name) {
    hydra.module.QualifiedName qname = hydra.Names.qualifyName(name);
    String local = hydra.ext.scala.Utils.scalaEscapeName((qname).local);
    hydra.util.Lazy<String> prefix = new hydra.util.Lazy<>(() -> hydra.lib.maybes.FromJust.apply((qname).namespace).value);
    return hydra.ext.scala.Utils.sname(hydra.lib.strings.Cat2.apply(
      hydra.lib.strings.Cat2.apply(
        prefix.get(),
        "."),
      local));
  }

  static hydra.ext.scala.syntax.Data sapplyTypes(hydra.ext.scala.syntax.Data fun, hydra.util.ConsList<hydra.ext.scala.syntax.Type> typeArgs) {
    java.util.function.Function<hydra.ext.scala.syntax.Type, String> typeToStr = (java.util.function.Function<hydra.ext.scala.syntax.Type, String>) (t -> hydra.ext.scala.Utils.typeToString(t));
    hydra.util.Lazy<hydra.util.ConsList<String>> typeStrings = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      typeToStr,
      typeArgs));
    String typeArgStr = hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      "[",
      hydra.lib.strings.Intercalate.apply(
        ", ",
        typeStrings.get()),
      "]"));
    return (fun).accept(new hydra.ext.scala.syntax.Data.PartialVisitor<>() {
      @Override
      public hydra.ext.scala.syntax.Data otherwise(hydra.ext.scala.syntax.Data instance) {
        return fun;
      }

      @Override
      public hydra.ext.scala.syntax.Data visit(hydra.ext.scala.syntax.Data.Ref ref) {
        return (ref).value.accept(new hydra.ext.scala.syntax.Data_Ref.PartialVisitor<>() {
          @Override
          public hydra.ext.scala.syntax.Data otherwise(hydra.ext.scala.syntax.Data_Ref instance) {
            return fun;
          }

          @Override
          public hydra.ext.scala.syntax.Data visit(hydra.ext.scala.syntax.Data_Ref.Name dn) {
            hydra.ext.scala.syntax.PredefString nameStr = (dn).value.value;
            String rawName = (nameStr).value;
            return hydra.ext.scala.Utils.sname(hydra.lib.strings.Cat2.apply(
              rawName,
              typeArgStr));
          }
        });
      }
    });
  }

  static String typeToString(hydra.ext.scala.syntax.Type t) {
    return (t).accept(new hydra.ext.scala.syntax.Type.PartialVisitor<>() {
      @Override
      public String otherwise(hydra.ext.scala.syntax.Type instance) {
        return "Any";
      }

      @Override
      public String visit(hydra.ext.scala.syntax.Type.Ref tr) {
        return (tr).value.accept(new hydra.ext.scala.syntax.Type_Ref.PartialVisitor<>() {
          @Override
          public String otherwise(hydra.ext.scala.syntax.Type_Ref instance) {
            return "Any";
          }

          @Override
          public String visit(hydra.ext.scala.syntax.Type_Ref.Name tn) {
            return (tn).value.value;
          }
        });
      }

      @Override
      public String visit(hydra.ext.scala.syntax.Type.Var tv) {
        return (tv).value.name.value;
      }

      @Override
      public String visit(hydra.ext.scala.syntax.Type.FunctionType ft) {
        return (ft).value.accept(new hydra.ext.scala.syntax.Type_FunctionType.PartialVisitor<>() {
          @Override
          public String otherwise(hydra.ext.scala.syntax.Type_FunctionType instance) {
            return "Any";
          }

          @Override
          public String visit(hydra.ext.scala.syntax.Type_FunctionType.Function fn) {
            hydra.util.Lazy<hydra.util.ConsList<String>> params = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
              hydra.ext.scala.Utils::typeToString,
              (fn).value.params));
            String res = hydra.ext.scala.Utils.typeToString((fn).value.res);
            return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
              "(",
              hydra.lib.strings.Intercalate.apply(
                ", ",
                params.get()),
              ") => ",
              res));
          }
        });
      }

      @Override
      public String visit(hydra.ext.scala.syntax.Type.Apply ta) {
        hydra.util.Lazy<hydra.util.ConsList<String>> argStrs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
          hydra.ext.scala.Utils::typeToString,
          (ta).value.args));
        String base = hydra.ext.scala.Utils.typeToString((ta).value.tpe);
        return hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
          base,
          "[",
          hydra.lib.strings.Intercalate.apply(
            ", ",
            argStrs.get()),
          "]"));
      }
    });
  }

  static hydra.ext.scala.syntax.Type stapply(hydra.ext.scala.syntax.Type t, hydra.util.ConsList<hydra.ext.scala.syntax.Type> args) {
    return new hydra.ext.scala.syntax.Type.Apply(new hydra.ext.scala.syntax.Type_Apply(t, args));
  }

  static hydra.ext.scala.syntax.Type stapply1(hydra.ext.scala.syntax.Type t1, hydra.ext.scala.syntax.Type t2) {
    return hydra.ext.scala.Utils.stapply(
      t1,
      hydra.util.ConsList.of(t2));
  }

  static hydra.ext.scala.syntax.Type stapply2(hydra.ext.scala.syntax.Type t1, hydra.ext.scala.syntax.Type t2, hydra.ext.scala.syntax.Type t3) {
    return hydra.ext.scala.Utils.stapply(
      t1,
      hydra.util.ConsList.of(
        t2,
        t3));
  }

  static hydra.ext.scala.syntax.Type_Param stparam(hydra.core.Name name) {
    String v = hydra.Formatting.capitalize((name).value);
    return new hydra.ext.scala.syntax.Type_Param((hydra.util.ConsList<hydra.ext.scala.syntax.Mod>) (hydra.util.ConsList.<hydra.ext.scala.syntax.Mod>empty()), new hydra.ext.scala.syntax.Name.Value(v), (hydra.util.ConsList<hydra.ext.scala.syntax.Type_Param>) (hydra.util.ConsList.<hydra.ext.scala.syntax.Type_Param>empty()), (hydra.util.ConsList<hydra.ext.scala.syntax.TypeBounds>) (hydra.util.ConsList.<hydra.ext.scala.syntax.TypeBounds>empty()), (hydra.util.ConsList<hydra.ext.scala.syntax.Type>) (hydra.util.ConsList.<hydra.ext.scala.syntax.Type>empty()), (hydra.util.ConsList<hydra.ext.scala.syntax.Type>) (hydra.util.ConsList.<hydra.ext.scala.syntax.Type>empty()));
  }

  static hydra.ext.scala.syntax.Type stref(String s) {
    return new hydra.ext.scala.syntax.Type.Ref(new hydra.ext.scala.syntax.Type_Ref.Name(new hydra.ext.scala.syntax.Type_Name(s)));
  }

  static hydra.ext.scala.syntax.Pat svar(hydra.core.Name name) {
    String v = (name).value;
    return new hydra.ext.scala.syntax.Pat.Var(new hydra.ext.scala.syntax.Pat_Var(new hydra.ext.scala.syntax.Data_Name(new hydra.ext.scala.syntax.PredefString(v))));
  }

  static hydra.util.PersistentSet<String> scalaReservedWords() {
    return hydra.ext.scala.Language.scalaReservedWords();
  }
}
