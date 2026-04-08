package hydra.predicates

import hydra.coders.*

import hydra.context.*

import hydra.core.*

import hydra.errors.*

import hydra.graph.*

import hydra.variants.*

def isComplexBinding(tc: hydra.graph.Graph)(b: hydra.core.Binding): Boolean =
  {
  lazy val term: hydra.core.Term = (b.term)
  lazy val mts: Option[hydra.core.TypeScheme] = (b.`type`)
  hydra.lib.maybes.cases[hydra.core.TypeScheme, Boolean](mts)(hydra.predicates.isComplexTerm(tc)(term))((ts: hydra.core.TypeScheme) =>
    {
    lazy val isPolymorphic: Boolean = hydra.lib.logic.not(hydra.lib.lists.`null`[hydra.core.Name](ts.variables))
    {
      lazy val isNonNullary: Boolean = hydra.lib.equality.gt[Int](hydra.arity.typeArity(ts.`type`))(0)
      {
        lazy val isComplex: Boolean = hydra.predicates.isComplexTerm(tc)(term)
        hydra.lib.logic.or(hydra.lib.logic.or(isPolymorphic)(isNonNullary))(isComplex)
      }
    }
  })
}

def isComplexTerm(tc: hydra.graph.Graph)(t: hydra.core.Term): Boolean =
  t match
  case hydra.core.Term.let(v_Term_let__) => true
  case hydra.core.Term.typeApplication(v_Term_typeApplication__) => true
  case hydra.core.Term.typeLambda(v_Term_typeLambda__) => true
  case hydra.core.Term.variable(v_Term_variable_name) => hydra.predicates.isComplexVariable(tc)(v_Term_variable_name)
  case _ => hydra.lib.lists.foldl[Boolean, hydra.core.Term]((b: Boolean) =>
    (sub: hydra.core.Term) =>
    hydra.lib.logic.or(b)(hydra.predicates.isComplexTerm(tc)(sub)))(false)(hydra.rewriting.subterms(t))

def isComplexVariable(tc: hydra.graph.Graph)(name: hydra.core.Name): Boolean =
  {
  lazy val metaLookup: Option[hydra.core.Term] = hydra.lib.maps.lookup[hydra.core.Name, hydra.core.Term](name)(tc.metadata)
  hydra.lib.logic.ifElse[Boolean](hydra.lib.maybes.isJust[hydra.core.Term](metaLookup))(true)(hydra.lib.logic.ifElse[Boolean](hydra.lib.sets.member[hydra.core.Name](name)(tc.lambdaVariables))(true)({
    lazy val typeLookup: Option[hydra.core.TypeScheme] = hydra.lib.maps.lookup[hydra.core.Name, hydra.core.TypeScheme](name)(tc.boundTypes)
    hydra.lib.maybes.maybe[Boolean, hydra.core.TypeScheme]({
      lazy val primLookup: Option[hydra.graph.Primitive] = hydra.lib.maps.lookup[hydra.core.Name, hydra.graph.Primitive](name)(tc.primitives)
      hydra.lib.maybes.maybe[Boolean, hydra.graph.Primitive](true)((prim: hydra.graph.Primitive) =>
        hydra.lib.equality.gt[Int](hydra.arity.typeSchemeArity(prim.`type`))(0))(primLookup)
    })((ts: hydra.core.TypeScheme) =>
      hydra.lib.equality.gt[Int](hydra.arity.typeSchemeArity(ts))(0))(typeLookup)
  }))
}

def isEncodedTerm(t: hydra.core.Term): Boolean =
  hydra.strip.deannotateTerm(t) match
  case hydra.core.Term.application(v_Term_application_a) => hydra.predicates.isEncodedTerm(v_Term_application_a.function)
  case hydra.core.Term.union(v_Term_union_i) => hydra.lib.equality.equal[scala.Predef.String]("hydra.core.Term")(v_Term_union_i.typeName)
  case _ => false

def isEncodedType(t: hydra.core.Term): Boolean =
  hydra.strip.deannotateTerm(t) match
  case hydra.core.Term.application(v_Term_application_a) => hydra.predicates.isEncodedType(v_Term_application_a.function)
  case hydra.core.Term.union(v_Term_union_i) => hydra.lib.equality.equal[scala.Predef.String]("hydra.core.Type")(v_Term_union_i.typeName)
  case _ => false

def isEnumRowType(rt: Seq[hydra.core.FieldType]): Boolean =
  hydra.lib.lists.foldl[Boolean, Boolean](hydra.lib.logic.and)(true)(hydra.lib.lists.map[hydra.core.FieldType, Boolean]((f: hydra.core.FieldType) =>
  hydra.predicates.isUnitType(hydra.strip.deannotateType(f.`type`)))(rt))

def isEnumType(typ: hydra.core.Type): Boolean =
  hydra.strip.deannotateType(typ) match
  case hydra.core.Type.union(v_Type_union_rt) => hydra.predicates.isEnumRowType(v_Type_union_rt)
  case _ => false

def isNominalType(typ: hydra.core.Type): Boolean =
  hydra.strip.deannotateType(typ) match
  case hydra.core.Type.record(v_Type_record_rt) => true
  case hydra.core.Type.union(v_Type_union_rt) => true
  case hydra.core.Type.wrap(v_Type_wrap_wt) => true
  case hydra.core.Type.forall(v_Type_forall_fa) => hydra.predicates.isNominalType(v_Type_forall_fa.body)
  case _ => false

def isSerializable(cx: hydra.context.Context)(graph: hydra.graph.Graph)(el: hydra.core.Binding): Either[hydra.errors.Error, Boolean] =
  {
  def variants(typ: hydra.core.Type): Seq[hydra.variants.TypeVariant] =
    hydra.lib.lists.map[hydra.core.Type, hydra.variants.TypeVariant](hydra.reflect.typeVariant)(hydra.rewriting.foldOverType(hydra.coders.TraversalOrder.pre)((m: Seq[hydra.core.Type]) =>
    (t: hydra.core.Type) => hydra.lib.lists.cons[hydra.core.Type](t)(m))(Seq())(typ))
  hydra.lib.eithers.map[Map[hydra.core.Name, hydra.core.Type], Boolean, hydra.errors.Error]((deps: Map[hydra.core.Name, hydra.core.Type]) =>
    {
    lazy val allVariants: scala.collection.immutable.Set[hydra.variants.TypeVariant] = hydra.lib.sets.fromList[hydra.variants.TypeVariant](hydra.lib.lists.concat[hydra.variants.TypeVariant](hydra.lib.lists.map[hydra.core.Type,
       Seq[hydra.variants.TypeVariant]](variants)(hydra.lib.maps.elems[hydra.core.Name, hydra.core.Type](deps))))
    hydra.lib.logic.not(hydra.lib.sets.member[hydra.variants.TypeVariant](hydra.variants.TypeVariant.function)(allVariants))
  })(hydra.predicates.typeDependencies(cx)(graph)(false)(hydra.lib.equality.identity[hydra.core.Type])(el.name))
}

def isSerializableByName(cx: hydra.context.Context)(graph: hydra.graph.Graph)(name: hydra.core.Name): Either[hydra.errors.Error, Boolean] =
  {
  def variants(typ: hydra.core.Type): Seq[hydra.variants.TypeVariant] =
    hydra.lib.lists.map[hydra.core.Type, hydra.variants.TypeVariant](hydra.reflect.typeVariant)(hydra.rewriting.foldOverType(hydra.coders.TraversalOrder.pre)((m: Seq[hydra.core.Type]) =>
    (t: hydra.core.Type) => hydra.lib.lists.cons[hydra.core.Type](t)(m))(Seq())(typ))
  hydra.lib.eithers.map[Map[hydra.core.Name, hydra.core.Type], Boolean, hydra.errors.Error]((deps: Map[hydra.core.Name, hydra.core.Type]) =>
    {
    lazy val allVariants: scala.collection.immutable.Set[hydra.variants.TypeVariant] = hydra.lib.sets.fromList[hydra.variants.TypeVariant](hydra.lib.lists.concat[hydra.variants.TypeVariant](hydra.lib.lists.map[hydra.core.Type,
       Seq[hydra.variants.TypeVariant]](variants)(hydra.lib.maps.elems[hydra.core.Name, hydra.core.Type](deps))))
    hydra.lib.logic.not(hydra.lib.sets.member[hydra.variants.TypeVariant](hydra.variants.TypeVariant.function)(allVariants))
  })(hydra.predicates.typeDependencies(cx)(graph)(false)(hydra.lib.equality.identity[hydra.core.Type])(name))
}

def isSerializableType(typ: hydra.core.Type): Boolean =
  {
  lazy val allVariants: scala.collection.immutable.Set[hydra.variants.TypeVariant] = hydra.lib.sets.fromList[hydra.variants.TypeVariant](hydra.lib.lists.map[hydra.core.Type,
     hydra.variants.TypeVariant](hydra.reflect.typeVariant)(hydra.rewriting.foldOverType(hydra.coders.TraversalOrder.pre)((m: Seq[hydra.core.Type]) =>
    (t: hydra.core.Type) => hydra.lib.lists.cons[hydra.core.Type](t)(m))(Seq())(typ)))
  hydra.lib.logic.not(hydra.lib.sets.member[hydra.variants.TypeVariant](hydra.variants.TypeVariant.function)(allVariants))
}

def isTrivialTerm(t: hydra.core.Term): Boolean =
  hydra.strip.deannotateTerm(t) match
  case hydra.core.Term.literal(v_Term_literal__) => true
  case hydra.core.Term.variable(v_Term_variable_nm) => hydra.lib.equality.equal[Int](hydra.lib.lists.length[scala.Predef.String](hydra.lib.strings.splitOn(".")(v_Term_variable_nm)))(1)
  case hydra.core.Term.unit => true
  case hydra.core.Term.application(v_Term_application_app) => {
    lazy val fun: hydra.core.Term = (v_Term_application_app.function)
    {
      lazy val arg: hydra.core.Term = (v_Term_application_app.argument)
      fun match
        case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
          case hydra.core.Function.elimination(v_Function_elimination_e) => v_Function_elimination_e match
            case hydra.core.Elimination.record(v_Elimination_record__) => hydra.predicates.isTrivialTerm(arg)
            case hydra.core.Elimination.wrap(v_Elimination_wrap__) => hydra.predicates.isTrivialTerm(arg)
            case _ => false
          case _ => false
        case _ => false
    }
  }
  case hydra.core.Term.maybe(v_Term_maybe_opt) => hydra.lib.maybes.maybe[Boolean, hydra.core.Term](true)((inner: hydra.core.Term) => hydra.predicates.isTrivialTerm(inner))(v_Term_maybe_opt)
  case hydra.core.Term.record(v_Term_record_rec) => hydra.lib.lists.foldl[Boolean, hydra.core.Field]((acc: Boolean) =>
    (fld: hydra.core.Field) =>
    hydra.lib.logic.and(acc)(hydra.predicates.isTrivialTerm(fld.term)))(true)(v_Term_record_rec.fields)
  case hydra.core.Term.wrap(v_Term_wrap_wt) => hydra.predicates.isTrivialTerm(v_Term_wrap_wt.body)
  case hydra.core.Term.typeApplication(v_Term_typeApplication_ta) => hydra.predicates.isTrivialTerm(v_Term_typeApplication_ta.body)
  case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => hydra.predicates.isTrivialTerm(v_Term_typeLambda_tl.body)
  case _ => false

def isType(t: hydra.core.Type): Boolean =
  hydra.strip.deannotateType(t) match
  case hydra.core.Type.application(v_Type_application_a) => hydra.predicates.isType(v_Type_application_a.function)
  case hydra.core.Type.forall(v_Type_forall_l) => hydra.predicates.isType(v_Type_forall_l.body)
  case hydra.core.Type.union(v_Type_union_rt) => false
  case hydra.core.Type.variable(v_Type_variable_v) => hydra.lib.equality.equal[hydra.core.Name](v_Type_variable_v)("hydra.core.Type")
  case _ => false

def isUnitTerm(v1: hydra.core.Term): Boolean =
  v1 match
  case hydra.core.Term.unit => true
  case _ => false

def isUnitType(v1: hydra.core.Type): Boolean =
  v1 match
  case hydra.core.Type.unit => true
  case _ => false

def typeDependencies(cx: hydra.context.Context)(graph: hydra.graph.Graph)(withSchema: Boolean)(transform: (hydra.core.Type => hydra.core.Type))(name: hydra.core.Name): Either[hydra.errors.Error,
   Map[hydra.core.Name, hydra.core.Type]] =
  {
  def requireType(name2: hydra.core.Name): Either[hydra.errors.Error, hydra.core.Type] =
    {
    lazy val cx1: hydra.context.Context = hydra.context.Context(hydra.lib.lists.cons[scala.Predef.String](hydra.lib.strings.cat2("type dependencies of ")(name2))(cx.trace),
       (cx.messages), (cx.other))
    hydra.lib.eithers.bind[hydra.errors.Error, hydra.core.Binding, hydra.core.Type](hydra.lexical.requireBinding(graph)(name2))((el: hydra.core.Binding) =>
      hydra.lib.eithers.bimap[hydra.errors.DecodingError, hydra.core.Type, hydra.errors.Error, hydra.core.Type]((_e: hydra.errors.DecodingError) => hydra.errors.Error.decoding(_e))((_a: hydra.core.Type) => _a)(hydra.decode.core.`type`(graph)(el.term)))
  }
  def toPair(name2: hydra.core.Name): Either[hydra.errors.Error, Tuple2[hydra.core.Name, hydra.core.Type]] =
    hydra.lib.eithers.map[hydra.core.Type, Tuple2[hydra.core.Name, hydra.core.Type], hydra.errors.Error]((typ: hydra.core.Type) => Tuple2(name2,
       transform(typ)))(requireType(name2))
  def deps(seeds: scala.collection.immutable.Set[hydra.core.Name])(names: Map[hydra.core.Name, hydra.core.Type]): Either[hydra.errors.Error,
     Map[hydra.core.Name, hydra.core.Type]] =
    hydra.lib.logic.ifElse[Either[hydra.errors.Error, Map[hydra.core.Name, hydra.core.Type]]](hydra.lib.sets.`null`[hydra.core.Name](seeds))(Right(names))(hydra.lib.eithers.bind[hydra.errors.Error,
       Seq[Tuple2[hydra.core.Name, hydra.core.Type]], Map[hydra.core.Name, hydra.core.Type]](hydra.lib.eithers.mapList[hydra.core.Name,
       Tuple2[hydra.core.Name, hydra.core.Type], hydra.errors.Error](toPair)(hydra.lib.sets.toList[hydra.core.Name](seeds)))((pairs: Seq[Tuple2[hydra.core.Name,
       hydra.core.Type]]) =>
    {
    lazy val newNames: Map[hydra.core.Name, hydra.core.Type] = hydra.lib.maps.union[hydra.core.Name, hydra.core.Type](names)(hydra.lib.maps.fromList[hydra.core.Name,
       hydra.core.Type](pairs))
    {
      lazy val refs: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.lists.foldl[scala.collection.immutable.Set[hydra.core.Name],
         scala.collection.immutable.Set[hydra.core.Name]](hydra.lib.sets.union[hydra.core.Name])(hydra.lib.sets.empty[hydra.core.Name])(hydra.lib.lists.map[Tuple2[hydra.core.Name,
         hydra.core.Type], scala.collection.immutable.Set[hydra.core.Name]]((pair: Tuple2[hydra.core.Name,
         hydra.core.Type]) =>
        hydra.dependencies.typeDependencyNames(withSchema)(hydra.lib.pairs.second[hydra.core.Name, hydra.core.Type](pair)))(pairs))
      {
        lazy val visited: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.fromList[hydra.core.Name](hydra.lib.maps.keys[hydra.core.Name,
           hydra.core.Type](names))
        {
          lazy val newSeeds: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.difference[hydra.core.Name](refs)(visited)
          deps(newSeeds)(newNames)
        }
      }
    }
  }))
  deps(hydra.lib.sets.singleton[hydra.core.Name](name))(hydra.lib.maps.empty[hydra.core.Name, hydra.core.Type])
}
