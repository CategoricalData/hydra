package hydra.predicates

import hydra.coders.*

import hydra.context.*

import hydra.core.*

import hydra.errors.*

import hydra.variants.*

import hydra.lib.eithers

import hydra.lib.equality

import hydra.lib.lists

import hydra.lib.logic

import hydra.lib.maps

import hydra.lib.pairs

import hydra.lib.sets

import hydra.lib.strings

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

def isSerializable(cx: hydra.context.Context)(graph: hydra.graph.Graph)(el: hydra.core.Binding): Either[hydra.context.InContext[hydra.errors.Error], Boolean] =
  {
  def variants(typ: hydra.core.Type): Seq[hydra.variants.TypeVariant] =
    hydra.lib.lists.map[hydra.core.Type, hydra.variants.TypeVariant](hydra.reflect.typeVariant)(hydra.rewriting.foldOverType(hydra.coders.TraversalOrder.pre)((m: Seq[hydra.core.Type]) =>
    (t: hydra.core.Type) => hydra.lib.lists.cons[hydra.core.Type](t)(m))(Seq())(typ))
  hydra.lib.eithers.map[Map[hydra.core.Name, hydra.core.Type], Boolean, hydra.context.InContext[hydra.errors.Error]]((deps: Map[hydra.core.Name,
     hydra.core.Type]) =>
    {
    lazy val allVariants: scala.collection.immutable.Set[hydra.variants.TypeVariant] = hydra.lib.sets.fromList[hydra.variants.TypeVariant](hydra.lib.lists.concat[hydra.variants.TypeVariant](hydra.lib.lists.map[hydra.core.Type,
       Seq[hydra.variants.TypeVariant]](variants)(hydra.lib.maps.elems[hydra.core.Name, hydra.core.Type](deps))))
    hydra.lib.logic.not(hydra.lib.sets.member[hydra.variants.TypeVariant](hydra.variants.TypeVariant.function)(allVariants))
  })(hydra.predicates.typeDependencies(cx)(graph)(false)(hydra.lib.equality.identity[hydra.core.Type])(el.name))
}

def isSerializableByName(cx: hydra.context.Context)(graph: hydra.graph.Graph)(name: hydra.core.Name): Either[hydra.context.InContext[hydra.errors.Error],
   Boolean] =
  {
  def variants(typ: hydra.core.Type): Seq[hydra.variants.TypeVariant] =
    hydra.lib.lists.map[hydra.core.Type, hydra.variants.TypeVariant](hydra.reflect.typeVariant)(hydra.rewriting.foldOverType(hydra.coders.TraversalOrder.pre)((m: Seq[hydra.core.Type]) =>
    (t: hydra.core.Type) => hydra.lib.lists.cons[hydra.core.Type](t)(m))(Seq())(typ))
  hydra.lib.eithers.map[Map[hydra.core.Name, hydra.core.Type], Boolean, hydra.context.InContext[hydra.errors.Error]]((deps: Map[hydra.core.Name,
     hydra.core.Type]) =>
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

def typeDependencies(cx: hydra.context.Context)(graph: hydra.graph.Graph)(withSchema: Boolean)(transform: (hydra.core.Type => hydra.core.Type))(name: hydra.core.Name): Either[hydra.context.InContext[hydra.errors.Error],
   Map[hydra.core.Name, hydra.core.Type]] =
  {
  def requireType(name2: hydra.core.Name): Either[hydra.context.InContext[hydra.errors.Error], hydra.core.Type] =
    {
    lazy val cx1: hydra.context.Context = hydra.context.Context(hydra.lib.lists.cons[scala.Predef.String](hydra.lib.strings.cat2("type dependencies of ")(name2))(cx.trace),
       (cx.messages), (cx.other))
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.core.Binding, hydra.core.Type](hydra.lexical.requireElement(cx1)(graph)(name2))((el: hydra.core.Binding) =>
      hydra.lib.eithers.bimap[hydra.errors.Error, hydra.core.Type, hydra.context.InContext[hydra.errors.Error],
         hydra.core.Type]((_wc_e: hydra.errors.Error) => hydra.context.InContext(_wc_e, cx1))((_wc_a: hydra.core.Type) => _wc_a)(hydra.lib.eithers.bimap[hydra.errors.DecodingError,
         hydra.core.Type, hydra.errors.Error, hydra.core.Type]((_e: hydra.errors.DecodingError) => hydra.errors.Error.other(_e))((_a: hydra.core.Type) => _a)(hydra.decode.core.`type`(graph)(el.term))))
  }
  def toPair(name2: hydra.core.Name): Either[hydra.context.InContext[hydra.errors.Error], Tuple2[hydra.core.Name, hydra.core.Type]] =
    hydra.lib.eithers.map[hydra.core.Type, Tuple2[hydra.core.Name, hydra.core.Type], hydra.context.InContext[hydra.errors.Error]]((typ: hydra.core.Type) => Tuple2(name2,
       transform(typ)))(requireType(name2))
  def deps(seeds: scala.collection.immutable.Set[hydra.core.Name])(names: Map[hydra.core.Name, hydra.core.Type]): Either[hydra.context.InContext[hydra.errors.Error],
     Map[hydra.core.Name, hydra.core.Type]] =
    hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], Map[hydra.core.Name, hydra.core.Type]]](hydra.lib.sets.`null`[hydra.core.Name](seeds))(Right(names))(hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error],
       Seq[Tuple2[hydra.core.Name, hydra.core.Type]], Map[hydra.core.Name, hydra.core.Type]](hydra.lib.eithers.mapList[hydra.core.Name,
       Tuple2[hydra.core.Name, hydra.core.Type], hydra.context.InContext[hydra.errors.Error]](toPair)(hydra.lib.sets.toList[hydra.core.Name](seeds)))((pairs: Seq[Tuple2[hydra.core.Name,
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
