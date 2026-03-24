package hydra.unification

import hydra.coders.*

import hydra.context.*

import hydra.core.*

import hydra.errors.*

import hydra.typing.*

import hydra.lib.eithers

import hydra.lib.equality

import hydra.lib.lists

import hydra.lib.logic

import hydra.lib.maps

import hydra.lib.maybes

import hydra.lib.strings

def joinTypes(cx: hydra.context.Context)(left: hydra.core.Type)(right: hydra.core.Type)(comment: scala.Predef.String): Either[hydra.context.InContext[hydra.errors.UnificationError], Seq[hydra.typing.TypeConstraint]] =
  {
  lazy val sleft: hydra.core.Type = hydra.rewriting.deannotateType(left)
  lazy val sright: hydra.core.Type = hydra.rewriting.deannotateType(right)
  def joinOne(l: hydra.core.Type)(r: hydra.core.Type): hydra.typing.TypeConstraint =
    hydra.typing.TypeConstraint(l, r, hydra.lib.strings.cat2("join types; ")(comment))
  def cannotUnify[T0]: Either[hydra.context.InContext[hydra.errors.UnificationError], T0] =
    Left(hydra.context.InContext(hydra.errors.UnificationError(sleft, sright, hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("cannot unify ")(hydra.show.core.`type`(sleft)))(" with "))(hydra.show.core.`type`(sright))), cx))
  def assertEqual[T0]: Either[hydra.context.InContext[hydra.errors.UnificationError], Seq[T0]] =
    hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.UnificationError], Seq[T0]]](hydra.lib.equality.equal[hydra.core.Type](sleft)(sright))(Right(Seq()))(cannotUnify)
  def joinList(lefts: Seq[hydra.core.Type])(rights: Seq[hydra.core.Type]): Either[hydra.context.InContext[hydra.errors.UnificationError], Seq[hydra.typing.TypeConstraint]] =
    hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.UnificationError], Seq[hydra.typing.TypeConstraint]]](hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.core.Type](lefts))(hydra.lib.lists.length[hydra.core.Type](rights)))(Right(hydra.lib.lists.zipWith[hydra.core.Type, hydra.core.Type, hydra.typing.TypeConstraint](joinOne)(lefts)(rights)))(cannotUnify)
  def joinRowTypes(left2: Seq[hydra.core.FieldType])(right2: Seq[hydra.core.FieldType]): Either[hydra.context.InContext[hydra.errors.UnificationError], Seq[hydra.typing.TypeConstraint]] =
    hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.UnificationError], Seq[hydra.typing.TypeConstraint]]](hydra.lib.logic.and(hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.core.Name](hydra.lib.lists.map[hydra.core.FieldType, hydra.core.Name]((x: hydra.core.FieldType) => (x.name))(left2)))(hydra.lib.lists.length[hydra.core.Name](hydra.lib.lists.map[hydra.core.FieldType, hydra.core.Name]((x: hydra.core.FieldType) => (x.name))(right2))))(hydra.lib.lists.foldl[Boolean, Boolean](hydra.lib.logic.and)(true)(hydra.lib.lists.zipWith[hydra.core.Name, hydra.core.Name, Boolean]((left3: hydra.core.Name) =>
    (right3: hydra.core.Name) => hydra.lib.equality.equal[scala.Predef.String](left3)(right3))(hydra.lib.lists.map[hydra.core.FieldType, hydra.core.Name]((x: hydra.core.FieldType) => (x.name))(left2))(hydra.lib.lists.map[hydra.core.FieldType, hydra.core.Name]((x: hydra.core.FieldType) => (x.name))(right2)))))(joinList(hydra.lib.lists.map[hydra.core.FieldType, hydra.core.Type]((x: hydra.core.FieldType) => (x.`type`))(left2))(hydra.lib.lists.map[hydra.core.FieldType, hydra.core.Type]((x: hydra.core.FieldType) => (x.`type`))(right2)))(cannotUnify)
  sleft match
    case hydra.core.Type.application(v_Type_application_l) => sright match
      case hydra.core.Type.application(v_Type_application_r) => Right(Seq(joinOne(v_Type_application_l.function)(v_Type_application_r.function), joinOne(v_Type_application_l.argument)(v_Type_application_r.argument)))
      case _ => cannotUnify
    case hydra.core.Type.either(v_Type_either_l) => sright match
      case hydra.core.Type.either(v_Type_either_r) => Right(Seq(joinOne(v_Type_either_l.left)(v_Type_either_r.left), joinOne(v_Type_either_l.right)(v_Type_either_r.right)))
      case _ => cannotUnify
    case hydra.core.Type.function(v_Type_function_l) => sright match
      case hydra.core.Type.function(v_Type_function_r) => Right(Seq(joinOne(v_Type_function_l.domain)(v_Type_function_r.domain), joinOne(v_Type_function_l.codomain)(v_Type_function_r.codomain)))
      case _ => cannotUnify
    case hydra.core.Type.list(v_Type_list_l) => sright match
      case hydra.core.Type.list(v_Type_list_r) => Right(Seq(joinOne(v_Type_list_l)(v_Type_list_r)))
      case _ => cannotUnify
    case hydra.core.Type.literal() => assertEqual
    case hydra.core.Type.map(v_Type_map_l) => sright match
      case hydra.core.Type.map(v_Type_map_r) => Right(Seq(joinOne(v_Type_map_l.keys)(v_Type_map_r.keys), joinOne(v_Type_map_l.values)(v_Type_map_r.values)))
      case _ => cannotUnify
    case hydra.core.Type.maybe(v_Type_maybe_l) => sright match
      case hydra.core.Type.maybe(v_Type_maybe_r) => Right(Seq(joinOne(v_Type_maybe_l)(v_Type_maybe_r)))
      case _ => cannotUnify
    case hydra.core.Type.pair(v_Type_pair_l) => sright match
      case hydra.core.Type.pair(v_Type_pair_r) => Right(Seq(joinOne(v_Type_pair_l.first)(v_Type_pair_r.first), joinOne(v_Type_pair_l.second)(v_Type_pair_r.second)))
      case _ => cannotUnify
    case hydra.core.Type.record(v_Type_record_l) => sright match
      case hydra.core.Type.record(v_Type_record_r) => joinRowTypes(v_Type_record_l)(v_Type_record_r)
      case _ => cannotUnify
    case hydra.core.Type.set(v_Type_set_l) => sright match
      case hydra.core.Type.set(v_Type_set_r) => Right(Seq(joinOne(v_Type_set_l)(v_Type_set_r)))
      case _ => cannotUnify
    case hydra.core.Type.union(v_Type_union_l) => sright match
      case hydra.core.Type.union(v_Type_union_r) => joinRowTypes(v_Type_union_l)(v_Type_union_r)
      case _ => cannotUnify
    case hydra.core.Type.unit() => sright match
      case hydra.core.Type.unit() => Right(Seq())
      case _ => cannotUnify
    case hydra.core.Type.void() => sright match
      case hydra.core.Type.void() => Right(Seq())
      case _ => cannotUnify
    case hydra.core.Type.wrap(v_Type_wrap_l) => sright match
      case hydra.core.Type.wrap(v_Type_wrap_r) => Right(Seq(joinOne(v_Type_wrap_l)(v_Type_wrap_r)))
      case _ => cannotUnify
    case _ => cannotUnify
}

def unifyTypeConstraints[T0](cx: hydra.context.Context)(schemaTypes: Map[hydra.core.Name, T0])(constraints: Seq[hydra.typing.TypeConstraint]): Either[hydra.context.InContext[hydra.errors.UnificationError], hydra.typing.TypeSubst] =
  {
  def withConstraint(c: hydra.typing.TypeConstraint)(rest: Seq[hydra.typing.TypeConstraint]): Either[hydra.context.InContext[hydra.errors.UnificationError], hydra.typing.TypeSubst] =
    {
    lazy val sleft: hydra.core.Type = hydra.rewriting.deannotateType(c.left)
    lazy val sright: hydra.core.Type = hydra.rewriting.deannotateType(c.right)
    lazy val comment: scala.Predef.String = (c.comment)
    def bind(v: hydra.core.Name)(t: hydra.core.Type): Either[hydra.context.InContext[hydra.errors.UnificationError], hydra.typing.TypeSubst] =
      {
      lazy val subst: hydra.typing.TypeSubst = hydra.substitution.singletonTypeSubst(v)(t)
      def withResult(s: hydra.typing.TypeSubst): hydra.typing.TypeSubst = hydra.substitution.composeTypeSubst(subst)(s)
      hydra.lib.eithers.map[hydra.typing.TypeSubst, hydra.typing.TypeSubst, hydra.context.InContext[hydra.errors.UnificationError]](withResult)(hydra.unification.unifyTypeConstraints(cx)(schemaTypes)(hydra.substitution.substituteInConstraints(subst)(rest)))
    }
    def tryBinding(v: hydra.core.Name)(t: hydra.core.Type): Either[hydra.context.InContext[hydra.errors.UnificationError], hydra.typing.TypeSubst] =
      hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.UnificationError], hydra.typing.TypeSubst]](hydra.unification.variableOccursInType(v)(t))(Left(hydra.context.InContext(hydra.errors.UnificationError(sleft, sright, hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("Variable ")(v))(" appears free in type "))(hydra.show.core.`type`(t)))(" ("))(comment))(")")), cx)))(bind(v)(t))
    lazy val noVars: Either[hydra.context.InContext[hydra.errors.UnificationError], hydra.typing.TypeSubst] = {
      def withConstraints(constraints2: Seq[hydra.typing.TypeConstraint]): Either[hydra.context.InContext[hydra.errors.UnificationError], hydra.typing.TypeSubst] =
        hydra.unification.unifyTypeConstraints(cx)(schemaTypes)(hydra.lib.lists.concat2[hydra.typing.TypeConstraint](constraints2)(rest))
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.UnificationError], Seq[hydra.typing.TypeConstraint], hydra.typing.TypeSubst](hydra.unification.joinTypes(cx)(sleft)(sright)(comment))(withConstraints)
    }
    lazy val dflt: Either[hydra.context.InContext[hydra.errors.UnificationError], hydra.typing.TypeSubst] = sright match
      case hydra.core.Type.variable(v_Type_variable_name) => tryBinding(v_Type_variable_name)(sleft)
      case _ => noVars
    sleft match
      case hydra.core.Type.variable(v_Type_variable_name) => sright match
        case hydra.core.Type.variable(v_Type_variable_name2) => hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.UnificationError], hydra.typing.TypeSubst]](hydra.lib.equality.equal[scala.Predef.String](v_Type_variable_name)(v_Type_variable_name2))(hydra.unification.unifyTypeConstraints(cx)(schemaTypes)(rest))(hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.UnificationError], hydra.typing.TypeSubst]](hydra.lib.maybes.isJust[T0](hydra.lib.maps.lookup[hydra.core.Name, T0](v_Type_variable_name)(schemaTypes)))(hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.UnificationError], hydra.typing.TypeSubst]](hydra.lib.maybes.isJust[T0](hydra.lib.maps.lookup[hydra.core.Name, T0](v_Type_variable_name2)(schemaTypes)))(Left(hydra.context.InContext(hydra.errors.UnificationError(sleft, sright, hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("Attempted to unify schema names ")(v_Type_variable_name))(" and "))(v_Type_variable_name2))(" ("))(comment))(")")), cx)))(bind(v_Type_variable_name2)(sleft)))(bind(v_Type_variable_name)(sright)))
        case _ => tryBinding(v_Type_variable_name)(sright)
      case _ => dflt
  }
  hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.UnificationError], hydra.typing.TypeSubst]](hydra.lib.lists.`null`[hydra.typing.TypeConstraint](constraints))(Right(hydra.substitution.idTypeSubst))(withConstraint(hydra.lib.lists.head[hydra.typing.TypeConstraint](constraints))(hydra.lib.lists.tail[hydra.typing.TypeConstraint](constraints)))
}

def unifyTypeLists[T0](cx: hydra.context.Context)(schemaTypes: Map[hydra.core.Name, T0])(l: Seq[hydra.core.Type])(r: Seq[hydra.core.Type])(comment: scala.Predef.String): Either[hydra.context.InContext[hydra.errors.UnificationError], hydra.typing.TypeSubst] =
  {
  def toConstraint(l2: hydra.core.Type)(r2: hydra.core.Type): hydra.typing.TypeConstraint = hydra.typing.TypeConstraint(l2, r2, comment)
  hydra.unification.unifyTypeConstraints(cx)(schemaTypes)(hydra.lib.lists.zipWith[hydra.core.Type, hydra.core.Type, hydra.typing.TypeConstraint](toConstraint)(l)(r))
}

def unifyTypes[T0](cx: hydra.context.Context)(schemaTypes: Map[hydra.core.Name, T0])(l: hydra.core.Type)(r: hydra.core.Type)(comment: scala.Predef.String): Either[hydra.context.InContext[hydra.errors.UnificationError], hydra.typing.TypeSubst] =
  hydra.unification.unifyTypeConstraints(cx)(schemaTypes)(Seq(hydra.typing.TypeConstraint(l, r, comment)))

def variableOccursInType(`var`: hydra.core.Name)(typ0: hydra.core.Type): Boolean =
  {
  def tryType(b: Boolean)(typ: hydra.core.Type): Boolean =
    typ match
    case hydra.core.Type.variable(v_Type_variable_v) => hydra.lib.logic.or(b)(hydra.lib.equality.equal[scala.Predef.String](v_Type_variable_v)(`var`))
    case _ => b
  hydra.rewriting.foldOverType(hydra.coders.TraversalOrder.pre)(tryType)(false)(typ0)
}
