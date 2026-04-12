# Note: this is an automatically generated file. Do not edit.

r"""Validation functions for core terms and types."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Just, Maybe, Nothing, frozenlist
from typing import TypeVar, cast
import hydra.core
import hydra.error.core
import hydra.graph
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.paths
import hydra.rewriting
import hydra.variables

T0 = TypeVar("T0")
T1 = TypeVar("T1")

def find_duplicate(names: frozenlist[T0]) -> Maybe[T0]:
    r"""Find the first duplicate name in a list."""

    @lru_cache(1)
    def result() -> tuple[frozenset[T0], Maybe[T0]]:
        return hydra.lib.lists.foldl((lambda acc, name: (seen := hydra.lib.pairs.first(acc), dup := hydra.lib.pairs.second(acc), hydra.lib.maybes.cases(dup, (lambda : hydra.lib.logic.if_else(hydra.lib.sets.member(name, seen), (lambda : (seen, Just(name))), (lambda : (hydra.lib.sets.insert(name, seen), Nothing())))), (lambda _: acc)))[2]), (hydra.lib.sets.empty(), Nothing()), names)
    return hydra.lib.pairs.second(result())

def check_duplicate_bindings(path: hydra.paths.SubtermPath, bindings: frozenlist[hydra.core.Binding]) -> Maybe[hydra.error.core.InvalidTermError]:
    r"""Check for duplicate binding names in a list of bindings."""

    @lru_cache(1)
    def names() -> frozenlist[hydra.core.Name]:
        return hydra.lib.lists.map((lambda v1: v1.name), bindings)
    @lru_cache(1)
    def dup() -> Maybe[hydra.core.Name]:
        return find_duplicate(names())
    return hydra.lib.maybes.map((lambda name: cast(hydra.error.core.InvalidTermError, hydra.error.core.InvalidTermErrorDuplicateBinding(hydra.error.core.DuplicateBindingError(path, name)))), dup())

def find_duplicate_field_type(names: frozenlist[T0]) -> Maybe[T0]:
    r"""Find the first duplicate name in a list (for field type validation)."""

    @lru_cache(1)
    def result() -> tuple[frozenset[T0], Maybe[T0]]:
        return hydra.lib.lists.foldl((lambda acc, name: (seen := hydra.lib.pairs.first(acc), dup := hydra.lib.pairs.second(acc), hydra.lib.maybes.cases(dup, (lambda : hydra.lib.logic.if_else(hydra.lib.sets.member(name, seen), (lambda : (seen, Just(name))), (lambda : (hydra.lib.sets.insert(name, seen), Nothing())))), (lambda _: acc)))[2]), (hydra.lib.sets.empty(), Nothing()), names)
    return hydra.lib.pairs.second(result())

def check_duplicate_field_types(fields: frozenlist[hydra.core.FieldType], mk_error: Callable[[hydra.core.Name], Maybe[T0]]) -> Maybe[T0]:
    r"""Check for duplicate field names in a list of field types."""

    @lru_cache(1)
    def names() -> frozenlist[hydra.core.Name]:
        return hydra.lib.lists.map((lambda v1: v1.name), fields)
    @lru_cache(1)
    def dup() -> Maybe[hydra.core.Name]:
        return find_duplicate_field_type(names())
    return hydra.lib.maybes.cases(dup(), (lambda : Nothing()), (lambda name: mk_error(name)))

def check_duplicate_fields(path: hydra.paths.SubtermPath, names: frozenlist[hydra.core.Name]) -> Maybe[hydra.error.core.InvalidTermError]:
    r"""Check for duplicate field names in a list of fields."""

    @lru_cache(1)
    def dup() -> Maybe[hydra.core.Name]:
        return find_duplicate(names)
    return hydra.lib.maybes.map((lambda name: cast(hydra.error.core.InvalidTermError, hydra.error.core.InvalidTermErrorDuplicateField(hydra.error.core.DuplicateFieldError(path, name)))), dup())

def check_shadowing(path: hydra.paths.SubtermPath, cx: hydra.graph.Graph, names: frozenlist[hydra.core.Name]) -> Maybe[hydra.error.core.InvalidTermError]:
    r"""Check if any name in a list shadows a variable already in scope."""

    @lru_cache(1)
    def result() -> Maybe[hydra.error.core.InvalidTermError]:
        return hydra.lib.lists.foldl((lambda acc, name: hydra.lib.maybes.cases(acc, (lambda : hydra.lib.logic.if_else(hydra.lib.logic.or_(hydra.lib.maybes.is_just(hydra.lib.maps.lookup(name, cx.bound_terms)), hydra.lib.sets.member(name, cx.lambda_variables)), (lambda : Just(cast(hydra.error.core.InvalidTermError, hydra.error.core.InvalidTermErrorTermVariableShadowing(hydra.error.core.TermVariableShadowingError(path, name))))), (lambda : Nothing()))), (lambda _: acc))), Nothing(), names)
    return result()

def check_undefined_type_variables_in_type(path: T0, cx: hydra.graph.Graph, typ: hydra.core.Type, mk_error: Callable[[hydra.core.Name], Maybe[T1]]) -> Maybe[T1]:
    r"""Check a type for type variables not bound in the current scope."""

    @lru_cache(1)
    def free_vars() -> frozenset[hydra.core.Name]:
        return hydra.variables.free_variables_in_type(typ)
    @lru_cache(1)
    def undefined() -> frozenset[hydra.core.Name]:
        return hydra.lib.sets.difference(free_vars(), cx.type_variables)
    return hydra.lib.logic.if_else(hydra.lib.sets.null(undefined()), (lambda : Nothing()), (lambda : (first_undefined := hydra.lib.lists.head(hydra.lib.sets.to_list(undefined())), mk_error(first_undefined))[1]))

def check_undefined_type_variables_in_type_scheme(path: T0, cx: hydra.graph.Graph, ts: hydra.core.TypeScheme, mk_error: Callable[[hydra.core.Name], Maybe[T1]]) -> Maybe[T1]:
    r"""Check a type scheme for type variables not bound by the scheme or the current scope."""

    @lru_cache(1)
    def free_vars() -> frozenset[hydra.core.Name]:
        return hydra.variables.free_variables_in_type_scheme(ts)
    @lru_cache(1)
    def undefined() -> frozenset[hydra.core.Name]:
        return hydra.lib.sets.difference(free_vars(), cx.type_variables)
    return hydra.lib.logic.if_else(hydra.lib.sets.null(undefined()), (lambda : Nothing()), (lambda : (first_undefined := hydra.lib.lists.head(hydra.lib.sets.to_list(undefined())), mk_error(first_undefined))[1]))

def first_error(checks: frozenlist[Maybe[T0]]) -> Maybe[T0]:
    r"""Return the first error from a list of optional errors, or nothing if all are valid."""

    return hydra.lib.lists.foldl((lambda acc, check: hydra.lib.maybes.cases(acc, (lambda : check), (lambda _: acc))), Nothing(), checks)

def is_valid_name(name: hydra.core.Name) -> bool:
    r"""Check whether a name is valid at an introduction site. Currently rejects empty strings."""

    return hydra.lib.logic.not_(hydra.lib.equality.equal(name.value, ""))

def check_term(typed: bool, path: hydra.paths.SubtermPath, cx: hydra.graph.Graph, term: hydra.core.Term):
    def _hoist_hydra_validate_core_check_term_1(path, v1):
        match v1:
            case hydra.core.EliminationRecord(value=proj):
                tname = proj.type_name
                return hydra.lib.logic.if_else(hydra.lib.equality.equal(tname.value, ""), (lambda : Just(cast(hydra.error.core.InvalidTermError, hydra.error.core.InvalidTermErrorEmptyTypeNameInTerm(hydra.error.core.EmptyTypeNameInTermError(path))))), (lambda : Nothing()))

            case hydra.core.EliminationUnion(value=cs):
                tname = cs.type_name
                cs_default = cs.default
                cs_cases = cs.cases
                return first_error((hydra.lib.logic.if_else(hydra.lib.equality.equal(tname.value, ""), (lambda : Just(cast(hydra.error.core.InvalidTermError, hydra.error.core.InvalidTermErrorEmptyTypeNameInTerm(hydra.error.core.EmptyTypeNameInTermError(path))))), (lambda : Nothing())), hydra.lib.logic.if_else(hydra.lib.logic.and_(hydra.lib.lists.null(cs_cases), hydra.lib.maybes.is_nothing(cs_default)), (lambda : Just(cast(hydra.error.core.InvalidTermError, hydra.error.core.InvalidTermErrorEmptyCaseStatement(hydra.error.core.EmptyCaseStatementError(path, tname))))), (lambda : Nothing())), check_duplicate_fields(path, hydra.lib.lists.map((lambda v1: v1.name), cs_cases))))

            case _:
                return Nothing()
    def _hoist_hydra_validate_core_check_term_2(cx, path, typed, v1):
        match v1:
            case hydra.core.FunctionLambda(value=lam):
                param_name = lam.parameter
                return first_error((hydra.lib.logic.if_else(hydra.lib.maybes.is_just(hydra.lib.maps.lookup(param_name, cx.bound_terms)), (lambda : Just(cast(hydra.error.core.InvalidTermError, hydra.error.core.InvalidTermErrorTermVariableShadowing(hydra.error.core.TermVariableShadowingError(path, param_name))))), (lambda : Nothing())), hydra.lib.logic.if_else(is_valid_name(param_name), (lambda : Nothing()), (lambda : Just(cast(hydra.error.core.InvalidTermError, hydra.error.core.InvalidTermErrorInvalidLambdaParameterName(hydra.error.core.InvalidLambdaParameterNameError(path, param_name)))))), hydra.lib.logic.if_else(typed, (lambda : hydra.lib.maybes.cases(lam.domain, (lambda : Nothing()), (lambda dom: check_undefined_type_variables_in_type(path, cx, dom, (lambda uv_name: Just(cast(hydra.error.core.InvalidTermError, hydra.error.core.InvalidTermErrorUndefinedTypeVariableInLambdaDomain(hydra.error.core.UndefinedTypeVariableInLambdaDomainError(path, uv_name))))))))), (lambda : Nothing()))))

            case hydra.core.FunctionElimination(value=elim):
                return _hoist_hydra_validate_core_check_term_1(path, elim)

            case _:
                return Nothing()
    match term:
        case hydra.core.TermAnnotated(value=ann):
            body = ann.body
            ann_map = ann.annotation
            def _hoist_ann_map_body_1(v1):
                match v1:
                    case hydra.core.TermAnnotated():
                        return Just(cast(hydra.error.core.InvalidTermError, hydra.error.core.InvalidTermErrorNestedTermAnnotation(hydra.error.core.NestedTermAnnotationError(path))))

                    case _:
                        return Nothing()
            return first_error((hydra.lib.logic.if_else(hydra.lib.maps.null(ann_map), (lambda : Just(cast(hydra.error.core.InvalidTermError, hydra.error.core.InvalidTermErrorEmptyTermAnnotation(hydra.error.core.EmptyTermAnnotationError(path))))), (lambda : Nothing())), _hoist_ann_map_body_1(body)))

        case hydra.core.TermApplication(value=app):
            fun = app.function
            arg = app.argument
            def _hoist_arg_body_1(v1):
                match v1:
                    case hydra.core.LiteralBoolean(value=bool_val):
                        return Just(cast(hydra.error.core.InvalidTermError, hydra.error.core.InvalidTermErrorConstantCondition(hydra.error.core.ConstantConditionError(path, bool_val))))

                    case _:
                        return Nothing()
            def _hoist_arg_body_2(v1):
                match v1:
                    case hydra.core.TermLiteral(value=lit):
                        return _hoist_arg_body_1(lit)

                    case _:
                        return Nothing()
            def _hoist_arg_body_3(v1):
                match v1:
                    case hydra.core.TermVariable(value=prim_name):
                        return hydra.lib.logic.if_else(hydra.lib.equality.equal(prim_name.value, "hydra.lib.logic.ifElse"), (lambda : _hoist_arg_body_2(arg)), (lambda : Nothing()))

                    case _:
                        return Nothing()
            def _hoist_arg_body_4(fun_name, v1):
                match v1:
                    case hydra.core.TermVariable(value=arg_name):
                        return hydra.lib.logic.if_else(hydra.lib.equality.equal(fun_name, arg_name), (lambda : Just(cast(hydra.error.core.InvalidTermError, hydra.error.core.InvalidTermErrorSelfApplication(hydra.error.core.SelfApplicationError(path, fun_name))))), (lambda : Nothing()))

                    case _:
                        return Nothing()
            def _hoist_arg_body_5(v1):
                match v1:
                    case hydra.core.TermVariable(value=fun_name):
                        return _hoist_arg_body_4(fun_name, arg)

                    case _:
                        return Nothing()
            def _hoist_arg_body_6(v1):
                match v1:
                    case hydra.core.FunctionLambda(value=lam):
                        param = lam.parameter
                        body = lam.body
                        def _hoist_body_body_1(v12):
                            match v12:
                                case hydra.core.TermVariable(value=body_var):
                                    return hydra.lib.logic.if_else(hydra.lib.equality.equal(param, body_var), (lambda : Just(cast(hydra.error.core.InvalidTermError, hydra.error.core.InvalidTermErrorUnnecessaryIdentityApplication(hydra.error.core.UnnecessaryIdentityApplicationError(path))))), (lambda : Nothing()))

                                case _:
                                    return Nothing()
                        return _hoist_body_body_1(body)

                    case _:
                        return Nothing()
            def _hoist_arg_body_7(v1):
                match v1:
                    case hydra.core.TermFunction(value=f):
                        return _hoist_arg_body_6(f)

                    case _:
                        return Nothing()
            def _hoist_arg_body_8(unwrap_name, v1):
                match v1:
                    case hydra.core.TermWrap(value=wt):
                        wrap_name = wt.type_name
                        return hydra.lib.logic.if_else(hydra.lib.equality.equal(unwrap_name, wrap_name), (lambda : Just(cast(hydra.error.core.InvalidTermError, hydra.error.core.InvalidTermErrorRedundantWrapUnwrap(hydra.error.core.RedundantWrapUnwrapError(path, unwrap_name))))), (lambda : Nothing()))

                    case _:
                        return Nothing()
            def _hoist_arg_body_9(v1):
                match v1:
                    case hydra.core.EliminationWrap(value=unwrap_name):
                        return _hoist_arg_body_8(unwrap_name, arg)

                    case _:
                        return Nothing()
            def _hoist_arg_body_10(v1):
                match v1:
                    case hydra.core.FunctionElimination(value=elim):
                        return _hoist_arg_body_9(elim)

                    case _:
                        return Nothing()
            def _hoist_arg_body_11(v1):
                match v1:
                    case hydra.core.TermFunction(value=f):
                        return _hoist_arg_body_10(f)

                    case _:
                        return Nothing()
            return first_error((_hoist_arg_body_3(fun), _hoist_arg_body_5(fun), _hoist_arg_body_7(fun), _hoist_arg_body_11(fun)))

        case hydra.core.TermRecord(value=rec):
            tname = rec.type_name
            flds = rec.fields
            return first_error((hydra.lib.logic.if_else(hydra.lib.equality.equal(tname.value, ""), (lambda : Just(cast(hydra.error.core.InvalidTermError, hydra.error.core.InvalidTermErrorEmptyTypeNameInTerm(hydra.error.core.EmptyTypeNameInTermError(path))))), (lambda : Nothing())), check_duplicate_fields(path, hydra.lib.lists.map((lambda v1: v1.name), flds))))

        case hydra.core.TermLet(value=lt):
            bindings = lt.bindings
            @lru_cache(1)
            def names() -> frozenlist[hydra.core.Name]:
                return hydra.lib.lists.map((lambda v1: v1.name), bindings)
            return first_error((hydra.lib.logic.if_else(hydra.lib.lists.null(bindings), (lambda : Just(cast(hydra.error.core.InvalidTermError, hydra.error.core.InvalidTermErrorEmptyLetBindings(hydra.error.core.EmptyLetBindingsError(path))))), (lambda : Nothing())), check_duplicate_bindings(path, bindings), Nothing(), first_error(hydra.lib.lists.map((lambda bname: hydra.lib.logic.if_else(is_valid_name(bname), (lambda : Nothing()), (lambda : Just(cast(hydra.error.core.InvalidTermError, hydra.error.core.InvalidTermErrorInvalidLetBindingName(hydra.error.core.InvalidLetBindingNameError(path, bname))))))), names())), hydra.lib.logic.if_else(typed, (lambda : first_error(hydra.lib.lists.map((lambda b: hydra.lib.maybes.cases(b.type, (lambda : Nothing()), (lambda ts: check_undefined_type_variables_in_type_scheme(path, cx, ts, (lambda uv_name: Just(cast(hydra.error.core.InvalidTermError, hydra.error.core.InvalidTermErrorUndefinedTypeVariableInBindingType(hydra.error.core.UndefinedTypeVariableInBindingTypeError(path, uv_name))))))))), bindings))), (lambda : Nothing()))))

        case hydra.core.TermUnion(value=inj):
            tname = inj.type_name
            return hydra.lib.logic.if_else(hydra.lib.equality.equal(tname.value, ""), (lambda : Just(cast(hydra.error.core.InvalidTermError, hydra.error.core.InvalidTermErrorEmptyTypeNameInTerm(hydra.error.core.EmptyTypeNameInTermError(path))))), (lambda : Nothing()))

        case hydra.core.TermFunction(value=fun):
            return _hoist_hydra_validate_core_check_term_2(cx, path, typed, fun)

        case hydra.core.TermTypeApplication(value=ta):
            return hydra.lib.logic.if_else(typed, (lambda : check_undefined_type_variables_in_type(path, cx, ta.type, (lambda uv_name: Just(cast(hydra.error.core.InvalidTermError, hydra.error.core.InvalidTermErrorUndefinedTypeVariableInTypeApplication(hydra.error.core.UndefinedTypeVariableInTypeApplicationError(path, uv_name))))))), (lambda : Nothing()))

        case hydra.core.TermTypeLambda(value=tl):
            tv_name = tl.parameter
            return first_error((hydra.lib.logic.if_else(hydra.lib.sets.member(tv_name, hydra.lib.sets.delete(tv_name, cx.type_variables)), (lambda : Just(cast(hydra.error.core.InvalidTermError, hydra.error.core.InvalidTermErrorTypeVariableShadowingInTypeLambda(hydra.error.core.TypeVariableShadowingInTypeLambdaError(path, tv_name))))), (lambda : Nothing())), hydra.lib.logic.if_else(is_valid_name(tv_name), (lambda : Nothing()), (lambda : Just(cast(hydra.error.core.InvalidTermError, hydra.error.core.InvalidTermErrorInvalidTypeLambdaParameterName(hydra.error.core.InvalidTypeLambdaParameterNameError(path, tv_name))))))))

        case hydra.core.TermVariable(value=var_name):
            return hydra.lib.logic.if_else(hydra.lib.logic.or_(hydra.lib.maybes.is_just(hydra.lib.maps.lookup(var_name, cx.bound_terms)), hydra.lib.logic.or_(hydra.lib.sets.member(var_name, cx.lambda_variables), hydra.lib.maybes.is_just(hydra.lib.maps.lookup(var_name, cx.primitives)))), (lambda : Nothing()), (lambda : Just(cast(hydra.error.core.InvalidTermError, hydra.error.core.InvalidTermErrorUndefinedTermVariable(hydra.error.core.UndefinedTermVariableError(path, var_name))))))

        case hydra.core.TermWrap(value=wt):
            tname = wt.type_name
            return hydra.lib.logic.if_else(hydra.lib.equality.equal(tname.value, ""), (lambda : Just(cast(hydra.error.core.InvalidTermError, hydra.error.core.InvalidTermErrorEmptyTypeNameInTerm(hydra.error.core.EmptyTypeNameInTermError(path))))), (lambda : Nothing()))

        case _:
            return Nothing()

def check_void(typ: hydra.core.Type) -> Maybe[hydra.error.core.InvalidTypeError]:
    r"""Return an error if the given type is TypeVoid."""

    match typ:
        case hydra.core.TypeVoid():
            return Just(cast(hydra.error.core.InvalidTypeError, hydra.error.core.InvalidTypeErrorVoidInNonBottomPosition(hydra.error.core.VoidInNonBottomPositionError(hydra.paths.SubtermPath(())))))

        case _:
            return Nothing()

def first_type_error(checks: frozenlist[Maybe[T0]]) -> Maybe[T0]:
    r"""Return the first type error from a list of optional errors, or nothing if all are valid."""

    return hydra.lib.lists.foldl((lambda acc, check: hydra.lib.maybes.cases(acc, (lambda : check), (lambda _: acc))), Nothing(), checks)

def term(typed: bool, g: hydra.graph.Graph, t: hydra.core.Term) -> Maybe[hydra.error.core.InvalidTermError]:
    r"""Validate a term, returning the first error found or nothing if valid. The 'typed' parameter indicates whether to expect System F (typed) terms; when true, type variable binding checks and UntypedTermVariableError are active."""

    return hydra.rewriting.fold_term_with_graph_and_path((lambda recurse, path, cx, acc, trm: hydra.lib.maybes.cases(acc, (lambda : (check_result := check_term(typed, hydra.paths.SubtermPath(path), cx, trm), hydra.lib.maybes.cases(check_result, (lambda : recurse(Nothing(), trm)), (lambda err: Just(err))))[1]), (lambda _: acc))), g, Nothing(), t)

def validate_type_node(bound_vars: frozenset[hydra.core.Name], typ: hydra.core.Type):
    def _hoist_hydra_validate_core_validate_type_node_1(elem_type, v1):
        match v1:
            case hydra.core.TypeFunction():
                return Just(cast(hydra.error.core.InvalidTypeError, hydra.error.core.InvalidTypeErrorNonComparableSetElementType(hydra.error.core.NonComparableSetElementTypeError(hydra.paths.SubtermPath(()), elem_type))))

            case _:
                return Nothing()
    match typ:
        case hydra.core.TypeAnnotated(value=ann):
            body = ann.body
            ann_map = ann.annotation
            def _hoist_ann_map_body_1(v1):
                match v1:
                    case hydra.core.TypeAnnotated():
                        return Just(cast(hydra.error.core.InvalidTypeError, hydra.error.core.InvalidTypeErrorNestedTypeAnnotation(hydra.error.core.NestedTypeAnnotationError(hydra.paths.SubtermPath(())))))

                    case _:
                        return Nothing()
            return first_type_error((hydra.lib.logic.if_else(hydra.lib.maps.null(ann_map), (lambda : Just(cast(hydra.error.core.InvalidTypeError, hydra.error.core.InvalidTypeErrorEmptyTypeAnnotation(hydra.error.core.EmptyTypeAnnotationError(hydra.paths.SubtermPath(())))))), (lambda : Nothing())), _hoist_ann_map_body_1(body)))

        case hydra.core.TypeEither(value=et):
            return first_type_error((check_void(et.left), check_void(et.right)))

        case hydra.core.TypeForall(value=ft):
            param_name = ft.parameter
            return first_type_error((hydra.lib.logic.if_else(hydra.lib.sets.member(param_name, bound_vars), (lambda : Just(cast(hydra.error.core.InvalidTypeError, hydra.error.core.InvalidTypeErrorTypeVariableShadowingInForall(hydra.error.core.TypeVariableShadowingInForallError(hydra.paths.SubtermPath(()), param_name))))), (lambda : Nothing())), hydra.lib.logic.if_else(is_valid_name(param_name), (lambda : Nothing()), (lambda : Just(cast(hydra.error.core.InvalidTypeError, hydra.error.core.InvalidTypeErrorInvalidForallParameterName(hydra.error.core.InvalidForallParameterNameError(hydra.paths.SubtermPath(()), param_name))))))))

        case hydra.core.TypeFunction(value=ft2):
            return check_void(ft2.codomain)

        case hydra.core.TypeList(value=lt):
            return check_void(lt)

        case hydra.core.TypeMap(value=mt):
            key_type = mt.keys
            def _hoist_key_type_body_1(v1):
                match v1:
                    case hydra.core.TypeFunction():
                        return Just(cast(hydra.error.core.InvalidTypeError, hydra.error.core.InvalidTypeErrorNonComparableMapKeyType(hydra.error.core.NonComparableMapKeyTypeError(hydra.paths.SubtermPath(()), key_type))))

                    case _:
                        return Nothing()
            return first_type_error((_hoist_key_type_body_1(key_type), check_void(key_type), check_void(mt.values)))

        case hydra.core.TypePair(value=pt):
            return first_type_error((check_void(pt.first), check_void(pt.second)))

        case hydra.core.TypeRecord(value=fields):
            return first_type_error((hydra.lib.logic.if_else(hydra.lib.lists.null(fields), (lambda : Just(cast(hydra.error.core.InvalidTypeError, hydra.error.core.InvalidTypeErrorEmptyRecordType(hydra.error.core.EmptyRecordTypeError(hydra.paths.SubtermPath(())))))), (lambda : Nothing())), check_duplicate_field_types(fields, (lambda dup_name: Just(cast(hydra.error.core.InvalidTypeError, hydra.error.core.InvalidTypeErrorDuplicateRecordTypeFieldNames(hydra.error.core.DuplicateRecordTypeFieldNamesError(hydra.paths.SubtermPath(()), dup_name)))))), first_type_error(hydra.lib.lists.map((lambda f: check_void(f.type)), fields))))

        case hydra.core.TypeSet(value=elem_type):
            return first_type_error((_hoist_hydra_validate_core_validate_type_node_1(elem_type, elem_type), check_void(elem_type)))

        case hydra.core.TypeUnion(value=fields2):
            return first_type_error((hydra.lib.logic.if_else(hydra.lib.lists.null(fields2), (lambda : Just(cast(hydra.error.core.InvalidTypeError, hydra.error.core.InvalidTypeErrorEmptyUnionType(hydra.error.core.EmptyUnionTypeError(hydra.paths.SubtermPath(())))))), (lambda : Nothing())), hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(fields2), 1), (lambda : (single_field := hydra.lib.lists.head(fields2), Just(cast(hydra.error.core.InvalidTypeError, hydra.error.core.InvalidTypeErrorSingleVariantUnion(hydra.error.core.SingleVariantUnionError(hydra.paths.SubtermPath(()), single_field.name)))))[1]), (lambda : Nothing())), check_duplicate_field_types(fields2, (lambda dup_name: Just(cast(hydra.error.core.InvalidTypeError, hydra.error.core.InvalidTypeErrorDuplicateUnionTypeFieldNames(hydra.error.core.DuplicateUnionTypeFieldNamesError(hydra.paths.SubtermPath(()), dup_name)))))), first_type_error(hydra.lib.lists.map((lambda f: check_void(f.type)), fields2))))

        case hydra.core.TypeVariable(value=var_name):
            return hydra.lib.logic.if_else(hydra.lib.sets.member(var_name, bound_vars), (lambda : Nothing()), (lambda : Just(cast(hydra.error.core.InvalidTypeError, hydra.error.core.InvalidTypeErrorUndefinedTypeVariable(hydra.error.core.UndefinedTypeVariableError(hydra.paths.SubtermPath(()), var_name))))))

        case _:
            return Nothing()

def type(bound_vars: frozenset[hydra.core.Name], typ: hydra.core.Type):
    r"""Validate a type, returning the first error found or nothing if valid. The first argument is the set of type variables already in scope."""

    @lru_cache(1)
    def check_result() -> Maybe[hydra.error.core.InvalidTypeError]:
        return validate_type_node(bound_vars, typ)
    def _hoist_check_result_body_1(v1):
        match v1:
            case hydra.core.TypeForall(value=ft):
                @lru_cache(1)
                def new_bound() -> frozenset[hydra.core.Name]:
                    return hydra.lib.sets.insert(ft.parameter, bound_vars)
                return type(new_bound(), ft.body)

            case hydra.core.TypeAnnotated(value=ann):
                return type(bound_vars, ann.body)

            case hydra.core.TypeApplication(value=at):
                return first_type_error((type(bound_vars, at.function), type(bound_vars, at.argument)))

            case hydra.core.TypeEither(value=et):
                return first_type_error((type(bound_vars, et.left), type(bound_vars, et.right)))

            case hydra.core.TypeFunction(value=ft):
                return first_type_error((type(bound_vars, ft.domain), type(bound_vars, ft.codomain)))

            case hydra.core.TypeList(value=lt):
                return type(bound_vars, lt)

            case hydra.core.TypeMap(value=mt):
                return first_type_error((type(bound_vars, mt.keys), type(bound_vars, mt.values)))

            case hydra.core.TypeMaybe(value=mt):
                return type(bound_vars, mt)

            case hydra.core.TypePair(value=pt):
                return first_type_error((type(bound_vars, pt.first), type(bound_vars, pt.second)))

            case hydra.core.TypeRecord(value=fields):
                return first_type_error(hydra.lib.lists.map((lambda f: type(bound_vars, f.type)), fields))

            case hydra.core.TypeSet(value=st):
                return type(bound_vars, st)

            case hydra.core.TypeUnion(value=fields):
                return first_type_error(hydra.lib.lists.map((lambda f: type(bound_vars, f.type)), fields))

            case hydra.core.TypeWrap(value=wt):
                return type(bound_vars, wt)

            case _:
                return Nothing()
    return hydra.lib.maybes.cases(check_result(), (lambda : _hoist_check_result_body_1(typ)), (lambda err: Just(err)))
