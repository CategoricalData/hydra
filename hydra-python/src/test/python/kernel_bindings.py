"""Kernel term bindings for the Python test suite.

These bindings mirror what's in hydra-haskell's Hydra.Sources.Kernel.Terms.Monads
and other kernel term modules. They're needed for evaluation tests that reference
kernel functions like hydra.monads.pure.

This is a temporary solution until we can load kernel modules from JSON properly.
"""

from hydra.dsl.python import FrozenDict, Just, Nothing
import hydra.core
import hydra.graph


# Helper to create common names
def _name(s: str) -> hydra.core.Name:
    return hydra.core.Name(s)


def _var(s: str) -> hydra.core.Term:
    return hydra.core.TermVariable(_name(s))


def _lambda(param: str, body: hydra.core.Term) -> hydra.core.Term:
    return hydra.core.TermFunction(hydra.core.FunctionLambda(hydra.core.Lambda(
        parameter=_name(param),
        domain=Nothing(),
        body=body
    )))


def _wrap(type_name: str, body: hydra.core.Term) -> hydra.core.Term:
    return hydra.core.TermWrap(hydra.core.WrappedTerm(
        type_name=_name(type_name),
        body=body
    ))


def _unwrap(type_name: str) -> hydra.core.Term:
    return hydra.core.TermFunction(hydra.core.FunctionElimination(
        hydra.core.EliminationWrap(_name(type_name))
    ))


def _record(type_name: str, fields: list[tuple[str, hydra.core.Term]]) -> hydra.core.Term:
    return hydra.core.TermRecord(hydra.core.Record(
        type_name=_name(type_name),
        fields=tuple(
            hydra.core.Field(name=_name(n), term=t)
            for n, t in fields
        )
    ))


def _project(type_name: str, field_name: str) -> hydra.core.Term:
    return hydra.core.TermFunction(hydra.core.FunctionElimination(
        hydra.core.EliminationRecord(hydra.core.Projection(
            type_name=_name(type_name),
            field=_name(field_name)
        ))
    ))


def _app(fn: hydra.core.Term, arg: hydra.core.Term) -> hydra.core.Term:
    return hydra.core.TermApplication(hydra.core.Application(
        function=fn,
        argument=arg
    ))


def _let(bindings: list[tuple[str, hydra.core.Term]], body: hydra.core.Term) -> hydra.core.Term:
    return hydra.core.TermLet(hydra.core.Let(
        bindings=tuple(
            hydra.core.Binding(name=_name(n), term=t, type=Nothing())
            for n, t in bindings
        ),
        body=body
    ))


def _maybe_just(term: hydra.core.Term) -> hydra.core.Term:
    return hydra.core.TermMaybe(Just(term))


def _maybe_nothing() -> hydra.core.Term:
    return hydra.core.TermMaybe(Nothing())


def _maybe_cases(nothing_case: hydra.core.Term, just_case: hydra.core.Term) -> hydra.core.Term:
    r"""Create a maybe eliminator: \mx -> case mx of Nothing -> nothing_case; Just x -> just_case x"""
    return hydra.core.TermFunction(hydra.core.FunctionElimination(
        hydra.core.EliminationMaybe(hydra.core.MaybeEliminator(
            nothing=nothing_case,
            just=just_case
        ))
    ))


def get_monads_bindings() -> dict[hydra.core.Name, hydra.core.Binding]:
    """Get the kernel monad bindings (hydra.monads.*)."""
    bindings = {}

    flow_type_name = "hydra.compute.Flow"
    flow_state_type_name = "hydra.compute.FlowState"

    # ========================================
    # hydra.monads.pure :: a -> Flow s a
    # pure = \xp -> Flow (\s -> \t -> FlowState (Just xp) s t)
    # ========================================
    pure_name = _name("hydra.monads.pure")

    pure_term = _lambda("xp",
        _wrap(flow_type_name,
            _lambda("s",
                _lambda("t",
                    _record(flow_state_type_name, [
                        ("value", _maybe_just(_var("xp"))),
                        ("state", _var("s")),
                        ("trace", _var("t")),
                    ])
                )
            )
        )
    )

    bindings[pure_name] = hydra.core.Binding(
        name=pure_name,
        term=pure_term,
        type=Nothing()
    )

    # ========================================
    # hydra.monads.map :: (a -> b) -> Flow s a -> Flow s b
    # map = \f -> \f1 -> Flow (\s0 -> \t0 ->
    #   let f2 = unwrap Flow f1 s0 t0 in
    #   FlowState (Maybes.map f (value f2)) (state f2) (trace f2))
    # ========================================
    map_name = _name("hydra.monads.map")

    # fmap for Maybe: \f -> \mx -> case mx of Nothing -> Nothing; Just x -> Just (f x)
    # We use the primitive: hydra.lib.maybes.map
    maybes_map = hydra.core.TermFunction(hydra.core.FunctionPrimitive(_name("hydra.lib.maybes.map")))

    map_term = _lambda("f",
        _lambda("f1",
            _wrap(flow_type_name,
                _lambda("s0",
                    _lambda("t0",
                        _let([
                            ("f2", _app(_app(_app(_unwrap(flow_type_name), _var("f1")), _var("s0")), _var("t0")))
                        ],
                            _record(flow_state_type_name, [
                                ("value", _app(_app(maybes_map, _var("f")),
                                    _app(_project(flow_state_type_name, "value"), _var("f2")))),
                                ("state", _app(_project(flow_state_type_name, "state"), _var("f2"))),
                                ("trace", _app(_project(flow_state_type_name, "trace"), _var("f2"))),
                            ])
                        )
                    )
                )
            )
        )
    )

    bindings[map_name] = hydra.core.Binding(
        name=map_name,
        term=map_term,
        type=Nothing()
    )

    # ========================================
    # hydra.monads.bind :: Flow s a -> (a -> Flow s b) -> Flow s b
    # bind = \l -> \r -> Flow (\s0 -> \t0 ->
    #   let fs1 = unwrap Flow l s0 t0 in
    #   maybe (FlowState Nothing (state fs1) (trace fs1))
    #         (\v -> unwrap Flow (r v) (state fs1) (trace fs1))
    #         (value fs1))
    # ========================================
    bind_name = _name("hydra.monads.bind")

    # For the maybe case, we need:
    # - nothing case: FlowState Nothing (state fs1) (trace fs1)
    # - just case: \v -> unwrap Flow (r v) (state fs1) (trace fs1)

    bind_term = _lambda("l",
        _lambda("r",
            _wrap(flow_type_name,
                _lambda("s0",
                    _lambda("t0",
                        _let([
                            ("fs1", _app(_app(_app(_unwrap(flow_type_name), _var("l")), _var("s0")), _var("t0")))
                        ],
                            # maybe nothing_case just_case (value fs1)
                            _app(
                                _app(
                                    _app(
                                        hydra.core.TermFunction(hydra.core.FunctionPrimitive(_name("hydra.lib.maybes.maybe"))),
                                        # nothing case
                                        _record(flow_state_type_name, [
                                            ("value", _maybe_nothing()),
                                            ("state", _app(_project(flow_state_type_name, "state"), _var("fs1"))),
                                            ("trace", _app(_project(flow_state_type_name, "trace"), _var("fs1"))),
                                        ])
                                    ),
                                    # just case: \v -> unwrap Flow (r v) (state fs1) (trace fs1)
                                    _lambda("v",
                                        _app(_app(_app(_unwrap(flow_type_name),
                                            _app(_var("r"), _var("v"))),
                                            _app(_project(flow_state_type_name, "state"), _var("fs1"))),
                                            _app(_project(flow_state_type_name, "trace"), _var("fs1")))
                                    )
                                ),
                                # (value fs1)
                                _app(_project(flow_state_type_name, "value"), _var("fs1"))
                            )
                        )
                    )
                )
            )
        )
    )

    bindings[bind_name] = hydra.core.Binding(
        name=bind_name,
        term=bind_term,
        type=Nothing()
    )

    # ========================================
    # hydra.monads.fail :: String -> Flow s a
    # fail = \msg -> Flow (\s -> \t ->
    #   FlowState Nothing s (pushError msg t))
    # ========================================
    fail_name = _name("hydra.monads.fail")

    fail_term = _lambda("msg",
        _wrap(flow_type_name,
            _lambda("s",
                _lambda("t",
                    _record(flow_state_type_name, [
                        ("value", _maybe_nothing()),
                        ("state", _var("s")),
                        ("trace", _app(_app(_var("hydra.monads.pushError"), _var("msg")), _var("t"))),
                    ])
                )
            )
        )
    )

    bindings[fail_name] = hydra.core.Binding(
        name=fail_name,
        term=fail_term,
        type=Nothing()
    )

    # ========================================
    # hydra.monads.pushError :: String -> Trace -> Trace
    # Simplified version that just prepends "Error: msg ()" to messages
    # ========================================
    push_error_name = _name("hydra.monads.pushError")
    trace_type_name = "hydra.compute.Trace"

    # Simplified: pushError msg t = Trace (stack t) (cons ("Error: " ++ msg ++ " ()") (messages t)) (other t)
    push_error_term = _lambda("msg",
        _lambda("t",
            _record(trace_type_name, [
                ("stack", _app(_project(trace_type_name, "stack"), _var("t"))),
                ("messages", _app(
                    _app(
                        hydra.core.TermFunction(hydra.core.FunctionPrimitive(_name("hydra.lib.lists.cons"))),
                        _app(
                            _app(
                                hydra.core.TermFunction(hydra.core.FunctionPrimitive(_name("hydra.lib.strings.cat"))),
                                hydra.core.TermList((
                                    hydra.core.TermLiteral(hydra.core.LiteralString("Error: ")),
                                    _var("msg"),
                                    hydra.core.TermLiteral(hydra.core.LiteralString(" (")),
                                    # Simplified: we skip the stack trace and just close the parens
                                    _app(
                                        _app(
                                            hydra.core.TermFunction(hydra.core.FunctionPrimitive(_name("hydra.lib.strings.intercalate"))),
                                            hydra.core.TermLiteral(hydra.core.LiteralString(" > "))
                                        ),
                                        _app(_project(trace_type_name, "stack"), _var("t"))
                                    ),
                                    hydra.core.TermLiteral(hydra.core.LiteralString(")")),
                                ))
                            ),
                            hydra.core.TermUnit()  # Dummy arg for cat which takes a list
                        )
                    ),
                    _app(_project(trace_type_name, "messages"), _var("t"))
                )),
                ("other", _app(_project(trace_type_name, "other"), _var("t"))),
            ])
        )
    )

    bindings[push_error_name] = hydra.core.Binding(
        name=push_error_name,
        term=push_error_term,
        type=Nothing()
    )

    # ========================================
    # hydra.monads.withTrace :: String -> Flow s a -> Flow s a
    # withTrace label flow = Flow (\s -> \t ->
    #   let t1 = Trace (cons label (stack t)) (messages t) (other t) in
    #   let fs = unwrap Flow flow s t1 in
    #   FlowState (value fs) (state fs) (Trace (stack t) (messages (trace fs)) (other (trace fs))))
    #
    # Simplified version without let bindings - inline everything
    # ========================================
    with_trace_name = _name("hydra.monads.withTrace")

    # Helper to inline t1 = Trace (cons label (stack t)) (messages t) (other t)
    def make_t1():
        return _record(trace_type_name, [
            ("stack", _app(
                _app(
                    hydra.core.TermFunction(hydra.core.FunctionPrimitive(_name("hydra.lib.lists.cons"))),
                    _var("label")
                ),
                _app(_project(trace_type_name, "stack"), _var("t"))
            )),
            ("messages", _app(_project(trace_type_name, "messages"), _var("t"))),
            ("other", _app(_project(trace_type_name, "other"), _var("t"))),
        ])

    # Helper to inline fs = unwrap Flow flow s t1
    def make_fs():
        return _app(_app(_app(_unwrap(flow_type_name), _var("flow")), _var("s")), make_t1())

    with_trace_term = _lambda("label",
        _lambda("flow",
            _wrap(flow_type_name,
                _lambda("s",
                    _lambda("t",
                        # FlowState (value fs) (state fs) (Trace (stack t) (messages (trace fs)) (other (trace fs)))
                        _record(flow_state_type_name, [
                            ("value", _app(_project(flow_state_type_name, "value"), make_fs())),
                            ("state", _app(_project(flow_state_type_name, "state"), make_fs())),
                            ("trace", _record(trace_type_name, [
                                ("stack", _app(_project(trace_type_name, "stack"), _var("t"))),
                                ("messages", _app(_project(trace_type_name, "messages"),
                                    _app(_project(flow_state_type_name, "trace"), make_fs()))),
                                ("other", _app(_project(trace_type_name, "other"),
                                    _app(_project(flow_state_type_name, "trace"), make_fs()))),
                            ])),
                        ])
                    )
                )
            )
        )
    )

    bindings[with_trace_name] = hydra.core.Binding(
        name=with_trace_name,
        term=with_trace_term,
        type=Nothing()
    )

    return bindings


def get_annotations_bindings() -> dict[hydra.core.Name, hydra.core.Binding]:
    """Get the kernel annotation bindings (hydra.annotations.*)."""
    # TODO: Add annotation bindings if needed
    return {}


def get_all_kernel_bindings() -> dict[hydra.core.Name, hydra.core.Binding]:
    """Get all kernel term bindings needed for tests."""
    bindings = {}
    bindings.update(get_monads_bindings())
    bindings.update(get_annotations_bindings())
    return bindings
