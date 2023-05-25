import dataclasses
import graphviz
import functools
from dataclasses import dataclass
from typing import Callable, Any, Tuple, Optional

from syntax import *


def bind(s, x, v):
    return {**s, x: v}


@dataclass
class Rule:
    name: str
    fn: Any
    types: Optional[list[Any]]


class DerivationGraph(graphviz.Digraph):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.node_attr["shape"] = "box"
        self.fresh_id = 0

    def fresh_node_id(self):
        self.fresh_id += 1
        return str(self.fresh_id)


class Relation:
    def __init__(self, name):
        self.name = name
        self.rules = []

    def rule(self, name, matches=None):
        def decorator(fn):
            rule = Rule(name, fn, matches)
            self.rules.append(rule)
            return fn

        return decorator

    def __call__(self, *args):
        pass

    def _str_judgement(self, lhs, rhs):
        return f"{lhs} =>{self.name} {rhs}"

    def _tex_judgement(self, lhs, rhs):
        return f"{lhs} \\Downarrow_{{\mathit{{{self.name}}}}} {rhs}"

    def _matching_rules(self, args):
        for rule in self.rules:
            rule_args = []
            rule_matches = True
            if rule.types is None:
                yield rule
            else:
                for (arg, type_) in zip(args, rule.types):
                    if type_ is Any:
                        rule_args.append(arg)
                    else:
                        if not isinstance(arg, type_):
                            rule_matches = False
                            break
                        rule_args += [
                            getattr(arg, field.name)
                            for field in dataclasses.fields(type_)
                        ]

                if rule_matches:
                    yield (rule, rule_args)

    def eval(self, *args):
        for (rule, rule_args) in self._matching_rules(args):

            def run(rel, *args):
                return rel.eval(*args)

            result = rule.fn(run, *rule_args)
            if result is not None:
                return result

    def _deriv_graph(self, graph, parent_id, args):
        current_id = graph.fresh_node_id()
        for (rule, rule_args) in self._matching_rules(args):

            def run(rel, *args):
                return rel._deriv_graph(graph, current_id, args)

            result = rule.fn(run, *rule_args)
            if result is not None:
                graph.node(
                    current_id,
                    label=self._str_judgement(rule_args, result),
                    xlabel=f"{self.name}-{rule.name}",
                )
                if parent_id is not None:
                    graph.edge(current_id, parent_id)
                return result

    def derivation_graph(self, *args):
        g = DerivationGraph()
        self._deriv_graph(g, None, args)
        return g


expr = Relation("E")
stmt = Relation("S")


@expr.rule("Binop", matches=[Any, Binop])
def calc_binop(run, env, op, e1, e2):
    v1 = run(expr, env, e1)
    v2 = run(expr, env, e2)
    match op:
        case "+":
            return v1 + v2
        case "-":
            return v1 - v2
        case "*":
            return v1 * v2
        case "/":
            return v1 // v2


@expr.rule("Var", matches=[Any, Var])
def calc_var(_, env, x):
    if x in env:
        return env[x]


@expr.rule("Int", matches=[Any, Int])
def calc_int(_1, _2, n):
    return n


@stmt.rule("Seq", matches=[Any, Seq])
def calc_seq(run, env, lhs, rhs):
    (env1, out1) = run(stmt, env, lhs)
    (env2, out2) = run(stmt, env1, rhs)
    return (env2, out1 + out2)


@stmt.rule("Print", matches=[Any, Print])
def calc_print(run, env, e):
    v = run(expr, env, e)
    return (env, [v])


@stmt.rule("Assign", matches=[Any, Assign])
def calc_assign(run, env, var, e):
    value = run(expr, env, e)
    return (bind(env, var, value), [])


@stmt.rule("Skip", matches=[Any, Skip])
def calc_skip(_, env):
    return (env, [])


example = Seq(Assign("x", Int(2)), Print(Binop("*", Var("x"), Var("x"))))
g = stmt.derivation_graph({}, example)
g.view()
