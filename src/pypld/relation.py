import networkx as nx
import graphviz
from dataclasses import dataclass, field
from typing import Any, Optional

from pypld.utils import texescape


@dataclass
class Rule:
    name: str
    fn: Any
    types: Optional[list[Any]]


@dataclass
class StuckError(Exception):
    message: str


@dataclass(frozen=True, unsafe_hash=True)
class FreshNode:
    pass


class DerivationGraph(nx.DiGraph):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.fresh_id = 0

    def fresh_node_id(self):
        self.fresh_id += 1
        return str(self.fresh_id)


def rule(name: str, matches: Optional[list[Any]] = None):
    def decorator(fn):
        return Rule(name, fn, matches)

    return decorator


@dataclass
class _Relation:
    name: str
    rules: dict[str, Rule] = field(default_factory=dict)

    def __str__(self):
        return f'Relation("{self.name}", {list(self.rules.keys())})'

    def _matching_rules(self, args):
        for rule in self.rules.values():
            if rule.types is None or all(
                type_ is Any or isinstance(arg, type_) for (arg, type_) in zip(args, rule.types)
            ):
                yield (rule, args)

    def add_rules(self, *rules):
        for rule in rules:
            if rule.name in self.rules:
                raise ValueError(f"Rule {rule.name} already exists")
            self.rules[rule.name] = rule


class BigStepRelation(_Relation):
    def _str_judgement(self, lhs, rhs):
        return f"{lhs} =>{self.name} {rhs}"

    def _tex_judgement(self, lhs, rhs):
        return f"{lhs} \\Downarrow_{{\\mathit{{{texescape(self.name)}}}}} {rhs}"

    def _eval(self, args):
        for (rule, rule_args) in self._matching_rules(args):

            def run(rel, *args):
                return rel._eval(args)

            result = rule.fn(run, *rule_args)
            if result is not None:
                return result

        raise StuckError(f"No {self.name} rule applies to {args}")

    def eval_all(self, *args):
        """
        Evaluate the relation on the given arguments.

        Parameters:
        *args: The inputs to the relation.

        Returns:
        The output of the relation.
        """

        return self._eval(args)

    def eval(self, *args):
        """
        Evaluate the relation on the given arguments.

        Parameters:
        *args: The inputs to the relation.

        Returns:
        The output of the relation.
        """

        return self._eval(args)

    def _deriv_graph(self, graph, parent_id, args):
        for (rule, rule_args) in self._matching_rules(args):

            current_id = graph.fresh_node_id()

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
        """Return a DerivationGraph() that represents (one of) the derivation(s)
        produced by running the rules of this relation on the given arguments.

        Parameters:
        *args: The inputs to the relation.

        Returns:
        A DerivationGraph() object.

        Raises:
        StuckError: If

        """
        g = DerivationGraph()
        self._deriv_graph(g, None, args)
        return g


class SmallStepRelation(_Relation):
    def _str_judgement(self, lhs, rhs):
        return f"{lhs} ->{self.name} {rhs}"

    def _tex_judgement(self, lhs, rhs):
        return f"{lhs} \\rightarrow_{{\\mathit{{{texescape(self.name)}}}}} {rhs}"

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

    def derivation_graph(self, *state):
        """Return a DerivationGraph() that represents (one of) the derivation(s)
        produced by running the rules of this relation on the given arguments.

        Parameters:
        *args: The inputs to the relation.

        Returns:
        A DerivationGraph() object.

        Raises:
        StuckError: If

        """
        g = DerivationGraph()
        q = []
        self._deriv_graph(g, q, state)
        return g
