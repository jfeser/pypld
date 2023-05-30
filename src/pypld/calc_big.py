from typing import Any

from utils import bind
from syntax import Binop, Var, Int, Seq, Print, Assign, Skip
from relation import BigStepRelation, rule

expr = BigStepRelation("E")
stmt = BigStepRelation("S")


@rule("Binop", matches=[Any, Binop])
def binop(run, env, binop):
    v1 = run(expr, env, binop.lhs)
    v2 = run(expr, env, binop.rhs)
    match binop.op:
        case "+":
            return v1 + v2
        case "-":
            return v1 - v2
        case "*":
            return v1 * v2
        case "/":
            return v1 // v2


@rule("Var", matches=[Any, Var])
def var(_, env, var):
    if var in env:
        return env[var]


@rule("Int", matches=[Any, Int])
def int_(_1, _2, n):
    return n.value


expr.add_rules(binop, var, int_)


@rule("Seq", matches=[Any, Seq])
def seq(run, env, seq):
    (env1, out1) = run(stmt, env, seq.lhs)
    (env2, out2) = run(stmt, env1, seq.rhs)
    return (env2, out1 + out2)


@rule("Print", matches=[Any, Print])
def print_(run, env, print_):
    v = run(expr, env, print_.expr)
    return (env, [v])


@rule("Assign", matches=[Any, Assign])
def assign(run, env, assign):
    value = run(expr, env, assign.rhs)
    return (bind(env, assign.lhs, value), [])


@rule("Skip", matches=[Any, Skip])
def skip(run, env, skip):
    return (env, [])


stmt.add_rules(seq, print_, assign, skip)

x = Var("x")
example = Seq(Assign(x, Int(2)), Print(x * x))
g = stmt.derivation_graph({}, example)
g.view()
