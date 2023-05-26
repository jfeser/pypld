from typing import Any

from utils import bind
from syntax import Binop, Var, Int, Seq, Print, Assign, Skip
from relation import Relation

expr = Relation("E")
stmt = Relation("S")


@expr.rule("Binop", matches=[Any, Binop])
def calc_binop(run, env, binop):
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


@expr.rule("Var", matches=[Any, Var])
def calc_var(_, env, var):
    if var in env:
        return env[var]


@expr.rule("Int", matches=[Any, Int])
def calc_int(_1, _2, n):
    return n.value


@stmt.rule("Seq", matches=[Any, Seq])
def calc_seq(run, env, seq):
    (env1, out1) = run(stmt, env, seq.lhs)
    (env2, out2) = run(stmt, env1, seq.rhs)
    return (env2, out1 + out2)


@stmt.rule("Print", matches=[Any, Print])
def calc_print(run, env, print_):
    v = run(expr, env, print_.expr)
    return (env, [v])


@stmt.rule("Assign", matches=[Any, Assign])
def calc_assign(run, env, assign):
    value = run(expr, env, assign.rhs)
    return (bind(env, assign.lhs, value), [])


@stmt.rule("Skip", matches=[Any, Skip])
def calc_skip(run, env, skip):
    return (env, [])


x = Var("x")
example = Seq(Assign(x, Int(2)), Print(x * x))
g = stmt.derivation_graph({}, example)
g.view()
