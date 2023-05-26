from typing import Any

from utils import bind
from syntax import Binop, Var, Int, Seq, Print, Assign, Skip, Env
from relation import Relation

step = Relation("S")


@step.rule("Binop", matches=[Env, Any, Binop])
def calc_binop(run, env, out, binop):
    match binop.op, binop.lhs, binop.rhs:
        case "+", Int(v1), Int(v2):
            return (env, out, Int(v1 + v2))
        case "-", Int(v1), Int(v2):
            return (env, out, Int(v1 - v2))
        case "*", Int(v1), Int(v2):
            return (env, out, Int(v1 * v2))
        case "/", Int(v1), Int(v2):
            return (env, out, Int(v1 // v2))


@step.rule("P-Binop-left", matches=[Env, Any, Binop])
def calc_binop_left(run, env, out, binop):
    (env, out, lhs) = run(step, env, out, binop.lhs)
    return (env, out, Binop(binop.op, lhs, binop.rhs))


@step.rule("P-Binop-right", matches=[Env, Any, Binop])
def calc_binop_right(run, env, out, binop):
    match binop.lhs:
        case Int(_):
            (env, out, rhs) = run(step, env, out, binop.rhs)
            return (env, out, Binop(binop.op, binop.lhs, rhs))


@step.rule("Var", matches=[Env, Any, Var])
def calc_var(_, env, out, var):
    if var in env:
        return (env, out, env[var])


@step.rule("Skip", matches=[Env, Any, Seq])
def calc_skip(run, env, out, seq):
    match seq.lhs:
        case Skip():
            return (env, out, seq.rhs)


@step.rule("P-Seq", matches=[Env, Any, Seq])
def calc_skip_(run, env, out, seq):
    (env, out, lhs) = run(step, env, out, seq.lhs)
    return (env, out, Seq(lhs, seq.rhs))


@step.rule("Print", matches=[Env, Any, Print])
def calc_print(run, env, out, print_):
    match print_.expr:
        case Int(v):
            return (env, [v], Skip())


@step.rule("P-Print", matches=[Env, Any, Print])
def calc_print_(run, env, out, print_):
    (env, out, expr) = run(step, env, out, print_.expr)
    return (env, out, Print(expr))


@step.rule("Assign", matches=[Env, Any, Assign])
def calc_assign(run, env, out, assign):
    match assign.rhs:
        case Int(v):
            return (bind(env, assign.lhs, v), out, Skip())


@step.rule("P-Assign", matches=[Env, Any, Assign])
def calc_assign_(run, env, out, assign):
    (env, out, rhs) = run(step, env, out, assign.rhs)
    return (env, out, Assign(assign.lhs, rhs))


step_star = Relation("S*")


@step_star.rule("Single")
def calc_single(run, *state):
    return state


@step_star.rule("Multi")
def calc_multi(run, *state):
    state = run(step, *state)
    return run(step_star, *state)


print(step, step_star)
x = Var("x")
example = Seq(Assign(x, Int(2)), Print(x * x))
g = step_star.derivation_graph({}, example)
g.view()
