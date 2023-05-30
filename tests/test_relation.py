from pypld.relation import BigStepRelation, rule
from pypld.syntax import Binop, Int, Term


def test_big_step_simple():
    e = BigStepRelation("E")

    @rule("Int", matches=[Int])
    def int_(_, n):
        return n

    @rule("Add", matches=[Binop])
    def add(run, term):
        if term.op == "+":
            return Int(run(e, term.lhs).value + run(e, term.rhs).value)

    @rule("Sub", matches=[Binop])
    def sub(run, term):
        if term.op == "-":
            return Int(run(e, term.lhs).value - run(e, term.rhs).value)

    e.add_rules(int_, add, sub)

    assert e.eval(Int(0)) == Int(0)
    assert e.eval(Binop("+", Int(1), Int(2))) == Int(3)
    assert e.eval(Binop("-", Int(1), Binop("+", Int(2), Int(4)))) == Int(-5)


def test_big_step_nondet():
    e = BigStepRelation("N")

    class Flip(Term):
        pass

    @rule("Flip0", matches=[Flip])
    def flip0(_, flip):
        return Int(0)

    @rule("Flip1", matches=[Flip])
    def flip1(_, flip):
        return Int(1)

    @rule("Add", matches=[Binop])
    def add(run, term):
        if term.op == "+":
            return Int(run(e, term.lhs).value + run(e, term.rhs).value)

    e.add_rules(flip0, flip1, add)

    assert e.eval(Flip()) in [Int(0), Int(1)]
    assert e.eval(Binop("+", Flip(), Flip())) in [Int(0), Int(1), Int(2)]
    assert e.eval_all(Flip()) == set([Int(0), Int(1)])
    assert e.eval_all(Binop("+", Flip(), Flip())) in set([Int(0), Int(1), Int(2)])
