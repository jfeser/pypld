import dataclasses
from dataclasses import dataclass, replace
from typing import Optional, TypeVar, dataclass_transform

from pypld.utils import fresh, texescape


T = TypeVar("T")


@dataclass_transform()
def ast(cls):
    return dataclass(cls, frozen=True, unsafe_hash=True)


precedence = {
    "not": 1,
    "ref": 1,
    "*": 3,
    "/": 3,
    "and": 3,
    "or": 3,
    "+": 4,
    "-": 4,
    "<": 5,
    ">": 5,
    ">=": 5,
    "<=": 5,
    "=": 6,
    "!=": 6,
    ":=": 7,
    "!": 8,
}


@ast
class Term:
    def free(self) -> set["Var"]:
        """Return the free variables in a type."""
        raise NotImplementedError

    def subst(self, var, term):
        """Substitute variables in a type."""
        raise NotImplementedError

    def tex(self, prec=0) -> str:
        """Return a LaTeX representation of a type."""
        raise NotImplementedError


@ast
class Type(Term):
    pass


@ast
class Expr(Term):
    def __lt__(self, other):
        return Binop("<", self, other)

    def __gt__(self, other):
        return Binop(">", self, other)

    def __le__(self, other):
        return Binop("or", [Binop("<", self, other), Binop("=", self, other)])

    def __ge__(self, other):
        return Binop("or", [Binop(">", self, other), Binop("=", self, other)])

    def __eq__(self, other):
        return Binop("=", self, other)

    def __ne__(self, other):
        return Binop("not", [Binop("=", self, other)])

    def __add__(self, other):
        return Binop("+", self, other)

    def __sub__(self, other):
        return Binop("-", self, other)

    def __mul__(self, other):
        return Binop("*", self, other)

    def __truediv__(self, other):
        return Binop("/", self, other)

    def __and__(self, other):
        return Binop("and", self, other)

    def __or__(self, other):
        return Binop("or", self, other)

    def __invert__(self):
        return Unop("not", self)


@ast
class Var(Type, Expr):
    """Type variables"""

    name: str

    def free(self):
        return set([self])

    def subst(self, var, term):
        return term if self == var else self

    def tex(self, prec=0):
        return f"\\mathsf{{{texescape(self.name)}}}"


def simple_free(self) -> set[Var]:
    """Take the union of free of all fields of self that have a free method"""
    return set.union(
        *[getattr(self, f.name).free() for f in dataclasses.fields(self) if "free" in getattr(self, f.name).__dict__]
    )


def simple_subst(self, var: Var, term: Term):
    """Substitute in all fields of self that have a subst method"""
    return replace(
        self,
        **{
            f.name: getattr(self, f.name).subst(var, term)
            for f in dataclasses.fields(self)
            if "subst" in getattr(self, f.name).__dict__
        },
    )


class _PrimT:
    def type_free(self):
        return set()

    def type_subst(self, _1, _2):
        return self


@ast
class IntT(Type, _PrimT):
    """Integer types"""

    def tex(self, prec=0):
        return "\\mathsf{Int}"


@ast
class BoolT(Type, _PrimT):
    """Boolean types"""

    def tex(self, prec=0):
        return "\\mathsf{Bool}"


@ast
class ArrowT(Type):
    """Function types"""

    lhs: Type
    rhs: Type

    free = simple_free
    subst = simple_subst

    def tex(self, prec=0):
        body = f"{self.lhs.tex(10)} \\arrow {self.rhs.tex()}"
        return body if prec <= 9 else f"({body})"


@ast
class _QuantT(Type):
    """Quantified types"""

    symbol: str
    var: Var
    type_: Type

    def free(self) -> set[Var]:
        return self.type_.free() - {self.var}

    def subst(self, var, term: Term):
        if self.var == var:
            return self

        term_free = term.free()
        if self.var in term_free:
            new_var = fresh(self.type_.free() | term_free)
            new_type = self.type_.subst(self.var, Var(new_var))
            return replace(self, var=new_var, type_=new_type).subst(var, term)

        return replace(self, type_=self.type_.subst(var, term))

    def tex(self, prec=0):
        body = f"{self.symbol} {texescape(self.var)}.\\ {self.type_.tex()}"
        return body if prec <= 9 else f"({body})"


@ast
class ExistsT(_QuantT):
    """Existential types"""

    def __init__(self, var, type_):
        super().__init__("\\exists", var, type_)


@ast
class ForallT(_QuantT):
    """Universal types"""

    def __init__(self, var, type_):
        super().__init__("\\exists", var, type_)


@ast
class RecordT(Type):
    """Record types"""

    fields: dict[str, Type]

    def free(self) -> set[Var]:
        return set.union(*[t.free() for t in self.fields.values()])

    def subst(self, var: Var, type_: Type):
        return RecordT({k: t.subst(var, type_) for (k, t) in self.fields.items()})

    def tex(self, prec=0):
        body = ", ".join(f"{name}: {type_.tex()}" for name, type_ in self.fields.items())
        return f"\\{{{body}\\}}"


@ast
class TopT(Type, _PrimT):
    """Top type"""

    def tex(self, prec=0):
        return "\\top"


class _Const:
    def free(self):
        return set()

    def subst(self, _1, _2):
        return self


@ast
class Int(Expr, _Const):
    """Integers"""

    value: int

    def tex(self, prec=0):
        return str(self.value)


@ast
class Bool(Expr, _Const):
    """Booleans"""

    value: bool

    def tex(self, prec=0):
        return str(self.value)


@ast
class Ite(Expr):
    """If-then-else expressions"""

    cond: Expr
    then: Expr
    else_: Expr

    free = simple_free
    subst = simple_subst

    def tex(self, prec=0):
        body = f"\\ite{{{self.cond.tex()}}}{{{self.then.tex()}}}{{{self.else_.tex()}}}"
        return body if prec <= 9 else f"({body})"


@ast
class App(Expr):
    """Function application"""

    lhs: Expr
    rhs: Expr

    free = simple_free
    subst = simple_subst

    def tex(self, prec=0):
        body = f"{self.lhs.tex(10)}\\ {self.rhs.tex(10)}"
        return body if 10 > prec else f"({body})"


@ast
class Lam(Expr):
    """Anonymous functions"""

    var: Var
    type_: Optional[Type]
    body: Expr

    def __init__(self, *args):
        match args:
            case [var, type_, body]:
                self.var = var
                self.type_ = type_
                self.body = body
            case [var, body]:
                self.var = var
                self.type_ = None
                self.body = body

    def free(self):
        return (self.body.free() - {self.var}) | self.type_.free()

    def subst(self, var, expr):
        if var == self.var:
            return self

        free = expr.free()
        if self.var in free:
            new_var = fresh(self.body.free() | free)
            new_body = self.body.subst(self.var, Var(new_var))
            return Lam(new_var, self.type_, new_body).subst(var, expr)

        return Lam(self.var, self.type_.subst(var, expr), self.body.subst(var, expr))

    def tex(self, prec=0):
        body = f"\\lambda {self.var.tex()}"
        if self.type_ is not None:
            body += f": {self.type_.tex()}"
        body += f".\\ {self.body.tex()}"
        return body if prec <= 9 else f"({body})"


@ast
class TypeAbs(Expr):
    """Type abstraction"""

    var: Var
    body: Expr

    def free(self):
        return self.body.type_free() - {self.var}

    def subst(self, var, term):
        if var == self.var:
            return self

        free = term.free()
        if self.var in free:
            new_var = Var(fresh(self.body.free() | free))
            new_body = self.body.subst(self.var, new_var)
            return TypeAbs(new_var, new_body).subst(var, term)

        return TypeAbs(self.var, self.body.subst(var, term))

    def tex(self, prec=0):
        body = f"\\tabs{{{self.var.tex()}}}{{{self.body.tex(10)}}}"
        return body if prec <= 9 else f"({body})"


@ast
class TypeApp(Expr):
    """Type application"""

    lhs: Expr
    rhs: Type

    free = simple_free
    subst = simple_subst

    def tex(self, prec=0):
        body = f"{self.lhs.tex(10)} [{self.rhs.tex()}]"
        return body if prec <= 9 else f"({body})"


@ast
class Record(Expr):
    """Records"""

    fields: dict[str, Expr]

    def free(self):
        return set.union(*[e.free() for e in self.fields.values()])

    def subst(self, var, expr):
        return Record({k: v.subst(var, expr) for k, v in self.fields.items()})


@ast
class Proj(Expr):
    """Record field access (projection)"""

    expr: Expr
    field: str

    free = simple_free
    subst = simple_subst

    def tex(self, prec=0):
        return f"{self.expr.tex(10)} \\proj {self.field}"


@ast
class Unop(Expr):
    op: str
    expr: Expr

    free = simple_free
    subst = simple_subst

    def tex(self, prec=0):
        op_prec = precedence.get(self.op, 0)
        body = f"{self.op} {self.expr.tex(op_prec)}"
        return body if prec < op_prec else f"({body})"


@ast
class Ref(Unop):
    """Reference"""

    def __init__(self, expr):
        super().__init__("ref", expr)


@ast
class Deref(Unop):
    """Dereference"""

    def __init__(self, expr):
        super().__init__("!", expr)


@ast
class Binop(Expr):
    op: str
    lhs: Expr
    rhs: Expr

    free = simple_free
    subst = simple_subst

    def tex(self, prec=0):
        op_prec = precedence.get(self.op, 0)
        body = f"{self.lhs.tex(op_prec)} {self.op} {self.rhs.tex(op_prec)}"
        return body if op_prec > prec else f"({body})"


@ast
class Set(Binop):
    """Reference cell assignment"""

    def __init__(self, lhs, rhs):
        super().__init__(":=", lhs, rhs)


@ast
class Loc(Expr, _Const):
    """Location"""

    loc: int

    def tex(self, prec=0):
        return f"\\loc{{{self.loc}}}"


@ast
class Unit(Expr, _Const):
    """Unit"""

    def tex(self, prec=0):
        return "()"


@ast
class Let(Expr):
    """Let binding"""

    var: Var
    type_: Optional[Type]
    expr: Expr
    body: Expr

    def __init__(self, *args):
        match args:
            case [var, type_, expr, body]:
                self.var = var
                self.type_ = type_
                self.expr = expr
                self.body = body
            case [var, expr, body]:
                self.var = var
                self.type_ = None
                self.expr = expr
                self.body = body

    def free(self) -> set[Var]:
        return self.expr.free() | (self.body.free() - {self.var}) | (set() if self.type_ is None else self.type_.free())

    def subst(self, var: Var, expr: Term):
        if var == self.var:
            return self

        free = expr.free()
        if self.var in free:
            new_var = fresh(self.body.free() | free)
            new_body = self.body.subst(self.var, Var(new_var))
            return Let(new_var, self.type_, self.expr, new_body).subst(var, expr)

        return Let(
            self.var,
            self.type_.subst(var, expr) if self.type_ is not None else None,
            self.expr.subst(var, expr),
            self.body.subst(var, expr),
        )

    def tex(self, prec=0):
        body = f"\\letu{{{self.var.tex()}}}{{{self.rhs.tex()}}}{{{self.body.tex()}}}"
        return body if 10 > prec else f"({body})"


@ast
class Pack(Expr):
    """Pack"""

    hidden: Type
    expr: Expr
    intf: Type

    free = simple_free
    subst = simple_subst

    def tex(self, prec=0):
        body = f"\\pack{{{self.hidden.tex()}}}{{{self.expr.tex()}}}{{{self.intf.tex()}}}"
        return body if 10 > prec else f"({body})"


@ast
class Unpack(Expr):
    """Unpack"""

    var: Var
    tvar: Var
    expr: Expr
    body: Expr

    def free(self) -> set[Var]:
        return self.expr.free() | (self.body.free() - {self.var, self.tvar})

    def subst(self, var: Var, expr: Term) -> Expr:
        if var == self.var:
            return self

        free = expr.free()
        if self.var in free:
            new_var = fresh(self.body.free() | free)
            new_body = self.body.subst(self.var, Var(new_var))
            return Unpack(new_var, self.tvar, self.expr, new_body).subst(var, expr)

        return Unpack(
            self.var,
            self.tvar,
            self.expr.subst(var, expr),
            self.body.subst(var, expr),
        )

    def tex(self, prec=0):
        body = f"\\unpack{{{self.tvar.tex()}}}{{{self.var.tex()}}}{{{self.expr.tex()}}}{{{self.body.tex()}}}"
        return body if 10 > prec else f"({body})"


@ast
class Stmt(Term):
    pass


@ast
class Print(Stmt):
    expr: Expr

    subst = simple_subst
    free = simple_free

    def tex(self, prec=0):
        return f"\\print{{{self.expr.tex()}}}"


@ast
class Seq(Stmt):
    lhs: Stmt
    rhs: Stmt

    subst = simple_subst
    free = simple_free

    def tex(self, prec=0):
        body = f"{self.lhs.tex(10)}; {self.rhs.tex(10)}"
        return body if 10 > prec else f"({body})"


@ast
class Skip(Stmt):
    subst = simple_subst
    free = simple_free

    def tex(self, prec=0):
        return "\\skip"


@ast
class Assign(Stmt):
    lhs: Var
    rhs: Expr

    def subst(self, var: Var, term: Term):
        return replace(self, rhs=self.rhs.subst(var, term))

    def free(self) -> set[Var]:
        # TODO: should the lhs be free?
        return self.rhs.free() | {self.lhs}

    def tex(self, prec=0) -> str:
        return f"\\assign{{{self.lhs.tex()}}}{{{self.rhs.tex()}}}"


@ast
class If(Stmt):
    cond: Expr
    then: Stmt
    else_: Stmt

    subst = simple_subst
    free = simple_free

    def tex(self, prec=0) -> str:
        return f"\\if{{{self.cond.tex()}}}{{{self.then.tex()}}}{{{self.else_.tex()}}}"


@ast
class While(Stmt):
    cond: Expr
    body: Stmt

    subst = simple_subst
    free = simple_free

    def tex(self, prec=0) -> str:
        return f"\\while{{{self.cond.tex()}}}{{{self.body.tex()}}}"


@ast
class Env(dict):
    def tex(self, prec=0) -> str:
        if len(self) == 0:
            return "\\emptyset"
        else:
            return "\\{" + ", ".join(f"{var}: {term.tex()}" for (var, term) in self.items()) + "\\}"

    def bind(self, key, value):
        return Env({**self, key: value})

    def __setitem__(self, key, value):
        raise RuntimeError("Environments are immutable")
