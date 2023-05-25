from dataclasses import dataclass, replace
from typing import Optional

from utils import fresh, texescape


def ast(x):
    return dataclass(x, frozen=True)


@ast
class Type:
    def free(self) -> set[str]:
        """Return the free variables in a type."""
        raise NotImplementedError

    def subst(self, var: str, type_: "Type") -> "Type":
        """Substitute variables in a type."""
        raise NotImplementedError

    def tex(self, prec=0) -> str:
        """Return a LaTeX representation of a type."""
        raise NotImplementedError


@ast
class Expr:
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

    def free(self) -> set[str]:
        """Return the free variables in an expression."""
        raise NotImplementedError

    def subst(self, var: str, expr: "Expr") -> "Expr":
        """Substitute variables in an expression."""
        raise NotImplementedError


def simple_free(self):
    """Take the union of free of all fields of self that have a free method"""
    return set.union(
        *[
            getattr(self, f.name).free()
            for f in dataclasses.fields(self)
            if "free" in getattr(self, f.name).__dict__
        ]
    )


def simple_subst(self, var, expr):
    """Substitute in all fields of self that have a subst method"""
    return replace(
        self,
        **{
            f.name: getattr(self, f.name).subst(var, expr)
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
class VarT(Type):
    """Type variables"""

    name: str

    def free(self):
        return set([self])

    def subst(self, var, type_):
        return type_ if self == var else self


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
    var: VarT
    type_: Type

    def free(self):
        return self.type_.free() - {self.var}

    def subst(self, var: str, type_: Type) -> Type:
        if self.var == var:
            return self

        if self.var in type_.type_free():
            new_var = fresh(self.type_.type_free() | type_.type_free())
            new_type = self.type_.subst(self.var, VarT(new_var))
            return replace(self, var=new_var, type_=new_type.subst(var, type_))

        return replace(self, type_=self.type_.subst(var, type_))

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

    def free(self):
        return set.union(*[t.free() for t in self.fields.values()])

    def subst(self, var: str, type_: Type) -> Type:
        return RecordT({k: t.subst(var, type_) for (k, t) in self.fields.items()})

    def tex(self, prec=0):
        body = ", ".join(
            f"{name}: {type_.tex()}" for name, type_ in self.fields.items()
        )
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
class Var(Expr):
    """Variables"""

    name: str

    def free(self):
        return {self}

    def subst(self, var, expr: Expr):
        return expr if self == var else self

    def tex(self, prec=0):
        return f"\\mathit{{{texescape(self.name)}}}"


@ast
class Int(Expr, _Const):
    """Integers"""

    value: int


@ast
class Bool(Expr, _Const):
    """Booleans"""

    value: bool


@ast
class If(Expr):
    """If-then-else expressions"""

    cond: Expr
    then: Expr
    else_: Expr

    free = simple_free
    subst = simple_subst


@ast
class App(Expr):
    """Function application"""

    lhs: Expr
    rhs: Expr

    free = simple_free
    subst = simple_subst


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


@ast
class TypeAbs(Expr):
    """Type abstraction"""

    var: VarT
    body: Expr

    def subst(self, var: str, expr: Expr):
        # TODO: what if expr contains free type variables?
        return replace(self, body=self.body.subst(var, expr))

    def free(self):
        return self.body.type_free() - {self.var}

    def subst(self, var, term):
        if var == self.var:
            return self

        free = term.free()
        if self.var in free:
            new_var = VarT(fresh(self.body.free() | free))
            new_body = self.body.subst(self.var, new_var)
            return TypeAbs(new_var, new_body).subst(var, expr)

        return TypeAbs(self.var, self.body.subst(var, expr))


@ast
class TypeApp(Expr):
    """Type application"""

    lhs: Expr
    rhs: Type

    free = simple_free
    subst = simple_subst


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


@ast
class Unop(Expr):
    op: str
    expr: Expr

    free = simple_free
    subst = simple_subst


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
class Assign(Binop):
    """Assignment"""

    def __init__(self, lhs, rhs):
        super().__init__(":=", lhs, rhs)


@ast
class Loc(Expr, _Const):
    """Location"""

    loc: int


@ast
class Unit(Expr, _Const):
    """Unit"""

    pass


@ast
class Let(Expr):
    """Let binding"""

    var: str
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

    def free(self):
        return (
            self.expr.free()
            | (self.body.free() - {self.var})
            | (set() if self.type_ is None else self.type_.free())
        )

    def subst(self, var: str, expr: Expr):
        if var == self.var:
            return self

        free = expr.free()
        if self.var in free:
            new_var = fresh(self.body.free() | free)
            new_body = self.body.subst(self.var, Var(new_var))
            return Let(new_var, self.type_, self.expr, new_body).subst(var, expr)

        return Let(
            self.var,
            self.type_.subst(var, expr),
            self.expr.subst(var, expr),
            self.body.subst(var, expr),
        )


@ast
class Pack(Expr):
    """Pack"""

    hidden: Type
    expr: Expr
    intf: Type

    free = simple_free
    subst = simple_subst


@ast
class Unpack(Expr):
    """Unpack"""

    var: Var
    tvar: VarT
    expr: Expr
    body: Expr

    def free(self):
        return self.expr.free() | (self.body.free() - {self.var, self.tvar})

    def subst(self, var: str, expr: Expr):
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


@ast
class Binop(Expr):
    op: str
    lhs: Expr
    rhs: Expr


@ast
class Stmt:
    pass


@ast
class Print(Stmt):
    expr: Expr


@ast
class Seq(Stmt):
    lhs: Stmt
    rhs: Stmt


@ast
class Skip(Stmt):
    pass


def term_tex(expr: Expr, prec: int):
    """Convert a term to a string in LaTeX syntax."""
    match expr:
        case Prim(op, [lhs, rhs]):
            op_prec = operator_precedence(op)
            body = f"{term_tex(lhs, op_prec)} {op} {term_tex(rhs, op_prec)}"
            return body if op_prec > prec else f"({body})"
        case Prim(op, [e]):
            op_prec = operator_precedence(op)
            body = f"{op} {term_tex(e, op_prec)}"
            return body if op_prec > prec else f"({body})"
        case Int(val):
            return str(val)
        case Bool(val):
            return str(val)
        case Var("_"):
            return "\\_"
        case Var(name):
            return name
        case App(lhs, rhs):
            body = f"{term_tex(lhs, 10)}\\ {term_tex(rhs, 10)}"
            return body if 10 > prec else f"({body})"
        case Lam(var, None, body):
            body = f"\\lambda {term_tex(Var(var), 0)}.\\ {term_tex(body, 0)}"
            return body if 10 > prec else f"({body})"
        case Lam(var, type_, body):
            body = f"\\lambda {term_tex(Var(var), 0)} : {type_tex(type_, 0)}.\\ {term_tex(body, 0)}"
            return body if 10 > prec else f"({body})"
        case TypeAbs(var, body):
            body = f"\\tabs{{{var}}}{{{term_tex(body, 10)}}}"
            return body if 10 > prec else f"({body})"
        case TypeApp(lhs, rhs):
            body = f"{term_tex(lhs, 10)} [{type_tex(rhs, 0)}]"
        case Record(fields):
            body = ", ".join(
                f"{name} = {term_tex(expr, 0)}" for name, expr in fields.items()
            )
            return f"\\{{{body}\\}}"
        case Proj(expr, field):
            return f"{term_tex(expr, 10)} \\proj {field}"
        case Ref(expr):
            body = f"\\refc\\ {term_tex(expr, 10)}"
            return body if 10 > prec else f"({body})"
        case Deref(expr):
            return term_tex(Prim("!", [expr]), prec)
        case Assign(lhs, rhs):
            return term_tex(Prim(":=", [lhs, rhs]), prec)
        case Loc(loc):
            return f"\\loc{{{loc}}}"
        case Unit():
            return "()"
        case Let(var, None, rhs, body):
            body = f"\\letu{{{var}}}{{{term_tex(rhs, 0)}}}{{{term_tex(body, 0)}}}"
            return body if 10 > prec else f"({body})"
        case Pack(type_, expr, as_):
            body = f"\\pack{{{type_tex(type_, 0)}}}{{{term_tex(expr, 0)}}}{{{type_tex(as_, 0)}}}"
            return body if 10 > prec else f"({body})"
        case Unpack(var, tvar, expr, body):
            body = f"\\unpack{{{tvar}}}{{{var}}}{{{term_tex(expr, 0)}}}{{{term_tex(body, 0)}}}"
            return body if 10 > prec else f"({body})"
