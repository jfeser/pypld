from syntax import ast, Var


@ast
class Hole(Var):
    def tex(self, prec=0):
        return "\\square"


def plug(context, term):
    """Plug a term into a hole in a context."""
    return context.subst(Hole(), term)
