from __future__ import annotations
from dataclasses import dataclass
from typing import *

VarName = str

# Constants
@dataclass
class Const:
    pass

@dataclass
class CInt(Const):
    value: int

    def __str__(self) -> str:
        return f'CInt {self.value}'
    def __repr__(self) -> str:
        return self.__str__()

@dataclass
class CBool(Const):
    value: bool

    def __str__(self) -> str:
        return f'CBool {self.value}'
    def __repr__(self) -> str:
        return self.__str__()

# Types
@dataclass
class Type:
    def __str__(self) -> str:
        return f'{self.__class__.__name__}'
    def __repr__(self) -> str:
        return self.__str__()

@dataclass
class TyInt(Type):
    pass

@dataclass
class TyBool(Type):
    pass

# Operators
@dataclass
class Op:
    def __str__(self) -> str:
        return f'{self.__class__.__name__}'
    def __repr__(self) -> str:
        return self.__str__()

@dataclass
class OpAdd(Op):
    pass

@dataclass
class OpSub(Op):
    pass

@dataclass
class OpMul(Op):
    pass

@dataclass
class OpDiv(Op):
    pass

@dataclass
class OpMod(Op):
    pass

@dataclass
class OpLessThan(Op):
    pass

@dataclass
class OpEqual(Op):
    pass

@dataclass
class OpAnd(Op):
    pass

@dataclass
class OpOr(Op):
    pass

@dataclass
class OpNot(Op):
    pass

# Expressions
@dataclass
class Expr:
    pass

@dataclass
class ECst(Expr):
    value: Const

    def __str__(self) -> str:
        return f'ECst {self.value}'
    def __repr__(self) -> str:
        return self.__str__()

@dataclass
class EVar(Expr):
    var_name: VarName

    def __str__(self) -> str:
        return f"EVar '{self.var_name}'"
    def __repr__(self) -> str:
        return self.__str__()
    
@dataclass
class EBinOp(Expr):
    op: Op
    left: Expr
    right: Expr

    def __str__(self) -> str:
        return f'EBinOp {self.op} {self.left} {self.right}'
    def __repr__(self) -> str:
        return self.__str__()

@dataclass
class EUnaryOp(Expr):
    op: Op
    expr: Expr

    def __str__(self) -> str:
        return f'EUnaryOp {self.op} {self.expr}'
    def __repr__(self) -> str:
        return self.__str__()

# Commands
@dataclass
class Comm:
    pass

@dataclass
class CSkip(Comm):
    def __str__(self) -> str:
        return f'CSkip'
    
@dataclass
class CSeq(Comm):
    comm1: Comm
    comm2: Comm

    def __str__(self) -> str:
        return f'CSeq {self.comm1} {self.comm2}'
    def __repr__(self) -> str:
        return self.__str__()

@dataclass
class CAssign(Comm):
    var_name: VarName
    expr: Expr

    def __str__(self) -> str:
        return f'CAssign "{self.var_name}" {self.expr}'
    def __repr__(self) -> str:
        return self.__str__()

@dataclass
class CRead(Comm):
    var_name: VarName

    def __str__(self) -> str:
        return f'CRead {self.var_name}'
    def __repr__(self) -> str:
        return self.__str__()
    
@dataclass
class CWrite(Comm):
    expr: Expr

    def __str__(self) -> str:
        return f'CWrite {self.expr}'
    def __repr__(self) -> str:
        return self.__str__()

@dataclass
class CIf(Comm):
    expr: Expr
    comm1: Comm
    comm2: Comm

    def __str__(self) -> str:
        return f'CIf {self.expr} {self.comm1} {self.comm2}'
    def __repr__(self) -> str:
        return self.__str__()

@dataclass
class CWhile(Comm):
    expr: Expr
    comm: Comm

    def __str__(self) -> str:
        return f'CWhile {self.expr} {self.comm}'
    def __repr__(self) -> str:
        return self.__str__()
    
@dataclass
class CAssert(Comm):
    expr: Expr

    def __str__(self) -> str:
        return f'CAssert {self.expr}'
    def __repr__(self) -> str:
        return self.__str__()

# Programs    
@dataclass
class Prog:
    progDecls: progDecls
    progComms: progComms

    def __str__(self) -> str:
        return f'Prog {self.progDecls}, {self.progComms}'
    def __repr__(self) -> str:
        return self.__str__()

Decl = Tuple[Type, VarName]
Decls = List[Decl]
Comms = List[Comm]