from AST import *
import json

def from_data_to_class(data):
    if isinstance(data, list):
        type_list = []
        for element in data:
            type_list.append(type(element))

        if len(set(type_list)) == 1:
            return [from_data_to_class(d) for d in data]
        else:
            return (from_data_to_class(data[0]), data[1])
        
    elif isinstance(data, dict):
        key, value = list(data.items())[0]

        if key == "Prog":
            return Prog(
                progDecls = from_data_to_class(value[0]),
                progComms = from_data_to_class(value[1])
            )
        
        # Commands
        if key == "CSkip":
            return CSkip()
        if key == "CSeq":
            return CSeq(
                comm1 = from_data_to_class(value[0]),
                comm2 = from_data_to_class(value[1])
            )
        if key == "CAssign":
            return CAssign(
                var_name = value[0],
                expr = from_data_to_class(value[1])
            )
        if key == "CRead":
            return CRead(
                var_name = value
            )
        if key == "CWrite":
            return CWrite(
                expr = from_data_to_class(value)
            )
        if key == "CIf":
            return CIf(
                expr = from_data_to_class(value[0]),
                comm1 = from_data_to_class(value[1]),
                comm2 = from_data_to_class(value[2])
            )
        if key == "CWhile":
            return CWhile(
                expr = from_data_to_class(value[0]),
                comm = from_data_to_class(value[1])
            )
        if key == "CAssert":
            return CAssert(
                expr = from_data_to_class(value)
            )
        
        # Expressions
        if key == "ECst":
            return ECst(
                value = from_data_to_class(value)
            )
        if key == "EVar":
            return EVar(
                var_name = value
            )
        if key == "EBinOp":
            return EBinOp(
                op = from_data_to_class(value[0]),
                left = from_data_to_class(value[1]),
                right = from_data_to_class(value[2])
            )
        if key == "EUnaryOp":
            return EUnaryOp(
                op = from_data_to_class(value[0]),
                expr = from_data_to_class(value[1])
            )
        
        # Operations
        ops = {
            "OpAdd": OpAdd,
            "OpSub": OpSub,
            "OpMul": OpMul,
            "OpDiv": OpDiv,
            "OpMod": OpMod,
            "OpLessThan": OpLessThan,
            "OpEqual": OpEqual,
            "OpAnd": OpAnd,
            "OpOr": OpOr,
            "OpNot": OpNot
        }
        if key in ops:
            return ops[key]()
        
        # Constants
        consts = {
            "CInt": CInt,
            "CBool": CBool
        }
        if key in consts:
            return consts[key](value)
        
        # Types
        types = {
            "TyInt": TyInt,
            "TyBool": TyBool
        }
        if key in types:
            return types[key]()

def ast_to_while(ast, indent = 0):
    ret = ""

    if isinstance(ast, Prog):
        ret += f"{ast_to_while(ast.progDecls)}\n"
        ret += f"{ast_to_while(ast.progComms)}"
    elif isinstance(ast, CSkip):
        ret += " " * indent + "skip"
    elif isinstance(ast, CSeq):
        ret += f"{ast_to_while(ast.comm1, indent)}\n"
        ret += f"{ast_to_while(ast.comm2, indent)}"
    elif isinstance(ast, CAssign):
        ret += " " * indent + f"{ast.var_name} = {ast_to_while(ast.expr)};"
    elif isinstance(ast, CRead):
        ret += " " * indent + f"read ({ast.var_name});"
    elif isinstance(ast, CWrite):
        ret += " " * indent + f"write ({ast_to_while(ast.expr)});"
    elif isinstance(ast, CIf):
        ret += " " * indent + f"if ({ast_to_while(ast.expr)})\n"
        ret += " " * indent + f"then\n"
        ret += " " * indent + "{\n"
        ret += f"{ast_to_while(ast.comm1, indent+2)}\n"
        ret += " " * indent + "}\n"
        ret += " " * indent + f"else\n"
        ret += " " * indent + "{\n"
        ret += f"{ast_to_while(ast.comm2, indent+2)}"
        ret += " " * indent + "\n}"
    elif isinstance(ast, CWhile):
        ret += " " * indent + f"while ({ast_to_while(ast.expr)})\n"
        ret += " " * indent + "{\n"
        ret += f"{ast_to_while(ast.comm, indent+2)}"
        ret += " " * indent + "\n}"
    elif isinstance(ast, CAssert):
        ret += " " * indent + f"assert ({ast_to_while(ast.expr)});\n"
    
    # Expressions
    elif isinstance(ast, ECst):
        ret += f"{ast_to_while(ast.value)}"
    elif isinstance(ast, EVar):
        ret += str(ast.var_name)
    elif isinstance(ast, EBinOp):
        ret += f"{ast_to_while(ast.left)} {ast_to_while(ast.op)} {ast_to_while(ast.right)}"
    elif isinstance(ast, EUnaryOp):
        ret += f"{ast_to_while(ast.op)}({ast_to_while(ast.expr)})"

    # Operations
    elif isinstance(ast, OpAdd):
        ret += "+"
    elif isinstance(ast, OpSub):
        ret += "-"
    elif isinstance(ast, OpMul):
        ret += "*"
    elif isinstance(ast, OpDiv):
        ret += "/"
    elif isinstance(ast, OpMod):
        ret += "%"
    elif isinstance(ast, OpLessThan):
        ret += "<"
    elif isinstance(ast, OpEqual):
        ret += "=="
    elif isinstance(ast, OpAnd):
        ret += "&&"
    elif isinstance(ast, OpOr):
        ret += "||"
    elif isinstance(ast, OpNot):
        ret += "!"

    # Constants
    elif isinstance(ast, CInt):
        ret += str(ast.value)
    elif isinstance(ast, CBool):
        ret += str(ast.value)
    
    # Types
    elif isinstance(ast, TyInt):
        ret += "int"
    elif isinstance(ast, TyBool):
        ret += "bool"
    
    elif isinstance(ast, list):
        for element in ast:
            ret += f"{ast_to_while(element, indent)}\n"
    elif isinstance(ast, tuple):
        ret += f"{ast_to_while(ast[0], indent)} {ast_to_while(ast[1], indent)};"
    elif isinstance(ast, str):
        ret += ast

    return ret


json_path = "../json/HW02.json"

with open(json_path, 'r') as f:
    data = json.load(f)

ast = from_data_to_class(data)

print(str(ast))
print("-"*20)
print(ast_to_while(ast))