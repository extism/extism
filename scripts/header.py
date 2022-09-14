from pycparser import c_ast, parse_file

class Function:
    def __init__(self, name, return_type, args):
        self.name = name
        self.return_type = return_type
        self.args = args
        
        
typemap = {
    "_Bool": "bool",
    "ExtismPlugin": "int32_t",
    "ExtismSize": "uint64_t",
}
        
class Type:
    def __init__(self, name, const=False, pointer=False):
        self.name = typemap.get(name) or name
        self.const = const
        self.pointer = pointer
        
class Arg:
    def __init__(self, name, type):
        self.name = name
        self.type = type        

class Visitor(c_ast.NodeVisitor):
    def __init__(self, header):
        self.header = header
        
    def args(self, args):
        dest = []
        for arg in args:
            name = arg.name
            
            if isinstance(arg.type, c_ast.PtrDecl):
                t = arg.type.type
                is_ptr = True
            else:
                t = arg.type
                is_ptr = False

            type_name = t.type.names[0]
            const = hasattr(t.type, 'quals') and 'const' in t.type.quals
            t = Type(type_name, const=const, pointer=is_ptr)
            dest.append(Arg(name, t))
        return dest
        
    def visit_FuncDecl(self, node: c_ast.FuncDecl):
        if isinstance(node.type, c_ast.PtrDecl):
            t = node.type.type
            is_ptr = True
        else:
            t = node.type
            is_ptr = False

        name = t.declname
        args = self.args(node.args)
        return_type_name = t.type.names[0]
        const = hasattr(t.type, 'quals') and 'const' in t.type.quals
        return_type = Type(return_type_name, const=const, pointer=is_ptr)
        self.header.functions.append(Function(name, return_type, args))
        
class Header:
    def __init__(self, filename='runtime/extism.h'):
        self.functions = []
        self.header = parse_file(filename, use_cpp=True, cpp_args='-w')
        self.visitor = Visitor(self)
        self.visitor.visit(self.header)
        
    def __iter__(self):
        return self.functions.__iter__()
        
    def __getitem__(self, func):
        for f in self.functions:
            if f.name == func:
                return f
        return None
