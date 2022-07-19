{
    const ctx = options.parserCtx.reset()
}

line = NL? stmt:stmt {
    return {stmt}
}

stmt 'statement' = SAMEINDENT node:(
    item:item {return {tag: 'Item', item}}
    / expr:expr {return {tag: 'Expr', expr}}
    / semi {return null}
) NL? {
    return node;
}

item 'item' =
    'type' _ name:ty_id _ '=' _ ty:ty {
        return {
            tag: 'Ty',
            name,
            ty,
        }
    }
	/ name:var_id params:(__ @var_id)* _ '=' _ body:body {
        return {
            tag: 'Term',
            name,
            params,
            body,
        }
    }

expr 'expression' =
    param:var_id _ '->' _ body:expr {
        return {
            tag: 'Abs',
            param,
            body,
        }
    }
    / ascription

ascription 'type ascription' =
	expr:add _ ':' _ ty:ty {
        return {
            tag: 'Anno',
            expr,
            ty,
        }
    }
    / add

add =
	lhs:mult _ op:[+-] _ rhs:add {
        return ctx.makeInfix(lhs, op, rhs)
    }
	/ mult

mult =
	lhs:call _ op:[\*\/] _ rhs:mult {
        return ctx.makeInfix(lhs, op, rhs)
    }
	/ call

call 'application' =
	lhs:primary __ arg:call {
        return {
            tag: 'App',
            lhs,
            arg,
        }
    }
	/ primary

primary 'primary expression' =
    'if' 
	/ int:$([0-9]+) {
        return {
            tag: 'Lit',
            kind: {
                tag: 'Int',
                val: parseInt(int),
            },
        }
    }
    / val:$('true' / 'false') {
        return {
            tag: 'Lit',
            kind: {
                tag: 'Bool',
                val: val === 'true',
            },
        }
    }
    / '\'' str:([^\n\r\'\\] / '\\' .)* '\'' {
        return {
            tag: 'Lit',
            kind: {
                tag: 'String',
                val: str,
            }
        }
    }
    / name:(@var_id / @op_id) {
    	return {
            tag: 'Var',
            name,
        }
    }
    / 'let' body:(NL @block) {
        return {
            tag: 'Let',
            body,
        }
    }
    / '(' _ @expr _ ')'

body =
    expr
    / NL block:block {
        return {
            tag: 'Block',
            block,
        }
    }

block =
    INDENT first:stmt stmts:(semi @stmt)* DEDENT {
        stmts = [first, ...stmts]
        if (stmts[stmts.length - 1].tag !== 'Expr') {
            throw new Error('Last element in block must be an expression')
        }
        const expr = stmts.pop()
        return {
            stmts,
            expr: expr.expr,
        }
    }

ty 'type' =
    'forall' _ alpha:var_id _ '.' _ ty:ty {
        return {
            tag: 'Forall',
            alpha,
            ty,
        }
    }
    / param:simple_ty ret:(_ '->' _ @ty)* {
        if (!ret.length) {
            return param
        }

        return [param, ...ret].reduce((ret, ty) => ({
            tag: 'Func',
            param: ret,
            ret: ty,
        }))
    }

simple_ty =
    '(' _ ')' {
        return {
            tag: 'Lit',
            kind: {
                tag: 'Unit'
            }
        }
    }
    / tag:('Int' / 'String' / 'Bool') {
        return {
            tag: 'Lit',
            kind: {
                tag,
            }
        }
    }
	/ name:ty_id {
        return {
            tag: 'ConId',
            name,
        }
    }
    / name:var_id {
        return {
            tag: 'Var',
            name,
        }
    }
    / '(' _ ty:ty _ ')' {
        return ty
    }

var_id 'variable name' = $([_]*[a-z][_A-z0-9]*)
op_id = '(' @$([\+\-\*\/\^%&$\|]+) ')'
ty_id 'type name' = $([_]*[A-Z][_A-z0-9]*)

semi 'semi' = (EOL / ';')+

_ = [ \t]*
__ "whitespace" = [ \t]+
NL "new-line" = (_ EOL)+
EOL "end of line" = '\r\n' / '\n' / '\r'


// For the same indent we allow no whitespaces as it may be top-level indentation
SAMEINDENT = spaces:$(_) &{
    return ctx.isSameIndent(spaces)
} {}

INDENT = &(
    spaces:$(__) &{
        return ctx.isIndent(spaces)
    } {
        ctx.pushIndent(spaces)
    }
) {}

DEDENT = &{
    return ctx.popIndent()
} {}
