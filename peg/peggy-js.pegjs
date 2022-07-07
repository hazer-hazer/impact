{
    const gen = options.jsGen
    const ctx = options.parserCtx
}

program =
    _ stmts:(@stmt _)* {
        return stmts.join('\n')
    }

stmt 'statement' = SAMEINDENT (@item / @expr / @semi) EOL?

item 'item' =
	name:var_id params:(__ @var_id)* _ '=' _ &{return gen.indent_pred();} body:(block / expr) &{return gen.dedent_pred()} {
        return gen.letItem(name, params, body)
    }

expr 'expression' =
    ascription

ascription 'type ascription' =
	e:add _ ':' _ ty {return e}
    / add

add =
	lhs:mult _ op:[+-] _ rhs:add {
        return gen.infix(lhs, op, rhs)
    }
	/ mult

mult =
	lhs:call _ op:[\*\/] _ rhs:mult {
        return gen.infix(lhs, op, rhs)
    }
	/ call

call 'application' =
	lhs:primary __ arg:call { return gen.call(lhs, arg) }
	/ primary

primary 'primary expression' =
	int:$([0-9]+) {return int}
    / '\'' str:([^\n\r\'\\] / '\\' .)* '\'' {return `'${str.join('')}'`}
    / name:var_id {
    	return name
    }
    / 'let' _ body:block {
        return body
    }

block =
    INDENT &{return gen.indent_pred();} first:stmt stmts:(semi @stmt)* return_expr:(semi @expr)? &{return gen.dedent_pred()} DEDENT {
        return gen.block([first, ...stmts], return_expr)
    }

ty 'type' =
    simple_ty (_ '->' _ ty)*

simple_ty =
	ty_id
    / '(' _ ty _ ')'

var_id 'variable name' = $([a-z][A-z0-9]*)
ty_id 'type name' = $([A-Z][A-z0-9]*)

semi 'semi' = [;\n]+

_ = [ \t]*
__ "whitespace" = [ \t]+
NL "new-line" = (_ EOL)+
EOL "end of line" = '\r\n' / '\n' / '\r'


// For the same indent we allow no whitespaces as it may be top-level indentation
SAMEINDENT = spaces:$(_) &{
    return ctx.isSameIndent(spaces)
}

INDENT = &(
    spaces:$(__) &{
        return ctx.isIndent()
    } {
        ctx.pushIndent(spaces)
    }
)

DEDENT = &{
    return ctx.popIndent()
} {}
