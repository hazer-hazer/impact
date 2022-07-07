{
    let ind = 0;
    function indent() {
        ind++;
        return cur_ind();
    }

    function indent_pred() {
        indent();
        return true;
    }

    function dedent_pred() {
        dedent();
        return true;
    }

    function cur_ind() {
        return '    '.repeat(ind);
    }

    function dedent() {
        ind--;
        return cur_ind();
    }

    // function block(els, delim) {
    //     return `(function() {\n${cur_ind()}${els.map(el => `${cur_ind()}${el}`).join(delim)}${dedent()}})`
    // }
}

program =
    _ stmts:(@stmt _)* {
        return stmts.join('\n')
    }

stmt 'statement' =
    item
    / expr
    / semi

item 'item' =
	name:var_id params:(__ @var_id)* _ '=' _ &{return indent_pred();} body:(block / expr) &{return dedent_pred();} {
        if (params.length) {
    	    return `function ${name}(${params.join(', ')}) {\n${indent()}return ${body};\n${dedent()}}`
        } else {
            return `const ${name} = ${body}`
        }
    }

expr 'expression' =
    ascription

ascription 'type ascription' =
	e:add _ ':' _ ty {return e;}
    / add

add =
	lhs:mult _ op:[+-] _ rhs:add {
    	switch (op) {
        case '+': return lhs + rhs;
        case '-': return lhs - rhs;
        }
    }
	/ mult

mult =
	lhs:call _ op:[\*\/] _ rhs:mult {
    	switch (op) {
        case '*': return lhs * rhs;
        case '/': return lhs / rhs;
        }
    }
	/ call

call 'application' =
	lhs:primary __ rhs:call { return lhs(rhs); }
	/ primary

primary 'primary expression' =
	int:$([0-9]+) {return int}
    / '\'' str:.* '\'' {return `'${str}'`;}
    / name:var_id {
    	return name;
    }
    / 'let' _ body:block {
        return body;
    }

block =
    '{' _ '}' {
        return 'undefined';
    }
    / '{' _ &{return indent_pred();} first:stmt stmts:(semi @stmt)* return_expr:(semi @expr)? _ &{return dedent_pred();} '}' {
        indent();
        return `(function() {\n${
            [first, ...stmts].map(stmt => `${cur_ind()}${stmt}`).join(';\n')
        };${
            return_expr ? `\n${cur_ind()}return ${return_expr};` : ''
        }\n${
            dedent()
        }}())`
    }

ty 'type' =
    simple_ty ('->' ty)*

simple_ty =
	ty_id
    / '(' _ ty _ ')'

var_id 'variable name' = $([a-z][A-z0-9]*)
ty_id 'type name' = $([A-Z][A-z0-9]*)

semi 'semi' = [;\n]+

_ = [ \t\n\r]*
__ 'white-space' = [ \t]+
