{
    let ind = 0;
    function indent() {
        ind++;
        return cur_ind();
    }

    function cur_ind() {
        return '    '.repeat(ind);
    }

    function dedent() {
        ind--;
        return cur_ind();
    }

    function block(els, delim) {
        return `(function() {\n${indent()}${els.map(el => `${cur_ind()}${el}`).join(delim)}${dedent()}})`
    }
}

program =
    _ items:(@item _)* {
        return items.join('\n')
    }

stmt 'statement' =
    item
    / expr
    / semi

item 'item' =
	name:var_id params:(__ @var_id)* _ '=' _ body:(block / expr) {
    	return `function ${name}(${params.join(', ')}) {\n${indent()}return ${body};\n${dedent()}}`
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
	int:[0-9]+ {return int}
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
    / '{' _ first:stmt stmts:(semi @stmt)* return_expr:(semi @expr)? _ '}' {
        return `(function() {\n${indent()}${[first, ...stmts].map(stmt => `${cur_ind()}${stmt}`).join(';\n')};\n${return_expr ? `return ${return_expr};` : ''}${dedent()}\n}())`
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
