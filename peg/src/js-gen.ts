import { assert } from "console"

export type Options = {
    indentSize: number
}

const defaultOptions: Options = {
    indentSize: 4
}

export class JSGen {
    options: Options

    indentLevel: number = 0

    constructor(options: Options = defaultOptions) {
        this.options = { ...defaultOptions, ...options }
    }

    indent() {
        this.indentLevel++
        return this.indent_str()
    }

    dedent() {
        this.indentLevel--
        assert(this.indentLevel >= 0)
        return this.indent_str()
    }

    indent_str() {
        return ' '.repeat(this.options.indentSize).repeat(this.indentLevel)
    }

    indent_pred() {
        this.indent()
        return true
    }

    dedent_pred() {
        this.dedent()
        return true
    }

    letItem(name: string, params: string[], body: string): string {
        if (params.length) {
            return this.letFunc(name, params, body)
        } else {
            return this.letVar(name, body)
        }
    }

    letVar(name: string, body: string): string {
        return `
const ${name} = ${body};`.trim()
    }

    letFunc(name: string, params: string[], body: string): string {
        return `
function ${name}(${params.join(', ')}) {
${this.indent()}return ${body}
${this.dedent()}}`.trim()
    }

    infix(lhs: string, op: string, rhs: string): string {
        return `${lhs} ${op} ${rhs}`
    }

    block(stmts: string[]): string {
        let return_expr = stmts.pop();

        this.indent();
        return `
(function() {
${stmts.map(stmt => `${this.indent_str()}${stmt}`).join(';\n')};
${return_expr ? `\n${this.indent_str()}return ${return_expr}` : ''}
${this.dedent()}}())
`.trim()
    }

    call(lhs: string, arg: string): string {
        return `${lhs}(${arg})`
    }
}