const peggy = require('peggy')
const fs = require('fs')
const repl = require('repl')

const grammar = fs.readFileSync('./peggy-js.pegjs', 'utf-8')

const parser = peggy.generate(grammar, {
    allowedStartRules: ['program'],
})

const continueChars = ['=']


repl.start({
    prompt: '> ',
    eval: (input, ctx, filename, cb) => {
        try {
            let trimmed = input.trimEnd();
            if (continueChars.includes(trimmed[trimmed.length - 1])) {
                cb(new repl.Recoverable())
                return
            }

            const result = parser.parse(input);
            console.log(`Running code:\n${result}`);
            cb(null, eval(result))
        } catch (e) {
            if (typeof e.format === 'function') {
                console.log(e.format([
                    { source: grammar, text: input },
                ]))
                cb(null)
            } else {
                cb(e)
            }
        }
    }
})
