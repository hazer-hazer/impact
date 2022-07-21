import 'source-map-support/register'
import { Recoverable, start } from 'repl'
import { Compiler } from './compiler'

const continueChars = ['=']

const compiler = new Compiler({
    // printSource: true,
    // printJS: true,
    // printAst: true,
    printPrelude: true
})

start({
    prompt: '> ',
    terminal: false,
    useColors: true,
    ignoreUndefined: true,

    eval: (input, _ctx, _filename, cb) => {
        try {
            const trimmed = input.trimEnd()
            if (!trimmed.length) {
                return cb(null, undefined)
            }

            if (continueChars.includes(trimmed[trimmed.length - 1])) {
                return cb(new Recoverable(new Error('continue')), undefined)
            }

            const args = trimmed.split(' ')
            const maybeCommand = args[0]

            if (maybeCommand?.startsWith(':')) {
                const command = maybeCommand.slice(1)

                if (!command) {
                    throw new Error('Expected command name after `:`')
                }

                return cb(null, compiler.runCommand(command, args.slice(1)))
            }

            return cb(null, compiler.run(input))
        } catch (e) {
            cb(e as Error, undefined)
        }
    },
})
