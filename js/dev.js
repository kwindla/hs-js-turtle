
var tokenizer = require ('./Tokenizer')
var parser = require ('./Parser')
var evaluator = require ('./Evaluator')

console.log (process.argv[2])

tokens = tokenizer.tokenize (process.argv[2])
console.log (tokens)

exprl = parser.parse (tokens)
console.log (exprl)

results = evaluator.runProgram (exprl)
console.log (results)
