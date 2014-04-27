
var tp = require ('./TurtlePrimitives')
var tokenizer = require ('./Tokenizer')
var parser = require ('./Parser')
var evaluator = require ('./Evaluator')


runProgram (process.argv[2])


function runProgram (str) {
  var tokens,
      exprl,
      values

  tokens = tokenizer.tokenize (str)
  exprl = parser.parse (tokens, tp.InitialSymbolTable ())
  console.log (exprl); console.log ("----")
  results = evaluator.evaluate (exprl, tp.InitialSymbolTable ())
  console.log (results)
}



