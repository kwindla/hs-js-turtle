
exports.runProgramSVG    = runProgramSVG
exports.runProgramValues = runProgramValues
exports.runProgram       = runProgram


var tp = require ('./TurtlePrimitives')
var tokenizer = require ('./Tokenizer')
var parser = require ('./Parser')
var evaluator = require ('./Evaluator')


function runProgram (str) {
  var tokens, exprl, symTab, turtle, svg

  tokens = tokenizer.tokenize (str)
  exprl = parser.parse (tokens, tp.InitialSymbolTable ())

  // console.log (exprl); console.log ("----")

  symTab = tp.InitialSymbolTable ()
  turtle = tp.Turtle ()
  svg = []

  resultState = evaluator.evaluate (exprl, symTab, { turtle: turtle, svg: svg })
  return resultState
}

function runProgramSVG (str) {
  var resultState = runProgram (str)
  finalSVG = '<svg width="400" height="400">' +
    '<g transform="translate(0,400)">' +
    '<g transform="scale(1,-1)">' +
    (resultState.svg.join("\n")) + 
    "\n" + '</g></g></svg>'
  return finalSVG
}

function runProgramValues (str) {
  var resultState = runProgram (str)
  return resultState.values
}

