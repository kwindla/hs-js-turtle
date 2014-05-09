

exports.runProgramSVGElement = runProgramSVGElement
exports.runProgramSVGBody    = runProgramSVGBody
exports.runProgramValues     = runProgramValues
exports.runProgram           = runProgram


var tp = require ('./TurtlePrimitives')
var tokenizer = require ('./Tokenizer')
var parser = require ('./Parser')
var evaluator = require ('./Evaluator')


function runProgram (str) {
  var tokens, exprl, symTab, turtle, color, svg

  tokens = tokenizer.tokenize (str)
  exprl = parser.parse (tokens, tp.InitialSymbolTable ())

  // console.log (exprl); console.log ("----")

  symTab = tp.InitialSymbolTable ()
  turtle = tp.Turtle ()
  svg = []

  resultState = evaluator.evaluate (exprl, symTab, { turtle: turtle, svg: svg })
  return resultState
}

function runProgramSVGElement (str) {
  return '<svg width="400" height="400">' +
           runProgramSVGBody (str) +
         '</svg>'
}

function runProgramSVGBody (str) {
  var resultState = runProgram (str)
  finalSVG = 
    '<g transform="translate(0,320)">' +
    '<g transform="scale(1,-1)">' +
    (resultState.svg.join("\n")) + 
    "\n" +
    '</g></g>';
  return finalSVG
}

function runProgramValues (str) {
  var resultState = runProgram (str)
  return resultState.values
}

