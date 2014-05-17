
exports.startProgramRun      = startProgramRun
exports.runProgram           = runProgram
exports.runProgramSVGElement = runProgramSVGElement
exports.runProgramValues     = runProgramValues

var tp = require ('./TurtlePrimitives')
var tokenizer = require ('./Tokenizer')
var parser = require ('./Parser')
var evaluator = require ('./Evaluator')


function startProgramRun (str, yieldTest) {
  var tokens, exprl, symTab, turtle, color, svg
  tokens = tokenizer.tokenize (str)
  exprl = parser.parse (tokens, tp.InitialSymbolTable ())
  symTab = tp.InitialSymbolTable ()
  turtle = tp.Turtle ()
  svg = []
  pgmState = evaluator.evaluate (exprl,
                                 symTab, { turtle: turtle, svg: svg },
                                 yieldTest
                                )
  pgmState.SVGBody = function () { return SVGBodyFromProgramState (this) }
  pgmState.SVGElement = function () { return SVGElementFromProgramState (this) }
  return pgmState
}

function runProgram (str) {
  pgmState = startProgramRun (str)
  while (! pgmState.done) {
    pgmState.continue ()
  }
  return pgmState
}

function runProgramValues (str) {
  return runProgram(str).values()
}

function runProgramSVGElement (str) {
  return runProgram(str).SVGElement()
}

function SVGBodyFromProgramState (s) {
  return '<g transform="translate(0,100)">' +
         '<g transform="scale(1,-1)">' +
         (s.svg.join("\n")) + 
         "\n" + '</g></g>';
}

function SVGElementFromProgramState (s, width, height) {
  w = width || 100; h = height || 100
  return '<svg width="' + w + '" height="' + h + 
            '" viewbox="0 0 100 100' + '">' +
         SVGBodyFromProgramState (s) +
         '</svg>'
}
