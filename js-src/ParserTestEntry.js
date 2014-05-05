
exports._parse = _parse

var tp = require ('./TurtlePrimitives')
var tokenizer = require ('./Tokenizer')
var parser = require ('./Parser')


function _parse (str) {
  var tokens, exprl

  tokens = tokenizer.tokenize (str)
  exprl = parser.parse (tokens, tp.InitialSymbolTable ())

  // console.log (exprl); console.log ("----")

  return exprl
}
