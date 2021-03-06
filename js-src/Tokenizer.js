
exports.tokenize = tokenize


var TokenCases = [
  // isSpace case is handled by a trim down in the driver function
  [ /^[a-zA-Z\^\%\'\"]/, function (m) { return TokenSymbol (m[0]) } ],
  [ ',', TokenComma ],
  [ '=', TokenEquals ],
  [ '(', TokenLeftParen ],
  [ ')', TokenRightParen ],
  [ '{', TokenLeftBrace ],
  [ '}', TokenRightBrace ],
  [ '+', TokenOperator, 'Plus' ],
  [ '-', TokenOperator, 'Minus' ],
  [ '*', TokenOperator, 'Times' ],
  [ '/', TokenOperator, 'Div' ],
  [ '\\', TokenOperator, 'Mod' ],
  [ '~', TokenOperator, 'Equals' ],
  [ '!', TokenOperator, 'Not' ],
  [ '>', TokenOperator, 'GreaterThan' ],
  [ '<', TokenOperator, 'LessThan' ],
  [ '&', TokenDefun ],
  [ '?', TokenIf ],
  [ '#', TokenRepeat ],
  [ /^[0-9]+(\.[0-9]+)?/,   function (m) { return TokenNumber (+m[0]) } ]
]


function tokenize (stream) {
  var r = {}
  stream = stream.replace (/^\s+/,'')
  if (! stream) {
    return []
  }
  r.stream = stream
  if (! TokenCases.some (tryCase, r)) {
    throw "could not tokenize: " + stream
  }

  
  return [r.result.token].concat(tokenize(r.result.stream))
}


function tryCase (tcase) {
  if (r = tryLiteral(this.stream,tcase) || tryRegExp(this.stream,tcase)) {
    this.result = r
    return true
  }
  return false
}

function tryLiteral (stream, tcase) {
  var cas = tcase[0]
  if (stream.indexOf(cas) == 0) {  // duck type, here (test only
                                   // succeeds if cas is a string)
    return { token: tcase[1](tcase.slice(2)),
             stream: stream.substr(cas.length) }
  }
  return false;
}

function tryRegExp (stream, tcase) {
  var cas = tcase[0]
  var m
  if (cas.test && (m=stream.match(cas))) {
    var matched_chars = m[0]
    var token = tcase[1](m)
    return { token: token, stream: stream.substr(matched_chars.length) }
  }
  return false;
}

//

var Token = {
  type:    null,
  is:      function (s) { return s === this.type },
  inspect: function () { return (this.type + (this.v ? (' ' + this.v) : '')) }
}

function TokenSymbol (c) {
  return Object.create (Token, { type: {value: 'TokenSymbol'},
                                 v:    {value: c },
                                 inspect: { value:
    function () { 
      return (this.type + (this.v ? (" '" + this.v + "'") : ''))
    }}})
}

function TokenNumber (num) {
  return Object.create (Token, { type: {value: 'TokenNumber'},
                                 v:    {value: num } })
}

function TokenComma (c) {
  return Object.create (Token, { type: {value: 'TokenComma'} })
}

function TokenEquals (c) {
  return Object.create (Token, { type: {value: 'TokenEquals'} })
}

function TokenLeftParen () {
  return Object.create (Token, { type: {value: 'TokenLeftParen'} })
}

function TokenRightParen () {
  return Object.create (Token, { type: {value: 'TokenRightParen'} })
}

function TokenLeftBrace () {
  return Object.create (Token, { type: {value: 'TokenLeftBrace'} })
}

function TokenRightBrace () {
  return Object.create (Token, { type: {value: 'TokenRightBrace'} })
}

function TokenOperator (op) {
  return Object.create (Token, { type: {value: 'TokenOperator'},
                                 v: {value: op} })
}

function TokenDefun (c) {
  return Object.create (Token, { type: {value: 'TokenDefun'} })
}

function TokenIf (c) {
  return Object.create (Token, { type: {value: 'TokenIf'} })
}

function TokenRepeat (c) {
  return Object.create (Token, { type: {value: 'TokenRepeat'} })
}
 

