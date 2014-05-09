(function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);throw new Error("Cannot find module '"+o+"'")}var f=n[o]={exports:{}};t[o][0].call(f.exports,function(e){var n=t[o][1][e];return s(n?n:e)},f,f.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s})({1:[function(require,module,exports){

var _st = require ('./SymbolTable')

exports.evaluate = function (exprl, symbolTable, extraFields) {
  var state = EvalState()
  state.symTab = symbolTable
  Object.keys(extraFields).forEach (
    function (k) { state[k] = extraFields[k] }
  )

  exprl.forEach (function (expr) { var r = state.evaluate (expr, symbolTable)
                                   state.values.push (r) 
                                 }
                )

  return state
}


function EvalState () {
  return {
    values:   [],        // output values from top-level expression list
    evaluate: _evaluate
  }
}


function _evaluate (expr) {
  var f = EvalTable[expr.type]
  if (! f) { throw 'unkown expression type - ' + expr.type }
  return f.apply(this,[expr])
}


var EvalTable = {
  Assignment: function (e) {
    var value = this.evaluate (e.e)
    this.symTab.updateBindingForValue (e.v, value)
    return value
  },
  Symbol: function (e) {
    var binding = this.symTab.retrBinding (e.v)
    return binding.v
  },
  Defun: function (e) {
    // here we can do things differently than on the haskell side
    // because we have references and mutable objects. we'll save our
    // current symbol table ref to use when we're funcall'ed, and
    // insert a new derived table to protect this saved symbol table
    // from getting new entries otherwise
    this.symTab.updateBindingForDefun (e.v, e.arity, e.e, this.symTab)
    this.symTab = this.symTab.derivedTable ()
    return e.arity
  },
  Funcall: function (e) {
    var  callTimeSymTab = this.symTab,
         funcEvalSymTab,
         args,
         retval
    binding = this.symTab.retrBinding (e.v)
    if (binding.type == 'BoundBuiltin') {
      return binding.f.apply (this, [e.exprl])
    }
    if (binding.type == 'BoundDefun') {
      // plan of action: derive a symbol table from our saved
      // defun-scope symbol table (which, remember, is just a parent
      // of the calling scope's table), make entries in that new table
      // for the function's arguments, swap in that symbol table, eval
      // the function body, put back the symbol table from the calling
      // scope, and return
      funcEvalSymTab = binding.symTab.derivedTable ()
      for (var i=0; i<binding.arity; i++) {
        funcEvalSymTab["abcdefghi".charAt(i)] =
          _st.BoundValue (this.evaluate (e.exprl[i]))
      }
      this.symTab = funcEvalSymTab
      retval = this.evaluate (binding.e)
      this.symTab = callTimeSymTab
      return retval
    }
  },
  BinaryOp: function (e) {
    var left  = this.evaluate (e.left)
    var right = this.evaluate (e.right)
    return e.f (left, right)
  },
  UnaryOp: function (e) {
    return e.f (this.evaluate(e.e))
  },
  ConstantNumber: function (e) {
    return e.v
  },
  TernaryIf: function (e) {
    var cond = this.evaluate (e.econd)
    if (cond != 0) {
      return this.evaluate (e.eif)
    } else {
      return this.evaluate (e.ethen)
    }
  },
  ExprTreeListNode: function (e) {
    var outerSymTab = this.symTab,
        result
    this.symTab = this.symTab.derivedTable ()
    for (var i=0; i<e.exprl.length; i++) {
      result = this.evaluate (e.exprl[i])
    }
    this.symTab = outerSymTab
    return result
  },
  Repeat: function (e) {
    // need to handle expression list blocks differently from bare
    // expressions, here for a block, we want to repeat multiple times
    // with a stateful symbol table across all repeats, then "pop" that
    // symbol table and throw it away.
    var ntimes = this.evaluate (e.ntimes),
        outerSymTab,
        exprl,
        result
    if (e.e.is ('ExprTreeListNode')) {
      outerSymTab = this.symTab
      this.symTab = this.symTab.derivedTable ()
      exprl = e.e.exprl
      for (var i=0; i<ntimes; i++) {
        for (var j=0; j<exprl.length; j++) {
          result = this.evaluate (exprl[j])
        }
      }
      this.symTab = outerSymTab
    } else {
      for (var i=0; i<ntimes; i++) {
        result = this.evaluate (e.e)
      }
    }
    return result;
  }

}

//

function dbg (s) { console.log(s) }




},{"./SymbolTable":3}],2:[function(require,module,exports){

exports.parse = parse


var tp = require('./TurtlePrimitives')


var ParseState = {
  tokens: undefined,    // these will be set by arguments to parse ()
  symTab: undefined,
  expressionList:       _expressionList,
  expression:           _expression,
  term:                 _term,
  expressionTail:       _expressionTail,
  comparison:           _comparison,
  factor:               _factor,
  factorExpressionList: _factorExpressionList,
  termTail:             _termTail
}


function parse (tokens, symbolTable) {
  var state = Object.create (ParseState,
                             { tokens: {value: tokens},
                               symTab: {value: symbolTable} })
  return _expressionList.apply (state)
}

function dbg (s) { console.log(s) }


function _expressionList () {
  if (this.tokens.length == 0) { return [] }
  return [this.expression()].concat (this.expressionList())
}

function _expression () {
  var t0 = this.tokens[0],
      t1 = this.tokens[1],
      t2 = this.tokens[2];
  if (t0 && t1 && t0.is('TokenSymbol') && t1.is('TokenEquals')) {
    this.tokens.shift(); this.tokens.shift()
    return Assignment (t0.v, this.expression())
  } else if (t0 && t0.is('TokenDefun')) {
    if (! (t1 && t1.is('TokenSymbol'))) {
      throw ("defun token '&' should be followed by a symbol")
    }
    if (! (t2 && t2.is('TokenNumber'))) {
      throw ("defun token '&' should be followed by a symbol and a number")
    }
    this.tokens.shift(); this.tokens.shift(); this.tokens.shift()
    this.symTab.updateBindingForDefun (t1.v, +t2.v)
    return Defun (t1.v, +t2.v, this.expression())
  } else {
    return this.comparison( this.expressionTail (this.term()))
  }
  // fix: should there be something here? an error or explicit return
}

function _term () {
  return this.termTail (this.factor())
}

function _expressionTail (expr) {
  var t0 = this.tokens[0]
  if (t0 && t0.is('TokenOperator')) {
    if (t0.v == 'Plus') {
      this.tokens.shift()
      return this.expressionTail (BinaryOp ('+', function(a,b){return a+b},
                                            expr, this.term()) )
    } else if (t0.v == 'Minus') {
      this.tokens.shift()
      return this.expressionTail (BinaryOp ('-', function(a,b){return a-b},
                                            expr, this.term()) )
    }
  } 
  return expr
}

function _comparison (expr) {
  var t0 = this.tokens[0]
  if (t0 && t0.is('TokenOperator')) {
    if (t0.v == 'GreaterThan') {
      this.tokens.shift()
      return this.expressionTail (BinaryOp ('>', function(a,b){return a>b},
                                            expr, this.term()) )
    } else if (t0.v == 'LessThan') {
      this.tokens.shift()
      return this.expressionTail (BinaryOp ('<', function(a,b){return a<b},
                                            expr, this.term()) )
    } else if (t0.v == 'Equals') {
      this.tokens.shift()
      return this.expressionTail (BinaryOp ('=', function(a,b){return a===b},
                                            expr, this.term()) )
    }

  } 
  return expr
}

function _factor () {
  var t0 = this.tokens.shift()
  var t1, expr, exprl, binding
  var exprl = []
  if (t0.is('TokenNumber')) { return ConstantNumber (t0.v) }
  if (t0.is('TokenSymbol')) {
    binding = this.symTab.retrBinding (t0.v)
    if (binding.type == 'BoundValue') {
      return Symbol (t0.v)
    } else if ((binding.type=='BoundBuiltin') ||
               (binding.type=='BoundDefun')) {
      for (var i=0; i<binding.arity; i++) { exprl.push (this.expression()) }
      return Funcall (binding.arity, t0.v, exprl)
    } else { throw 'unknown binding type - ' + binding.type }
  }
  if (t0.is('TokenLeftParen')) {
    expr = this.expression ()
    t1 = this.tokens.shift()
    if (! t1.is('TokenRightParen')) {
      throw ("expected right paren, not - " + t1.inspect())
    }
    return expr
  }
  if (t0.is('TokenLeftBrace')) {
    var outerSymTab = this.symTab
    this.symTab = this.symTab.derivedTable ()
    exprl = this.factorExpressionList()
    this.symTab = outerSymTab
    return ExprTreeListNode (exprl);
  }
  if (t0.is('TokenIf')) {
    return TernaryIf (this.expression(), this.expression(), this.expression());
  }
  if (t0.is('TokenRepeat')) {
    return Repeat (this.expression(), this.expression())
  }
  if (t0.is('TokenOperator') && (t0.v == 'Plus')) { return this.expression() }
  if (t0.is('TokenOperator') && (t0.v == 'Minus')) {
    return UnaryOp ('-', function(v) { return -(v) }, this.expression())
  }
  throw ("incomplete pattern match in factor :) - " + t0.inspect())
}

function _factorExpressionList () {
  if (! this.tokens.length) {
    throw "unexpected end of token stream inside {braces}"
  }
  if (this.tokens[0].is('TokenRightBrace')) {
    this.tokens.shift()
    return []
  }
  return [this.expression()].concat (this.factorExpressionList())
}

function _termTail (expr) {
  var t0 = this.tokens[0]
  if (t0 && t0.is('TokenOperator')) {
    if (t0.v == 'Times') {
      this.tokens.shift()
      return this.expressionTail (BinaryOp ('*', function(a,b){return a*b},
                                            expr, this.term()) )
    } else if (t0.v == 'Div') {
      this.tokens.shift()
      return this.expressionTail (BinaryOp ('/', function(a,b){return a/b},
                                            expr, this.term()) )
    }
  } 
  return expr
}


//


var ExprNode = {
  type: null,
  is:      function (s) { return s === this.type },
  inspect: function () { return (this.type + (this.v ? (' ' + this.v) : '')) }
}


function ConstantNumber (n) {
  return Object.create (ExprNode, { type: {value: 'ConstantNumber'},
                                    v: {value: n} } )
}

function Symbol (c) {
  return Object.create (ExprNode, { type: {value: 'Symbol'},
                                    v: {value: c},
                                    inspect: { value:
    function () { 
      return (this.type + (this.v ? (" '" + this.v + "'") : ''))
    }}})
}

function Defun (c, arity, expr) {
  return Object.create (ExprNode, { type: {value: 'Defun'},
                                    v: {value: c},
                                    arity: {value: arity},
                                    e: {value: expr},
                                    inspect: { value:
    function () {
      return (this.type + " '" + this.v + "' " + this.arity +
              ' (' + this.e.inspect() + ')')
    } } })
}

function Funcall (arity, sym, exprl) {
  return Object.create (ExprNode, { type: {value: 'Funcall'},
                                    v: {value: sym}, 
                                    exprl: {value: exprl}, // args list
                                    inspect: {value:
    function () {
      return (this.type + ' ' + arity + " '" + this.v + "' " + 
                '['  +
              this.exprl.map ( function(e){return e.inspect()} ).join(', ') +
                ']')
    } } })
}

function Assignment (sym, expr) {
  return Object.create (ExprNode, { type: {value: 'Assignment'},
                                    v: {value: sym},
                                    e: {value: expr},
                                    inspect: {value:
    function () {
      return (this.type + " '" + this.v + "' (" + this.e.inspect() + ')')
    } } })
}

function BinaryOp (s, f, left, right) {
  return Object.create (ExprNode, { type: {value: 'BinaryOp'},
                                    v: {value: s},
                                    f: {value: f},
                                    left: {value: left},
                                    right: {value: right},
                                    inspect: { value:
    function () {
      return (this.type + ' (' + this.v + ') (' + this.left.inspect() + ')' +
                                           ' (' + this.right.inspect() + ')' )
    } } })
}

function UnaryOp (s, f, expr) {
  return Object.create (ExprNode, { type: {value: 'UnaryOp'},
                                    v: {value: s},
                                    f: {value: f},
                                    e: {value: expr},
                                    inspect: { value:
    function () {
      return (this.type + ' (' + this.v + ') (' + this.e.inspect() + ')')
    } } })
}

function TernaryIf (econd, eif, ethen) {
  return Object.create (ExprNode, { type:    {value: 'TernaryIf'},
                                    econd:   {value: econd},
                                    eif:     {value: eif},
                                    ethen:   {value: ethen},
                                    inspect: { value:
    function () {
      return (this.type + ' (' + this.econd.inspect() + ') ' +
              '(' + this.eif.inspect() + ') ' +
              '(' + this.ethen.inspect() + ')' )
    } } })
}

function ExprTreeListNode (exprl) {
  return Object.create (ExprNode, { type: {value: 'ExprTreeListNode'},
                                    exprl: {value: exprl},
                                    inspect: { value:
    function () {
      return (this.type + ' ' + ' [' +
              this.exprl.map ( function(e){return e.inspect()} ).join(', ') + 
              ']')
    } } })
}

function Repeat (ntimes, expr) {
  return Object.create (ExprNode, { type: {value: 'Repeat'},
                                    ntimes: {value: ntimes},
                                    e: {value: expr},
                                    inspect: {value:
    function () {
      return (this.type + ' (' + this.ntimes.inspect() + ') ' +
              '(' + this.e.inspect() + ')')
    } } })
}

},{"./TurtlePrimitives":5}],3:[function(require,module,exports){

exports.BaseSymbolTable = BaseSymbolTable
exports.BoundValue = BoundValue
exports.BoundBuiltin = BoundBuiltin

var Binding = {
  inspect: function () { return this.type + ' ' + this.v }
}

var SymbolTable = {
  parent: null,

  retrBinding:           _retrBinding,
  updateBindingForValue: _updateBindingForValue,
  updateBindingForDefun: _updateBindingForDefun,

  derivedTable: _derivedTable,
}

function BoundValue (v) {
  return Object.create (Binding, { type: {value: 'BoundValue'},
                                   v:    {value: v} })
}

function BoundBuiltin (arity, f) {
  return Object.create (Binding,  { type:  {value: 'BoundBuiltin'},
                                    arity: {value: arity},
                                    f:     {value: f},
                                    v:     {value: arity} // for inspect
                                   })
}

function BoundDefun (arity, expr, symtab) {
  return Object.create (Binding, { type:   {value: 'BoundDefun'},
                                   arity:  {value: arity},
                                   e:      {value: expr},
                                   symTab: {value: symtab},
                                   v:      {value: arity} // for inspect
                                 })
}

function BaseSymbolTable () {
  return Object.create (SymbolTable)
}

function _retrBinding (sym) {
  // we'll use the js inheritance chain to do our lookups. easy-peasy.
  return this[sym] || BoundValue (0)
}

function _updateBindingForValue (sym, v) {
  _setBinding (this, sym, BoundValue (v))
  return v
}

function _updateBindingForDefun (sym, arity, expr, symtab) {
  // dbg ('defun st ' + sym + ' - ' + arity)
  _setBinding (this, sym, BoundDefun (arity, expr, symtab))
  return arity   // FIX: is this return value correct (and do we use it)?
}

var util = require('util')

function _setBinding (symTab, sym, binding) {
  if (! lookUpward (symTab)) {
    // dbg ('not found - setting in leaf table')
    symTab[sym] = binding
  }
  return

  function lookUpward (symTab) {
    // dbg ('lU: ' + util.inspect(symTab))
    if (!symTab) { return false }
    if (symTab.hasOwnProperty(sym)) {
      // dbg ('    found ' + sym + ' ' + util.inspect(symTab[sym]))
      symTab[sym] = binding
      return true
    } else {
      // dbg ('    recursing')
      return lookUpward (symTab.parent)
    }
  }
}

function _derivedTable () {
  var newST = Object.create (this)
  newST.parent = this
  return newST
}



function dbg (s) { console.log(s) }

},{"util":11}],4:[function(require,module,exports){

exports.tokenize = tokenize


var TokenCases = [
  // isSpace case is handled by a trim down in the driver function
  [ /^[a-zA-Z\^\%]/, function (m) { return TokenSymbol (m[0]) } ],  
  [ '=', TokenEquals ],
  [ '(', TokenLeftParen ],
  [ ')', TokenRightParen ],
  [ '{', TokenLeftBrace ],
  [ '}', TokenRightBrace ],
  [ '+', TokenOperator, 'Plus' ],
  [ '-', TokenOperator, 'Minus' ],
  [ '*', TokenOperator, 'Times' ],
  [ '/', TokenOperator, 'Div' ],
  [ '~', TokenOperator, 'Equals' ],
  [ '>', TokenOperator, 'GreaterThan' ],
  [ '<', TokenOperator, 'LessThan' ],
  [ '&', TokenDefun ],
  [ '?', TokenIf ],
  [ '#', TokenRepeat ],
  [ /^[0-9]+/,   function (m) { return TokenNumber (+m[0]) } ]
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
 


},{}],5:[function(require,module,exports){

exports.InitialSymbolTable = InitialSymbolTable
exports.Turtle = Turtle


var st = require ('./SymbolTable')


var TurtleGlobals = {
  // for functions defined here, "this" will be the current
  // EvalState. e is the passed-in exprl of args to the function
    P: st.BoundValue (Math.PI)
  , F: st.BoundBuiltin (1, function (e) { return _Forward (e, this) })
  , R: st.BoundBuiltin (1, function (e) { return _RotateRight (e, this) })
  , L: st.BoundBuiltin (1, function (e) { return _RotateLeft (e, this) })

  , B: st.BoundBuiltin (2, function (e) { return _MoveBy (e, this) })
  , T: st.BoundBuiltin (1, function (e) { return _TurnTo (e, this) })
  , M: st.BoundBuiltin (1, function (e) { return _MoveTo (e, this) })

  , A: st.BoundBuiltin (1, function (e) { return _Alpha (e, this) })
  , C: st.BoundBuiltin (3, function (e) { return _Color (e, this) })

  , '^': st.BoundBuiltin (1, function (e) { return _PenToggle (e, this) })
  , U: st.BoundBuiltin (1, function (e) { return _PenUp (e, this) })
  , N: st.BoundBuiltin (1, function (e) { return _PenDown (e, this) })
  , K: st.BoundBuiltin (1, function (e) { return _StrokeWidth (e, this) })
}


function InitialSymbolTable () {
  var symTab = st.BaseSymbolTable ()
  Object.keys(TurtleGlobals).forEach (
    function (k) { symTab[k] = TurtleGlobals[k] } 
  )
  return symTab
}


function _Forward (exprl, evalState) {
  var howFar, p0, p1
  howFar = evalState.evaluate (exprl[0])
  p0 = evalState.turtle.pos.copy ()
  p1 = p0.copy().
    plus (evalState.turtle.heading.directionVect().times(howFar))
  evalState.svg.push (
    '<line x1="' + p0.x + '" y1="' + p0.y +
        '" x2="' + p1.x + '" y2="' + p1.y +
        '" style="stroke:' + evalState.turtle.color.toString() + 
             ';stroke-width:' + evalState.turtle.strokeWidth + '" />')
  evalState.turtle.pos = p1
  return howFar
}

function _RotateRight (exprl, evalState) {
  var howMuch = evalState.evaluate (exprl[0])
  evalState.turtle.heading.rotate (howMuch)
  return howMuch
}

function _RotateLeft (exprl, evalState) {
  var howMuch = evalState.evaluate (exprl[0])
  evalState.turtle.heading.rotate (-howMuch)
  return howMuch
}

function _MoveBy (exprl, evalState) {
  var p1 = Point ( evalState.evaluate (exprl[0])
                 , evalState.evaluate (exprl[1]) )
    , p0 = evalState.turtle.pos   
  evalState.turtle.pos.plus (p)
  return Math.sqrt( Math.pow(p1.x-p0.x,2) + Math.pow(p1.y-p0.y,2) )
}

function _Alpha (exprl, evalState) {
  var newA = evalState.evaluate (exprl[0])
  evalState.turtle.color.a = newA
  return newA
}

function _Color (exprl, evalState) {
  var newR = evalState.evaluate (exprl[0])
    , newG = evalState.evaluate (exprl[1])
    , newB = evalState.evaluate (exprl[2])
  evalState.turtle.color.r = newR
  evalState.turtle.color.g = newG
  evalState.turtle.color.b = newB
  return (0.299*newR + 0.587*newG + 0.114*newB)
}

// --


function Turtle (heading, pos, color, strokeWidth, penState) {
  var t = Object.create (
    { _heading: 0.0,
      pos:       Point (160, 160),
      color:     Color (0, 0, 0, 1.0),
      strokeWidth: 1,
      penIsDown: true,
      heading: {
        set: function(degrees) { t._heading=degrees },
        rotate: function(degrees) { t._heading+=degrees },
        degrees: function() {return t._heading},
        directionVect: function () {
          var theta = 2 * Math.PI * t._heading / 360
          return Point (Math.sin(theta), Math.cos(theta))
        }
      }
    });
  if (heading) { t._heading = heading }
  if (pos) { t.pos = pos }
  if (color) { t.color = color }
  return t
}

function Point (x, y) {
  p = Object.create (
    { x: 0, y: 0, 
      plus: function (p1) { this.x += p1.x; this.y += p1.y; return this },
      times: function (d) { this.x *= d; this.y *= d; return this },
      copy: function () { return Point (this.x, this.y) },
      toString: function () { return "(" + this.x + "," + this.y + ")" }
    });
  if (x) { p.x = x }
  if (y) { p.y = y }
  return p
}

function Color (r, g, b, a) {
  c = Object.create ( 
    { r: 0, g: 0, b: 0, a: 0 
      , toString: function () 
          { return 'rgba(' + [this.r, this.g, this.b, this.a].join(',') + ')' }
    } 
  )
  if (r) { c.r = r }
  if (g) { c.g = g }
  if (b) { c.b = b }
  if (a) { c.a = a }
  return c
}


function dbg (s) { console.log(s) }

},{"./SymbolTable":3}],6:[function(require,module,exports){


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


},{"./Evaluator":1,"./Parser":2,"./Tokenizer":4,"./TurtlePrimitives":5}],7:[function(require,module,exports){

var svg = require ('./TurtleSVG')

// alert (svg.runProgramSVG('#3{F100R90}'))

window.showOutput = function (inName, outName) {
  var input, output, svgString
  input = document.getElementById(inName),
  output = document.getElementById(outName)
  svgString = svg.runProgramSVG (input.value)
  output.innerHTML = svgString
}

window.runProgramSVGElement = svg.runProgramSVGElement
window.runProgramSVGBody = svg.runProgramSVGBody
},{"./TurtleSVG":6}],8:[function(require,module,exports){
if (typeof Object.create === 'function') {
  // implementation from standard node.js 'util' module
  module.exports = function inherits(ctor, superCtor) {
    ctor.super_ = superCtor
    ctor.prototype = Object.create(superCtor.prototype, {
      constructor: {
        value: ctor,
        enumerable: false,
        writable: true,
        configurable: true
      }
    });
  };
} else {
  // old school shim for old browsers
  module.exports = function inherits(ctor, superCtor) {
    ctor.super_ = superCtor
    var TempCtor = function () {}
    TempCtor.prototype = superCtor.prototype
    ctor.prototype = new TempCtor()
    ctor.prototype.constructor = ctor
  }
}

},{}],9:[function(require,module,exports){
// shim for using process in browser

var process = module.exports = {};

process.nextTick = (function () {
    var canSetImmediate = typeof window !== 'undefined'
    && window.setImmediate;
    var canPost = typeof window !== 'undefined'
    && window.postMessage && window.addEventListener
    ;

    if (canSetImmediate) {
        return function (f) { return window.setImmediate(f) };
    }

    if (canPost) {
        var queue = [];
        window.addEventListener('message', function (ev) {
            var source = ev.source;
            if ((source === window || source === null) && ev.data === 'process-tick') {
                ev.stopPropagation();
                if (queue.length > 0) {
                    var fn = queue.shift();
                    fn();
                }
            }
        }, true);

        return function nextTick(fn) {
            queue.push(fn);
            window.postMessage('process-tick', '*');
        };
    }

    return function nextTick(fn) {
        setTimeout(fn, 0);
    };
})();

process.title = 'browser';
process.browser = true;
process.env = {};
process.argv = [];

function noop() {}

process.on = noop;
process.once = noop;
process.off = noop;
process.emit = noop;

process.binding = function (name) {
    throw new Error('process.binding is not supported');
}

// TODO(shtylman)
process.cwd = function () { return '/' };
process.chdir = function (dir) {
    throw new Error('process.chdir is not supported');
};

},{}],10:[function(require,module,exports){
module.exports = function isBuffer(arg) {
  return arg && typeof arg === 'object'
    && typeof arg.copy === 'function'
    && typeof arg.fill === 'function'
    && typeof arg.readUInt8 === 'function';
}
},{}],11:[function(require,module,exports){
(function (process,global){
// Copyright Joyent, Inc. and other Node contributors.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit
// persons to whom the Software is furnished to do so, subject to the
// following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
// NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
// USE OR OTHER DEALINGS IN THE SOFTWARE.

var formatRegExp = /%[sdj%]/g;
exports.format = function(f) {
  if (!isString(f)) {
    var objects = [];
    for (var i = 0; i < arguments.length; i++) {
      objects.push(inspect(arguments[i]));
    }
    return objects.join(' ');
  }

  var i = 1;
  var args = arguments;
  var len = args.length;
  var str = String(f).replace(formatRegExp, function(x) {
    if (x === '%%') return '%';
    if (i >= len) return x;
    switch (x) {
      case '%s': return String(args[i++]);
      case '%d': return Number(args[i++]);
      case '%j':
        try {
          return JSON.stringify(args[i++]);
        } catch (_) {
          return '[Circular]';
        }
      default:
        return x;
    }
  });
  for (var x = args[i]; i < len; x = args[++i]) {
    if (isNull(x) || !isObject(x)) {
      str += ' ' + x;
    } else {
      str += ' ' + inspect(x);
    }
  }
  return str;
};


// Mark that a method should not be used.
// Returns a modified function which warns once by default.
// If --no-deprecation is set, then it is a no-op.
exports.deprecate = function(fn, msg) {
  // Allow for deprecating things in the process of starting up.
  if (isUndefined(global.process)) {
    return function() {
      return exports.deprecate(fn, msg).apply(this, arguments);
    };
  }

  if (process.noDeprecation === true) {
    return fn;
  }

  var warned = false;
  function deprecated() {
    if (!warned) {
      if (process.throwDeprecation) {
        throw new Error(msg);
      } else if (process.traceDeprecation) {
        console.trace(msg);
      } else {
        console.error(msg);
      }
      warned = true;
    }
    return fn.apply(this, arguments);
  }

  return deprecated;
};


var debugs = {};
var debugEnviron;
exports.debuglog = function(set) {
  if (isUndefined(debugEnviron))
    debugEnviron = process.env.NODE_DEBUG || '';
  set = set.toUpperCase();
  if (!debugs[set]) {
    if (new RegExp('\\b' + set + '\\b', 'i').test(debugEnviron)) {
      var pid = process.pid;
      debugs[set] = function() {
        var msg = exports.format.apply(exports, arguments);
        console.error('%s %d: %s', set, pid, msg);
      };
    } else {
      debugs[set] = function() {};
    }
  }
  return debugs[set];
};


/**
 * Echos the value of a value. Trys to print the value out
 * in the best way possible given the different types.
 *
 * @param {Object} obj The object to print out.
 * @param {Object} opts Optional options object that alters the output.
 */
/* legacy: obj, showHidden, depth, colors*/
function inspect(obj, opts) {
  // default options
  var ctx = {
    seen: [],
    stylize: stylizeNoColor
  };
  // legacy...
  if (arguments.length >= 3) ctx.depth = arguments[2];
  if (arguments.length >= 4) ctx.colors = arguments[3];
  if (isBoolean(opts)) {
    // legacy...
    ctx.showHidden = opts;
  } else if (opts) {
    // got an "options" object
    exports._extend(ctx, opts);
  }
  // set default options
  if (isUndefined(ctx.showHidden)) ctx.showHidden = false;
  if (isUndefined(ctx.depth)) ctx.depth = 2;
  if (isUndefined(ctx.colors)) ctx.colors = false;
  if (isUndefined(ctx.customInspect)) ctx.customInspect = true;
  if (ctx.colors) ctx.stylize = stylizeWithColor;
  return formatValue(ctx, obj, ctx.depth);
}
exports.inspect = inspect;


// http://en.wikipedia.org/wiki/ANSI_escape_code#graphics
inspect.colors = {
  'bold' : [1, 22],
  'italic' : [3, 23],
  'underline' : [4, 24],
  'inverse' : [7, 27],
  'white' : [37, 39],
  'grey' : [90, 39],
  'black' : [30, 39],
  'blue' : [34, 39],
  'cyan' : [36, 39],
  'green' : [32, 39],
  'magenta' : [35, 39],
  'red' : [31, 39],
  'yellow' : [33, 39]
};

// Don't use 'blue' not visible on cmd.exe
inspect.styles = {
  'special': 'cyan',
  'number': 'yellow',
  'boolean': 'yellow',
  'undefined': 'grey',
  'null': 'bold',
  'string': 'green',
  'date': 'magenta',
  // "name": intentionally not styling
  'regexp': 'red'
};


function stylizeWithColor(str, styleType) {
  var style = inspect.styles[styleType];

  if (style) {
    return '\u001b[' + inspect.colors[style][0] + 'm' + str +
           '\u001b[' + inspect.colors[style][1] + 'm';
  } else {
    return str;
  }
}


function stylizeNoColor(str, styleType) {
  return str;
}


function arrayToHash(array) {
  var hash = {};

  array.forEach(function(val, idx) {
    hash[val] = true;
  });

  return hash;
}


function formatValue(ctx, value, recurseTimes) {
  // Provide a hook for user-specified inspect functions.
  // Check that value is an object with an inspect function on it
  if (ctx.customInspect &&
      value &&
      isFunction(value.inspect) &&
      // Filter out the util module, it's inspect function is special
      value.inspect !== exports.inspect &&
      // Also filter out any prototype objects using the circular check.
      !(value.constructor && value.constructor.prototype === value)) {
    var ret = value.inspect(recurseTimes, ctx);
    if (!isString(ret)) {
      ret = formatValue(ctx, ret, recurseTimes);
    }
    return ret;
  }

  // Primitive types cannot have properties
  var primitive = formatPrimitive(ctx, value);
  if (primitive) {
    return primitive;
  }

  // Look up the keys of the object.
  var keys = Object.keys(value);
  var visibleKeys = arrayToHash(keys);

  if (ctx.showHidden) {
    keys = Object.getOwnPropertyNames(value);
  }

  // IE doesn't make error fields non-enumerable
  // http://msdn.microsoft.com/en-us/library/ie/dww52sbt(v=vs.94).aspx
  if (isError(value)
      && (keys.indexOf('message') >= 0 || keys.indexOf('description') >= 0)) {
    return formatError(value);
  }

  // Some type of object without properties can be shortcutted.
  if (keys.length === 0) {
    if (isFunction(value)) {
      var name = value.name ? ': ' + value.name : '';
      return ctx.stylize('[Function' + name + ']', 'special');
    }
    if (isRegExp(value)) {
      return ctx.stylize(RegExp.prototype.toString.call(value), 'regexp');
    }
    if (isDate(value)) {
      return ctx.stylize(Date.prototype.toString.call(value), 'date');
    }
    if (isError(value)) {
      return formatError(value);
    }
  }

  var base = '', array = false, braces = ['{', '}'];

  // Make Array say that they are Array
  if (isArray(value)) {
    array = true;
    braces = ['[', ']'];
  }

  // Make functions say that they are functions
  if (isFunction(value)) {
    var n = value.name ? ': ' + value.name : '';
    base = ' [Function' + n + ']';
  }

  // Make RegExps say that they are RegExps
  if (isRegExp(value)) {
    base = ' ' + RegExp.prototype.toString.call(value);
  }

  // Make dates with properties first say the date
  if (isDate(value)) {
    base = ' ' + Date.prototype.toUTCString.call(value);
  }

  // Make error with message first say the error
  if (isError(value)) {
    base = ' ' + formatError(value);
  }

  if (keys.length === 0 && (!array || value.length == 0)) {
    return braces[0] + base + braces[1];
  }

  if (recurseTimes < 0) {
    if (isRegExp(value)) {
      return ctx.stylize(RegExp.prototype.toString.call(value), 'regexp');
    } else {
      return ctx.stylize('[Object]', 'special');
    }
  }

  ctx.seen.push(value);

  var output;
  if (array) {
    output = formatArray(ctx, value, recurseTimes, visibleKeys, keys);
  } else {
    output = keys.map(function(key) {
      return formatProperty(ctx, value, recurseTimes, visibleKeys, key, array);
    });
  }

  ctx.seen.pop();

  return reduceToSingleString(output, base, braces);
}


function formatPrimitive(ctx, value) {
  if (isUndefined(value))
    return ctx.stylize('undefined', 'undefined');
  if (isString(value)) {
    var simple = '\'' + JSON.stringify(value).replace(/^"|"$/g, '')
                                             .replace(/'/g, "\\'")
                                             .replace(/\\"/g, '"') + '\'';
    return ctx.stylize(simple, 'string');
  }
  if (isNumber(value))
    return ctx.stylize('' + value, 'number');
  if (isBoolean(value))
    return ctx.stylize('' + value, 'boolean');
  // For some reason typeof null is "object", so special case here.
  if (isNull(value))
    return ctx.stylize('null', 'null');
}


function formatError(value) {
  return '[' + Error.prototype.toString.call(value) + ']';
}


function formatArray(ctx, value, recurseTimes, visibleKeys, keys) {
  var output = [];
  for (var i = 0, l = value.length; i < l; ++i) {
    if (hasOwnProperty(value, String(i))) {
      output.push(formatProperty(ctx, value, recurseTimes, visibleKeys,
          String(i), true));
    } else {
      output.push('');
    }
  }
  keys.forEach(function(key) {
    if (!key.match(/^\d+$/)) {
      output.push(formatProperty(ctx, value, recurseTimes, visibleKeys,
          key, true));
    }
  });
  return output;
}


function formatProperty(ctx, value, recurseTimes, visibleKeys, key, array) {
  var name, str, desc;
  desc = Object.getOwnPropertyDescriptor(value, key) || { value: value[key] };
  if (desc.get) {
    if (desc.set) {
      str = ctx.stylize('[Getter/Setter]', 'special');
    } else {
      str = ctx.stylize('[Getter]', 'special');
    }
  } else {
    if (desc.set) {
      str = ctx.stylize('[Setter]', 'special');
    }
  }
  if (!hasOwnProperty(visibleKeys, key)) {
    name = '[' + key + ']';
  }
  if (!str) {
    if (ctx.seen.indexOf(desc.value) < 0) {
      if (isNull(recurseTimes)) {
        str = formatValue(ctx, desc.value, null);
      } else {
        str = formatValue(ctx, desc.value, recurseTimes - 1);
      }
      if (str.indexOf('\n') > -1) {
        if (array) {
          str = str.split('\n').map(function(line) {
            return '  ' + line;
          }).join('\n').substr(2);
        } else {
          str = '\n' + str.split('\n').map(function(line) {
            return '   ' + line;
          }).join('\n');
        }
      }
    } else {
      str = ctx.stylize('[Circular]', 'special');
    }
  }
  if (isUndefined(name)) {
    if (array && key.match(/^\d+$/)) {
      return str;
    }
    name = JSON.stringify('' + key);
    if (name.match(/^"([a-zA-Z_][a-zA-Z_0-9]*)"$/)) {
      name = name.substr(1, name.length - 2);
      name = ctx.stylize(name, 'name');
    } else {
      name = name.replace(/'/g, "\\'")
                 .replace(/\\"/g, '"')
                 .replace(/(^"|"$)/g, "'");
      name = ctx.stylize(name, 'string');
    }
  }

  return name + ': ' + str;
}


function reduceToSingleString(output, base, braces) {
  var numLinesEst = 0;
  var length = output.reduce(function(prev, cur) {
    numLinesEst++;
    if (cur.indexOf('\n') >= 0) numLinesEst++;
    return prev + cur.replace(/\u001b\[\d\d?m/g, '').length + 1;
  }, 0);

  if (length > 60) {
    return braces[0] +
           (base === '' ? '' : base + '\n ') +
           ' ' +
           output.join(',\n  ') +
           ' ' +
           braces[1];
  }

  return braces[0] + base + ' ' + output.join(', ') + ' ' + braces[1];
}


// NOTE: These type checking functions intentionally don't use `instanceof`
// because it is fragile and can be easily faked with `Object.create()`.
function isArray(ar) {
  return Array.isArray(ar);
}
exports.isArray = isArray;

function isBoolean(arg) {
  return typeof arg === 'boolean';
}
exports.isBoolean = isBoolean;

function isNull(arg) {
  return arg === null;
}
exports.isNull = isNull;

function isNullOrUndefined(arg) {
  return arg == null;
}
exports.isNullOrUndefined = isNullOrUndefined;

function isNumber(arg) {
  return typeof arg === 'number';
}
exports.isNumber = isNumber;

function isString(arg) {
  return typeof arg === 'string';
}
exports.isString = isString;

function isSymbol(arg) {
  return typeof arg === 'symbol';
}
exports.isSymbol = isSymbol;

function isUndefined(arg) {
  return arg === void 0;
}
exports.isUndefined = isUndefined;

function isRegExp(re) {
  return isObject(re) && objectToString(re) === '[object RegExp]';
}
exports.isRegExp = isRegExp;

function isObject(arg) {
  return typeof arg === 'object' && arg !== null;
}
exports.isObject = isObject;

function isDate(d) {
  return isObject(d) && objectToString(d) === '[object Date]';
}
exports.isDate = isDate;

function isError(e) {
  return isObject(e) &&
      (objectToString(e) === '[object Error]' || e instanceof Error);
}
exports.isError = isError;

function isFunction(arg) {
  return typeof arg === 'function';
}
exports.isFunction = isFunction;

function isPrimitive(arg) {
  return arg === null ||
         typeof arg === 'boolean' ||
         typeof arg === 'number' ||
         typeof arg === 'string' ||
         typeof arg === 'symbol' ||  // ES6 symbol
         typeof arg === 'undefined';
}
exports.isPrimitive = isPrimitive;

exports.isBuffer = require('./support/isBuffer');

function objectToString(o) {
  return Object.prototype.toString.call(o);
}


function pad(n) {
  return n < 10 ? '0' + n.toString(10) : n.toString(10);
}


var months = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep',
              'Oct', 'Nov', 'Dec'];

// 26 Feb 16:19:34
function timestamp() {
  var d = new Date();
  var time = [pad(d.getHours()),
              pad(d.getMinutes()),
              pad(d.getSeconds())].join(':');
  return [d.getDate(), months[d.getMonth()], time].join(' ');
}


// log is just a thin wrapper to console.log that prepends a timestamp
exports.log = function() {
  console.log('%s - %s', timestamp(), exports.format.apply(exports, arguments));
};


/**
 * Inherit the prototype methods from one constructor into another.
 *
 * The Function.prototype.inherits from lang.js rewritten as a standalone
 * function (not on Function.prototype). NOTE: If this file is to be loaded
 * during bootstrapping this function needs to be rewritten using some native
 * functions as prototype setup using normal JavaScript does not work as
 * expected during bootstrapping (see mirror.js in r114903).
 *
 * @param {function} ctor Constructor function which needs to inherit the
 *     prototype.
 * @param {function} superCtor Constructor function to inherit prototype from.
 */
exports.inherits = require('inherits');

exports._extend = function(origin, add) {
  // Don't do anything if add isn't an object
  if (!add || !isObject(add)) return origin;

  var keys = Object.keys(add);
  var i = keys.length;
  while (i--) {
    origin[keys[i]] = add[keys[i]];
  }
  return origin;
};

function hasOwnProperty(obj, prop) {
  return Object.prototype.hasOwnProperty.call(obj, prop);
}

}).call(this,require("/usr/local/lib/node_modules/browserify/node_modules/insert-module-globals/node_modules/process/browser.js"),typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{"./support/isBuffer":10,"/usr/local/lib/node_modules/browserify/node_modules/insert-module-globals/node_modules/process/browser.js":9,"inherits":8}]},{},[7])