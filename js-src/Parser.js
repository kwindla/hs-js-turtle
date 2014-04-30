
exports.parse = parse


var tp = require('./TurtlePrimitives')


var ParseState = {
  tokens: undefined,    // these will be set by arguments to parse ()
  symTab: undefined,
  expressionList:       _expressionList,
  expression:           _expression,
  term:                 _term,
  expressionTail:       _expressionTail,
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
    return this.expressionTail (this.term())
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
                                    v: {value: c} } )
}

function Defun (c, arity, expr) {
  return Object.create (ExprNode, { type: {value: 'Defun'},
                                    v: {value: c},
                                    arity: {value: arity},
                                    e: {value: expr},
                                    inspect: { value:
    function () {
      return (this.type + ' ' + this.v + ' ' + this.arity +
              ' { ' + this.e.inspect() + ' }')
    } } })
}

function Funcall (arity, sym, exprl) {
  return Object.create (ExprNode, { type: {value: 'Funcall'},
                                    v: {value: sym}, 
                                    exprl: {value: exprl}, // args list
                                    inspect: {value:
    function () {
      return (this.type + ' ' + this.v + ' { '  +
              this.exprl.map ( function(e){return e.inspect()} ).join(', ') +
              ' } ')
    } } })
}

function Assignment (sym, expr) {
  return Object.create (ExprNode, { type: {value: 'Assignment'},
                                    v: {value: sym},
                                    e: {value: expr},
                                    inspect: {value:
    function () {
      return (this.type + ' ' + this.v + ' (' + this.e.inspect() + ')')
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
      return (this.type + ' ' + this.v + ' (' + this.left.inspect() + ')' +
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
      return (this.type + ' ' + this.v + ' (' + this.e.inspect() + ')')
    } } })
}

function TernaryIf (econd, eif, ethen) {
  return Object.create (ExprNode, { type:    {value: 'TernaryIf'},
                                    econd:   {value: econd},
                                    eif:     {value: eif},
                                    ethen:   {value: ethen},
                                    inspect: { value:
    function () {
      return (this.type + ' ' + this.econd.inspect() +
              ' (' + this.eif.inspect() + ')' +
              ' (' + this.ethen.inspect() + ')' )
    } } })
}

function ExprTreeListNode (exprl) {
  return Object.create (ExprNode, { type: {value: 'ExprTreeListNode'},
                                    exprl: {value: exprl},
                                    inspect: { value:
    function () {
      return (this.type + ' ' + ' { ' +
              this.exprl.map ( function(e){return e.inspect()} ).join(', ') + 
              ' }')
    } } })
}

function Repeat (ntimes, expr) {
  return Object.create (ExprNode, { type: {value: 'Repeat'},
                                    ntimes: {value: ntimes},
                                    e: {value: expr},
                                    inspect: {value:
    function () {
      return (this.type + ' ' + this.ntimes.inspect() + 
              ' { ' + this.e.inspect() + ' }')
    } } })
}
