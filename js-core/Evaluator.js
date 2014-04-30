

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
    // fix: symbol table derivation
    this.symTab.updateBindingForDefun (e.v, e.arity, e.e, null)
    return e.arity
  },
  Funcall: function (e) {
    binding = this.symTab.retrBinding (e.v)
    if (binding.type == 'BoundBuiltin') {
      return binding.f.apply (this, [e.exprl])
    }
    if (binding.type == 'BoundDefun') {
      var args = []
      // FIX: do symbol table stuff for args (and scope)
      for (var i=0; i<binding.arity; i++) {
        args.push (this.evaluate (e.exprl[i]))
      }
      return this.evaluate (binding.e)
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
    // FIX: and, again, symbol table ... tricky block difference from
    // singular expression
    var ntimes = this.evaluate (e.ntimes),
        result;
    for (var i=0; i<ntimes; i++) {
      result = this.evaluate (e.e)
    }
    return result;
  }

}

//

function dbg (s) { console.log(s) }



