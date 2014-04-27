
exports.evaluate = function (exprl, symbolTable) {
  var state = EvalState(symbolTable)
  exprl.forEach (function (expr) { var r = state.evaluate (expr, symbolTable)
                                   state.values.push (r) 
                                 }
                )
  return state.values
}


function EvalState (symTab) {
  return {
    values: [],        // output values from top-level expression list
    outLines: [],
    symTab: symTab,
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
    dbg ('funcall type test ' + binding.type + ' ' + e.v)
    if (binding.type == 'BoundBuiltin') {
      // FIX: implement
    }
    if (binding.type == 'BoundDefun') {
      dbg ('funcall defun')
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
    // FIX: symbol table manip
    var result
    for (var i=0; i<e.exprl.length; i++) {
      result = this.evaluate (e.exprl[i])
    }
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



