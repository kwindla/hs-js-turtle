
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



