
var _st = require ('./SymbolTable')

exports.evaluate = function (exprl, symbolTable, extraFields, yieldTest) {
  var s = EvalState (symbolTable, extraFields, yieldTest)

  var inner_eval = function () {
    var e0 = exprl.shift ()
    if (!e0) { return { done: true } }
    return _evaluate (e0, s,
                      function (v) { s._values.push (v);
                                     return inner_eval })
  }
  // dbg ('calling continue')
  s.k = inner_eval
  s.done = false
  return s
}


function EvalState (symbolTable, extraFields, yieldTest) {
  var s = {
    _values:   [],        // output values from top-level expression list
    instructionCount: 0,

    eval: _evaluate,
    values: function () { return this._values },
    push: function (x) { return this.stack.push(x) },
    pop: function () { return this.stack.pop() }
  }

  s.symTab = symbolTable
  s.yieldTest = yieldTest || function () { return false }

  s.continue = function () {
    var next = this.k
    if (! next) { throw ("don't know how to continue (no k)") }

    while (true) {
      // dbg ("top of while")
      // dbg (next)
      // dbg (this)
      next = next ()
      if (next.done) {
        this.done = true
        return this
      }
      if (this.yieldTest ()) {
        this.done = false;
        this.k = next
        return this
      }
    }
  }


  Object.keys(extraFields).forEach (
    function (k) { s[k] = extraFields[k] }
  )
  return s
}


function _evaluate (expr, s, k) {
  var f = EvalTable[expr.type]
  // dbg ('_e ' + expr.type)
  if (! f) { throw 'unkown expression type - ' + expr.type }
  s.instructionCount++
  // here, rather than returning simply 'f (expr, s, k)', we wrap this
  // call in a lambda, making a top-level point that always returns to
  // the continue() routine. doing this simplifies the implementation
  // of all of the eval routines in the table, below, such that we
  // don't need to wrap every tail call we make to _evaluate. the cost
  // is that our yieldTest will get called twice for "simple" machine
  // instructions like Symbol and ConstantNumber. if we were
  // optimizing, we might choose to fix this.
  return function () { return f (expr, s, k) }
}



// pre-CPS commit 7235c300f0b95322db85c063851b92e67be70a3b


// FIX: RESULT is a global!
function eval_rpt (c, s, k, f, idx) {
  if (!idx) { idx = 0 }
      if (c > 0) {
        return f (
          function (v) { 
            result = v; return eval_rpt (c-1, s, k, f, idx+1) 
          },
          idx)
      } else {
        return k (result)
      }
    }



var EvalTable = {
  Assignment: function (e, s, k) {
    // var value = this.evaluate (e.e)
    // this.symTab.updateBindingForValue (e.v, value)
    // return value
    return _evaluate (e.e, s,
                 function (v) {
                   s.symTab.updateBindingForValue (e.v, v)
                   return k (v)
                 })
  },
  Symbol: function (e, s, k) {
    // var binding = this.symTab.retrBinding (e.v)
    // return binding.v
    // dbg ('in s : ' + k)
    return k (s.symTab.retrBinding(e.v).v)
  },
  Defun: function (e, s, k) {
    // here we can do things differently than on the haskell side
    // because we have references and mutable objects. we'll save our
    // current symbol table ref to use when we're funcall'ed, and
    // insert a new derived table to protect this saved symbol table
    // from getting new entries otherwise
    // this.symTab.updateBindingForDefun (e.v, e.arity, e.e, this.symTab)
    // this.symTab = this.symTab.derivedTable ()
    // return e.arity
    s.instructionCount++
    s.symTab.updateBindingForDefun (e.v, e.arity, e.e, s.symTab)
    s.symTab = s.symTab.derivedTable ()
    return k (e.arity)
  },
  Funcall: function (e, s, k) {
    var binding
    s.instructionCount++

    binding = s.symTab.retrBinding (e.v)
    if (binding.type == 'BoundBuiltin') {
      return binding.f (e.exprl, s, k)
    }

    var  callTimeSymTab = s.symTab,
      funcEvalSymTab,
      args,
      retval
    if (binding.type == 'BoundDefun') {
      funcEvalSymTab = binding.symTab.derivedTable ()      
      argLabels = "abcdefghijklmnopqrstuvwxyz"
      count = 0
      function eval_args () {
        if (count < e.exprl.length) {
          s.instructionCount++
          return _evaluate (
            e.exprl[count], s,
            function (v) { funcEvalSymTab[argLabels.charAt(count)] =
                           _st.BoundValue (v) 
                           count++
                           return eval_args
                         })
        } else {
          s.instructionCount++
          return callk ()
        }
      }
      return eval_args ()

      function callk () {
        s.symTab = funcEvalSymTab
        return _evaluate (binding.e, s,
                          function (v) {
                            s.symTab = callTimeSymTab
                            return k (v)
                          })
      }
    }
    // plan of action: derive a symbol table from our saved
    // defun-scope symbol table (which, remember, is just a parent
    // of the calling scope's table), make entries in that new table
    // for the function's arguments, swap in that symbol table, eval
    // the function body, put back the symbol table from the calling
    // scope, and return
  },
  BinaryOp: function (e, s, k) {
    return _evaluate (e.left, s,
               function (l) {
                 return _evaluate (e.right, s,
                                   function (r) { return k (e.f(l,r)) }
                 )
               })
  },
  UnaryOp: function (e, s, k) {
    return _evaluate (e.e, s, function (v) { return k ( e.f(v)) })
  },
  ConstantNumber: function (e, s, k) { 
    return k (e.v)
  },
  TernaryIf: function (e, s, k) {
    return _evaluate (e.econd, s,
                      function (v) {
                        if (v !=0) {
                          return _evaluate (e.eif, s,
                                            function (v) { return k (v) })
                        } else {
                          return _evaluate (e.ethen, s,
                                            function (v) { return k (v) })
                 }
               })
  },
  ExprTreeListNode: function (e, s, k) {
    var outerSymTab = s.symTab,
        result,
        eval_list,
        count
    s.symTab = s.symTab.derivedTable ()
    count = 0
    return function eval_list () {
      if (count < e.exprl.length) {
        return _evaluate (e.exprl[count++], s,
                          function (v) { result=v; return eval_list })
      } else {
        s.symTab = outerSymTab
        return k (result)
      }
    }
  },
  Repeat: function (e, s, k) {
    // need to handle expression list blocks differently from bare
    // expressions, here for a block, we want to repeat multiple times
    // with a stateful symbol table across all repeats, then "pop" that
    // symbol table and throw it away.


    function eval_single_expr (ntimes, k) {
      return eval_rpt (ntimes, s, k,
                       function (rpt_k) { return _evaluate (e.e, s, rpt_k) })
    }

    function eval_expr_list (ntimes, k) {
      var outerSymTab = s.symTab
      s.symTab = s.symTab.derivedTable ()
      return eval_rpt (
        ntimes, s, function(v) { s.symTab = outerSymTab; return k(v) },
        function (rpt_k_loop) {
          return eval_rpt (e.e.exprl.length, s, rpt_k_loop,
                           function (rpt_k_eval, i) {
                              return _evaluate (e.e.exprl[i], s, rpt_k_eval)
                           })
        })
     }

    return _evaluate (e.ntimes, s,
                      function (ntimes) {
                        if (e.e.is ('ExprTreeListNode')) {
                          return eval_expr_list (ntimes, k)
                        } else {
                          return eval_single_expr (ntimes, k)
                        }
                      })
  }
}



//

function dbg (s) { console.log(s) }



