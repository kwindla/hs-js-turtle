
exports.runProgram = runProgram

var EvalState = {
  values: [],        // output values saved
  turtle: null,
  symbolTable: {},
  outLines: null,
  evaluate: evaluate
}

function runProgram (exprl) {
  var state = Object.create (EvalState)
  exprl.forEach (function (expr) { var r = evaluate.apply (state, [expr])
                                   dbg (r)
                                   state.values.push (r) })
  return state.values
}

function dbg (s) { console.log(s) }

function evaluate (expr) {
  var f = EvalTable[expr.type]
  dbg ('ev:' + expr.type)
  if (! f) { throw 'unkown expression type - ' + expr.type }
  return f.apply(this,[expr])
}

var EvalTable = {
  Assignment: function (expr) {
  },
  ConstantNumber: function (expr) {
    return expr.v
  },
  BinaryOp: function (expr) {
    var left  = this.evaluate (expr.left)
    var right = this.evaluate (expr.right)
    return expr.f (left, right)
  }
}
