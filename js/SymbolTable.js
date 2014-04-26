
exports.BaseSymbolTable = BaseSymbolTable

var Binding = {
  inspect: function () { return this.type + ' ' + this.v }
}

var SymbolTable = {
  retrBinding:           _retrBinding,
  updateBindingForValue: _updateBindingForValue,
  updateBindingForDefun: _updateBindingForDefun,

  derivedTable: _derivedTable,

  P: BoundValue (Math.PI),
  F: BoundBuiltin (1, function (e) {}),
  R: BoundBuiltin (1, function (e) {}),
  L: BoundBuiltin (1, function (e) {})
}

function BoundValue (v) {
  return Object.create (Binding, { type: {value: 'BoundValue'},
                                   v:    {value: v} })
}

function BoundBuiltin (arity, f) {
  return Object.create (Binding, { type:  {value: 'BoundBuiltin'},
                                   arity: {value: arity},
                                   f:     {value: f},
                                   v:     {value: arity} // for inspect
                                 })
}

function BaseSymbolTable () {
  return Object.create (SymbolTable)
}

function _retrBinding (sym) {
  return this[sym] || BoundValue(0)
}

function _updateBindingForValue (sym, v) {
}

function _updateBindingForDefun (sym, arity, expr, symtab) {
}

function _derivedTable (symTab) {
}



