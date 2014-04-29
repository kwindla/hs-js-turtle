
exports.BaseSymbolTable = BaseSymbolTable
exports.BoundValue = BoundValue
exports.BoundBuiltin = BoundBuiltin

var Binding = {
  inspect: function () { return this.type + ' ' + this.v }
}

var SymbolTable = {
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
  // dbg ('retr b ' + sym)
  return this[sym] || BoundValue(0)
}

function _updateBindingForValue (sym, v) {
  this[sym] = BoundValue (v)
  return v
}

function _updateBindingForDefun (sym, arity, expr, symtab) {
  // dbg ('defun st ' + sym + ' - ' + arity)
  this[sym] = BoundDefun (arity, expr, symtab) 
}

function _derivedTable () {
  return Object.create (this)
}



function dbg (s) { console.log(s) }
