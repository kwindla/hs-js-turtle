
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
