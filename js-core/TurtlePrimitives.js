
exports.InitialSymbolTable = InitialSymbolTable
exports.Turtle = Turtle


var st = require ('./SymbolTable')


var TurtleGlobals = {
  // for functions defined here, "this" will be the current
  // EvalState. e is the passed-in exprl of args to the function
  P: st.BoundValue (Math.PI),
  F: st.BoundBuiltin (1, function (e) { return _Forward (e, this) }),
  R: st.BoundBuiltin (1, function (e) { return _RotateRight (e, this) }),
  L: st.BoundBuiltin (1, function (e) { return _RotateLeft (e, this) })
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
  evalState.svg.push ('<line x1="' + p0.x + '" y1="' + p0.y +
                          '" x2="' + p1.x + '" y2="' + p1.y +
                          '" style="stroke:rgb(0,0,0);stroke-width:2" />')
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


function Turtle (heading, pos, color) {
  var t = Object.create (
    { _heading: 0.0,
      pos:       Point (200, 200),
      color:     Color (0, 0, 0),
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

function Color (r, g, b) {
  c = Object.create ( { r: 0, g: 0, b: 0 } )
  if (r) { c.r = r }
  if (g) { c.g = g }
  if (b) { c.b = b }
  return c
}


function dbg (s) { console.log(s) }
