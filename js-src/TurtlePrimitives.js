
exports.InitialSymbolTable = InitialSymbolTable
exports.Turtle = Turtle


var st = require ('./SymbolTable')


var TurtleGlobals = {
  // for functions defined here, "this" will be the current
  // EvalState. e is the passed-in exprl of args to the function
    P: st.BoundValue (Math.PI)
  , F: st.BoundBuiltin (1, function (e) { return _Forward (e, this) })
  , R: st.BoundBuiltin (1, function (e) { return _RotateRight (e, this) })
  , L: st.BoundBuiltin (1, function (e) { return _RotateLeft (e, this) })

  , B: st.BoundBuiltin (2, function (e) { return _MoveBy (e, this) })
  , T: st.BoundBuiltin (1, function (e) { return _TurnTo (e, this) })
  , M: st.BoundBuiltin (1, function (e) { return _MoveTo (e, this) })

  , A: st.BoundBuiltin (1, function (e) { return _Alpha (e, this) })
  , C: st.BoundBuiltin (3, function (e) { return _Color (e, this) })

  , '^': st.BoundBuiltin (1, function (e) { return _PenToggle (e, this) })
  , U: st.BoundBuiltin (1, function (e) { return _PenUp (e, this) })
  , N: st.BoundBuiltin (1, function (e) { return _PenDown (e, this) })
  , K: st.BoundBuiltin (1, function (e) { return _StrokeWidth (e, this) })
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
  evalState.svg.push (
    '<line x1="' + p0.x + '" y1="' + p0.y +
        '" x2="' + p1.x + '" y2="' + p1.y +
        '" style="stroke:' + evalState.turtle.color.toString() + 
             ';stroke-width:' + evalState.turtle.strokeWidth + '" />')
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

function _MoveBy (exprl, evalState) {
  var p1 = Point ( evalState.evaluate (exprl[0])
                 , evalState.evaluate (exprl[1]) )
    , p0 = evalState.turtle.pos   
  evalState.turtle.pos.plus (p)
  return Math.sqrt( Math.pow(p1.x-p0.x,2) + Math.pow(p1.y-p0.y,2) )
}

function _Alpha (exprl, evalState) {
  var newA = evalState.evaluate (exprl[0])
  evalState.turtle.color.a = newA
  return newA
}

function _Color (exprl, evalState) {
  var newR = evalState.evaluate (exprl[0])
    , newG = evalState.evaluate (exprl[1])
    , newB = evalState.evaluate (exprl[2])
  evalState.turtle.color.r = newR
  evalState.turtle.color.g = newG
  evalState.turtle.color.b = newB
  return (0.299*newR + 0.587*newG + 0.114*newB)
}

// --


function Turtle (heading, pos, color, strokeWidth, penState) {
  var t = Object.create (
    { _heading: 0.0,
      pos:       Point (160, 160),
      color:     Color (0, 0, 0, 1.0),
      strokeWidth: 1,
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

function Color (r, g, b, a) {
  c = Object.create ( 
    { r: 0, g: 0, b: 0, a: 0 
      , toString: function () 
          { return 'rgba(' + [this.r, this.g, this.b, this.a].join(',') + ')' }
    } 
  )
  if (r) { c.r = r }
  if (g) { c.g = g }
  if (b) { c.b = b }
  if (a) { c.a = a }
  return c
}


function dbg (s) { console.log(s) }
