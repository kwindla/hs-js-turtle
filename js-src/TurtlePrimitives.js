
exports.InitialSymbolTable = InitialSymbolTable
exports.Turtle = Turtle


var st = require ('./SymbolTable')


var TurtleGlobals = {
  // for functions defined here, "this" will be the current
  // EvalState. e is the passed-in exprl of args to the function
    P: st.BoundValue (Math.PI)
  , F: st.BoundBuiltin (1, _Forward)
  , R: st.BoundBuiltin (1, _RotateRight)
  , L: st.BoundBuiltin (1, _RotateLeft)

  , M: st.BoundBuiltin (2, _MoveTo)
  , B: st.BoundBuiltin (2, _MoveBy)
  , T: st.BoundBuiltin (1, _TurnTo)
  , H: st.BoundBuiltin (1, _TurnBy)

  , A: st.BoundBuiltin (1, _Alpha)
  , C: st.BoundBuiltin (3, _Color)

  , '^': st.BoundBuiltin (0, _PenToggle)
  , U: st.BoundBuiltin (0, _PenUp)
  , N: st.BoundBuiltin (0, _PenDown)
  , K: st.BoundBuiltin (1, _StrokeWidth)
}


function InitialSymbolTable () {
  var symTab = st.BaseSymbolTable ()
  Object.keys(TurtleGlobals).forEach (
    function (k) { symTab[k] = TurtleGlobals[k] } 
  )
  return symTab
}


// --

// FIX: respect pen position -- draw when pen is down
function _Forward (exprl, s, k) {
  var howFar, p0, p1
  return s.eval (
    exprl[0], s,
    function (howFar) {
      var p0 = s.turtle.pos.copy ()
      var p1 = p0.copy().
        plus (s.turtle.heading.directionVect().times(howFar))
      if (s.turtle.penIsDown) { lineFromTo (p0, p1, s) }
      s.turtle.pos = p1
      return k (howFar)
    });
}

function _RotateRight (exprl, s, k) {
  return s.eval (exprl[0], s,
                 function (howMuch) {
                   s.turtle.heading.rotate (howMuch)
                   return k (howMuch)
                 });
}

function _RotateLeft (exprl, s, k) {
  return s.eval (exprl[0], s,
                 function (howMuch) {
                   s.turtle.heading.rotate (-howMuch)
                   return k (howMuch)
                 });
}

function _MoveBy (exprl, s, k) {
  return s.eval (
    exprl[0], s,
    function (x) {
      return s.eval (exprl[1], s,
                         function (y) {
                           var p0 = s.turtle.pos.copy ()
                           var p1 = s.turtle.pos.plus (Point (x,y))
                           if (s.turtle.penIsDown) {
                             lineFromTo (p0, p1, s)
                           }
                           return k (Math.sqrt( Math.pow(p1.x-p0.x,2) + 
                                                Math.pow(p1.y-p0.y,2) ))
                         });
    });
}

function _MoveTo (exprl, s, k) {
  return s.eval (
    exprl[0], s,
    function (x) {
      return s.eval (exprl[1], s,
                     function (y) {
                       var p0 = s.turtle.pos.copy ()
                       var p1 = Point (x,y)
                       if (s.turtle.penIsDown) {
                         lineFromTo (p0, p1, s)
                       }
                       s.turtle.pos = p1
                       return k (Math.sqrt( Math.pow(p1.x-p0.x,2) + 
                                            Math.pow(p1.y-p0.y,2) ))
                     });
    });
}

function _TurnBy (exprl, s, k) {
  return s.eval (exprl[0], s,
                 function (d) { return k (s.turtle.heading.rotate (d)) })
}

function _TurnTo (exprl, s, k) {
  return s.eval (exprl[0], s,
                 function (d) { return k (s.turtle.heading.set (d)) })

}

function _Alpha (exprl, s, k) {
  return s.eval (exprl[0], s,
                 function (newA) { s.turtle.color.a = newA;
                                   return k (newA) })
}

//FIX:
function _Color (exprl, evalState) {
  var newR = evalState.evaluate (exprl[0])
    , newG = evalState.evaluate (exprl[1])
    , newB = evalState.evaluate (exprl[2])
  evalState.turtle.color.r = newR
  evalState.turtle.color.g = newG
  evalState.turtle.color.b = newB
  return (0.299*newR + 0.587*newG + 0.114*newB)
}

function _PenToggle (exprl, s, k) {
  return k (s.turtle.penIsDown = !s.turtle.penIsDown)
  
}

function _PenUp (exprl, s, k) {
  return k (s.turtle.penIsDown = false)
}

function _PenDown (exprl, s, k) {
  return k (evalState.turtle.penIsDown = true)
}

function _StrokeWidth (exprl, s, k) {
  return s.eval (
    exprl[0], s,
    function (width) { return k (s.turtle.strokeWidth = width) })
}

function lineFromTo (p0, p1, evalState) {
  evalState.svg.push (
    '<line x1="' + p0.x + '" y1="' + p0.y +
      '" x2="' + p1.x + '" y2="' + p1.y +
      '" style="stroke:' + evalState.turtle.color.toString() + 
      ';stroke-width:' + evalState.turtle.strokeWidth + '" />'
  )
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
        set: function(degrees) { return t._heading=degrees },
        rotate: function(degrees) { return t._heading+=degrees },
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
