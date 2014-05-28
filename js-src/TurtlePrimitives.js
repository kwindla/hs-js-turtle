
exports.InitialSymbolTable = InitialSymbolTable
exports.Turtle = Turtle


var st = require ('./SymbolTable')
var Color = require ('./Color')

var TurtleGlobals = {
  // for functions defined here, "this" will be the current
  // EvalState. e is the passed-in exprl of args to the function

    F: st.BoundBuiltin (1, _Forward)
  , R: st.BoundBuiltin (1, _RotateRight)
  , L: st.BoundBuiltin (1, _RotateLeft)

  , M: st.BoundBuiltin (2, _MoveTo)
  , N: st.BoundBuiltin (2, _MoveBy)

  , "'": st.BoundBuiltin (1, _TurnTo)
  , '"': st.BoundBuiltin (1, _TurnBy)

  , P: st.BoundValue (Math.PI)

  , A: st.BoundBuiltin (1, _StrokeAlpha)
  , B: st.BoundBuiltin (1, _FillAlpha)
  , C: st.BoundBuiltin (1, _StrokeIndexedColor)
  , D: st.BoundBuiltin (1, _FillIndexedColor)
  , E: st.BoundBuiltin (1, _StrokeWidth)

  , G: st.BoundBuiltin (3, _StrokeColor)
  , H: st.BoundBuiltin (3, _FillColor)

  , '^': st.BoundBuiltin (0, _PenToggle)

  , T: st.BoundBuiltin (2, _Box)
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

function _StrokeAlpha (exprl, s, k) {
  return s.eval (exprl[0], s,
                 function (newA) { return k (s.turtle.strokeColor.a(newA)) })

}

function _FillAlpha (exprl, s, k) {
  return s.eval (exprl[0], s,
                 function (newA) { return k (s.turtle.fillColor.a(newA)) })

}

function _StrokeColor (exprl, s, k) {
  return s.eval (
    exprl[0], s,
    function (r)
    { return s.eval (
      exprl[1], s, 
      function (g) 
      { return s.eval (
        exprl[2], s,
        function (b)
        { s.turtle.strokeColor.set (r, g, b)
          return k (0.299*r + 0.587*g + 0.114*b) })
      })
    })
}

function _StrokeIndexedColor (exprl, s, k) {
  return s.eval (exprl[0], s, function (idx)
                 { s.turtle.strokeColor.setFromIndex (idx)
                   return k (idx) })
}

function _FillColor (exprl, s, k) {
  return s.eval (
    exprl[0], s,
    function (r)
    { return s.eval (
      exprl[1], s,
      function (g)
      { return s.eval (
        exprl[2], s,
        function (b)
        { s.turtle.fillColor.set (r, g, b)
          return k (0.299*r + 0.587*g + 0.114*b) })
      })
    })
}

function _FillIndexedColor (exprl, s, k) {
  return s.eval (exprl[0], s, function (idx)
                 { s.turtle.fillColor.setFromIndex (idx)
                   return k (idx) })
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

function _Box (exprl, s, k) {
  return s.eval (
    exprl[0], s,
    function (width) {
      return s.eval (
        exprl[1], s,
        function (height) {
          var bl = s.turtle.pos.copy().plus(Point(-(width/2), -(height/2)))
          var str = ""
          str +=
  '<rect x="' + bl.x + '" y="' + bl.y + '" ' +
       ' width="' + width + '" height="' + height + '"' +
       ' fill="' + s.turtle.fillColor.toStringNoAlpha() + '"' +
       ' fill-opacity="' + s.turtle.fillColor.a().toFixed(2) + '"' +
       ' transform="rotate(' + (-s.turtle.heading.degrees()) + ',' + 
                               s.turtle.pos.x + ',' + s.turtle.pos.y + ')"'
          if (s.turtle.penIsDown) {
            str += ' style="stroke:' + s.turtle.strokeColor.toString() +
                   ';stroke-width:' + s.turtle.strokeWidth + '"'

          }
          str += (' />')
          s.svg.push (str)
          return k (width * height)
        });
    });  
}


function lineFromTo (p0, p1, evalState) {
  evalState.svg.push (
    '<line x1="' + p0.x.toFixed(2) + '" y1="' + p0.y.toFixed(2) +
      '" x2="' + p1.x.toFixed(2) + '" y2="' + p1.y.toFixed(2) +
      '" style="stroke:' + evalState.turtle.strokeColor.toString() + 
      ';stroke-width:' + evalState.turtle.strokeWidth + 
      ';stroke-linecap:round' +
      '" />'
  )
}

// --


function Turtle (heading, pos, color, strokeWidth, penState) {
  var t = Object.create (
    { _heading:        0.0,
      pos:             Point (50, 50),
      strokeColor:     Color (0, 0, 0, 99),
      fillColor:       Color (50, 50, 50, 99),
      strokeWidth:     1.0,
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
  if (color) { t.strokeColor = color }
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


function dbg (s) { console.log(s) }
