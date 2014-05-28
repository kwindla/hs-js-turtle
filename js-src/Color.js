
// Quick and dirty Color class for use by TurtlePrimitives.js
// Import as:
//
//   var Color = require ('./Color')
//
// Expects 1-99 values to be passed as arguments to constructor rather
// than 0-255 (to save characters in graphics programs).


var colorsTable = require ('./colors.json')


function Color (r, g, b, a) {
  var d = 255/99

  c = Object.create ( 
    { _r: 0, _g: 0, _b: 0, _a: 1.0

      , r: function (R) { if (R) { this._r=R*d; } return this._r }
      , g: function (G) { if (G) { this._g=G*d; } return this._g }
      , b: function (B) { if (B) { this._b=B*d; } return this._b }
      , a: function (A) { if (A) { this._a=A/99; } return this._a }

      , set: function (R,G,B,A) { this.r(R); this.g(G); this.b(B); this.a(A); 
                                  return this }
      , setRaw: function (R,G,B,A) { if (R) { this._r = R; }
                                     if (G) { this._g = G; }
                                     if (B) { this._b = B; }
                                     if (A) { this._a = A; }
                                     return this }

      , toString: function () {
          return 'rgba(' + [this._r, this._g, this._b].
                    map(function(c){return Math.round(c)}).join(',') + 
                    ',' + this._a.toFixed(2) + ')' }
      , toStringNoAlpha: function () {
          return 'rgb(' + [this._r, this._g, this._b].
                    map(function(c){return Math.round(c)}).join(',') + ')' }

      , setFromIndex: function (idx) {        
        var c = colorsTable [idx]
        if (!c) { dbg ("no color found for", idx); return }
        this.setRaw (c.r, c.g, c.b)
      }
    }
  )

  c.set (r, g, b, a)
  return c
}


module.exports = Color


function dbg () { console.log.apply(null, arguments) }
