
// Quick and dirty Color class for use by TurtlePrimitives.js
// Import as:
//
//   var Color = require ('./Color')
//
// Expects 1-99 values to be passed as arguments to constructor rather
// than 0-255 (to save characters in graphics programs). Indexed
// colors list is from the svg documentation.
// http://www.w3.org/TR/SVG/types.html#ColorKeywords

var colorsTable = require ('./colors')

function Color (r, g, b, a) {
  var d = 255/99

  c = Object.create ( 
    { r: 0, g: 0, b: 0, a: 0
      , setRGB: function (R,G,B) { this.r=R*d; this.g=G*d; this.b=B*d }
      , setRGBFromBytes: function (R,G,B) { this.r=R; this.g=G, this.b=B }
      , toString: function () {
          return 'rgba(' + [this.r, this.g, this.b].
                    map(function(c){return Math.round(c)}).join(',') + 
                    ',' + this.a.toFixed(2) + ')' }
      , toStringNoAlpha: function () {
          return 'rgb(' + [this.r, this.g, this.b].
                    map(function(c){return Math.round(c)}).join(',') + ')' }
      , setFromIndex: function (idx) {        
        var c = colorsTable [idx]
        if (!c) { dbg ("no color found for", idx); return }
        this.setRGBFromBytes (c.r, c.g, c.b)
      }
    }
  )
  if (r) { c.r = r*d }
  if (g) { c.g = g*d }
  if (b) { c.b = b*d }
  if (a) { c.a = a/99}

  return c
}


module.exports = Color


function dbg () { console.log.apply(null, arguments) }
