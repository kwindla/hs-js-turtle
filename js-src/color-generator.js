
var d3 = require('d3');

var black, white, colors, interpl, palette

black  = d3.rgb (0, 0, 0);
white  = d3.rgb (255, 255, 255);

colors = [ 'red', 'orange', 'yellow', 'greenYellow', 'green', 'greenCyan',
           'cyan', 'blueCyan', 'blue', 'blueMagenta', 'magenta', 'redMagenta' ]
  .map (function (str, idx) { return d3.hsl (idx*30, 1, 0.5) });


interpl = d3.interpolateLab (black, white);
palette = [];
for (var i=0; i<16; i++) {
  palette.push (interpl ((1/15)*i));
}

colors.forEach (function (color) {
  interpl = d3.interpolateLab (white, color)
  palette.push (interpl(0.25), interpl(0.5), interpl(0.75));
  palette.push (color)
  interpl = d3.interpolateLab (color, black)
  palette.push (interpl(0.25), interpl(0.5), interpl(0.75));
});

console.log (palette.map (function (c) { return d3.rgb(c) }));

//

function dbg() { console.log.apply (null, arguments) }
