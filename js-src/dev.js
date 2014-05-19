
var tsvg = require ('./TurtleSVG')


var args = process.argv.slice(2),
    output = 'svg'

if (! args.length) {
  console.log ('usage: <node> dev.js [-v] "program-string"\n',
               '       the -v option outputs a values array rather than svg\n')
  process.exit()
}

for (var i=0; i<args.length; i++) {
  if (args[i] === '-v') {
    output = 'values'
    args.splice (i, 1)
  }
}

// --

pgmState = tsvg.startProgramRun (args[0])
while (! pgmState.done) {
  // console.log (pgmState)
  pgmState.continue ()
}

if (output === 'svg') {
  console.log (pgmState.SVGElement())
}
if (output === 'values') {
  console.log (pgmState.values ())
}
