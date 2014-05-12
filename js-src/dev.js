
var tsvg = require ('./TurtleSVG')

pgmState = tsvg.
  startProgramRun (process.argv[2]
                   , function () {
                     console.log( ! (this.instructionCount % 100) );
                     return ! (this.instructionCount % 1)
                   }
                  )
while (! pgmState.done) {
  // console.log (pgmState)
  pgmState.continue ()
}
console.log (pgmState.values ())
