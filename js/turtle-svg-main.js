
var svg = require ('./TurtleSVG')

// alert (svg.runProgramSVG('#3{F100R90}'))

window.showOutput = function (inName, outName) {
  var input, output, svgString
  input = document.getElementById(inName),
  output = document.getElementById(outName)
  svgString = svg.runProgramSVG (input.value)
  output.innerHTML = svgString
}
