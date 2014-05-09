/**
 * @jsx React.DOM
 *
 */


var examplePgmStrings = [
    "#10{#3{F120R120}R36}"
  , "#20{F10R5}"
  , "B(-110)(-85)#24{F170R75}"
  , "#8{R45#4{#90{F(18/10)R2}R90}}"
  , "#36{R10#8{F55L45}}"
]

var ProgramInput, RunItButton, CharCount, ProgramGraphic,
    EvalAndDisplayWidget, TryExample, ExamplesWidget,
    eADWComponent, exWidgComponent



React.initializeTouchEvents(true);


// Textarea element for user-inputted program text. Expects a value
// prop containing a chunk of text with which to pre-fill. We've
// written the component this way to adhere to react's philosophy of
// controlled form components. --
// http://facebook.github.io/react/docs/forms.html
//
// Also expects an optional charCountHook prop. We'll call this hook
// function each time our text value changes or is externally updated,
// passing it the length in characters of our value text
// 
ProgramInput = React.createClass ({
  getInitialState: function() {
    return {value: '', charCountHook: null};
  },
  componentWillMount: function() {
    this.setState({ value: removeZWB(this.props.value)
                  , charCountHook: this.props.charCountHook })
  },
  componentWillReceiveProps: function(nextProps) {
    this.setState({value: removeZWB(nextProps.value)});
    if (this.state.charCountHook) {
      this.state.charCountHook (nextProps.value.length)
    }
  },
  handleChange: function(event) {
    this.setState({value: event.target.value});
    if (this.state.charCountHook) {
      this.state.charCountHook (event.target.value.length)
    }
  },
  render: function() {
    var value = this.state.value;
    return (
        <textarea id="program-input"
                  value={value}
                  onChange={this.handleChange}
        />
    );
  }
});

// Button to trigger program evaluation and graphic
// re-rendering. Expects a callback as an onClick prop.
//
RunItButton = React.createClass ({
  render: function() {
    return (
      <div id="run-button"
        onClick={this.props.onClick}>RUN THE PROGRAM &gt;</div>
    );
  }
});

// Character count display. Hook this up such that the setCharCount()
// method is called whenever an update is required.
//
CharCount = React.createClass ({
  getInitialState: function () { return {count: 0} },
  setCharCount: function (c) { this.setState ({count: c}) },
  render: function() {
    return (
        <div id="char-count">[{this.state.count}]</div>
    );
  }
});

// Container and SVG node for a program graphic. Expects a turtleSVG
// prop containing the dom children of an SVG tag.
// 
ProgramGraphic = React.createClass ({
  render: function() {
    return (
      <div
       dangerouslySetInnerHTML={
         {__html: '<svg id="program-graphic" viewbox="0,0,320,320">'
                  + this.props.turtleSVG 
                  + '</svg>'}
         }
      />
    );
  }
});

// Container and data flow for the combined input text area, character
// count, run button, and svg graphical display.
//
EvalAndDisplayWidget = React.createClass ({
  defaultProgramText: "#8{R45#4{#90{F(18/10)R2}R90}}",

  getInitialState: function() {
    return { turtleSVG: "", pgmText: "" }
  },
  setProgramTextAndEval: function (newProgramText) {
    var pgmUnmunged = removeZWB (newProgramText)
    var svgText = window.runProgramSVGBody (pgmUnmunged)
    this.setState ( { pgmText: pgmUnmunged
                    , turtleSVG: svgText } )
    this.refs['programInput'].getDOMNode().scrollIntoView(true)
  },
  setPTAndEFromProgramInput: function () {
    var pgmText = this.refs['programInput'].state.value
    this.setProgramTextAndEval (pgmText)

  },
  componentDidMount: function () {
    this.setProgramTextAndEval (this.defaultProgramText);
  },
  render: function() {
    return (
      <div id="main-widget-root-container">
        <ProgramInput ref="programInput"
                      value={this.state.pgmText}
                      charCountHook={function(l) {
                        this.refs['charCount'].setCharCount(l);
                      }.bind(this)}
                      />
        <div id="char-run-container">
          <CharCount ref="charCount" />
          <RunItButton
            onClick={this.setPTAndEFromProgramInput}
            onTouchStart={this.setPTAndEFromProgramInput}
          />
        </div>
        <ProgramGraphic turtleSVG={this.state.turtleSVG} />
      </div>
    );
  }
});


// --

// Expects a pgmText prop and a target prop
//
TryExample = React.createClass ({
  handleClick: function (e) {
    e.preventDefault ()
    this.props.target.setProgramTextAndEval (this.props.pgmText);
  },
  render: function() {
    return (
        <div className="try-example"
             onClick={this.handleClick}
             onTouchStart={this.handleClick}
             dangerouslySetInnerHTML={{__html: this.props.pgmText}}>
        </div>
    );
  }
});


ExamplesWidget = React.createClass ({
  render: function() {
    var brkSpcStrings = examplePgmStrings.map ( insertZWB )
    var rows = brkSpcStrings.map (
      function(s) { return <TryExample pgmText={s} target={eADWComponent} /> }
    )
    return (
      <div>
        {rows}
      </div>
    );
  }
});


eADWComponent = React.renderComponent
  ( <EvalAndDisplayWidget />,
    document.getElementById ('main-widget') 
  );

exWidgComponent = React.renderComponent 
  ( <ExamplesWidget />,
    document.getElementById ('examples-boxes') 
  );


// ----



function insertZWB (str) {
  var zeroWidthBrk = '\u200b'
  return str.split ('').
             filter ( function(c) {return c!=zeroWidthBrk} ).
             join (zeroWidthBrk)
}
function removeZWB (str) {
  var zeroWidthBrk = '\u200b'
  return str.replace (new RegExp(zeroWidthBrk,'g'), '')
}
