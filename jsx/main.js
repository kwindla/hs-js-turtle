/**
 * @jsx React.DOM
 *
 */

var ProgramInput, RunItButton, CharCount, ProgramGraphic,
    EvalAndDisplayWidget, TryExample, ExamplesWidget,
    eADWComponent, exWidgComponent;


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
    this.setState({ value: this.props.value
                  , charCountHook: this.props.charCountHook })
  },
  componentWillReceiveProps: function(nextProps) {
    this.setState({value: nextProps.value});
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
        <textarea value={value} onChange={this.handleChange} />
    );
  }
});

// Button to trigger program evaluation and graphic
// re-rendering. Expects a callback as an onClick prop.
//
RunItButton = React.createClass ({
  render: function() {
    return (
      <a onClick={this.props.onClick}>Run the Program!</a>
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
        <p>{this.state.count}</p>
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
  defaultProgramText: "Put your program here ...",

  getInitialState: function() {
    return { turtleSVG: "", pgmText: "" }
  },

  setProgramTextAndEval: function (newProgramText) {
    var svgText = window.runProgramSVGBody (newProgramText)
    this.setState ( { pgmText: newProgramText
                    , turtleSVG: svgText } )
  },
  setPTAndEFromProgramInput: function () {
    var pgmText = this.refs['programInput'].state.value
    this.setProgramTextAndEval (pgmText)
  },

  render: function() {
    return (
      <div id="main-widget-root-container">
        <ProgramInput ref="programInput" 
                      value={this.state.pgmText || this.defaultProgramText}
                      charCountHook={function(l) {
                        this.refs['charCount'].setCharCount(l);
                      }.bind(this)}
                      />
        <CharCount ref="charCount" />
        <RunItButton
          onClick={this.setPTAndEFromProgramInput}
          onTouchStart={this.setPTAndEFromProgramInput}
        />
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
        <div onClick={this.handleClick}
             onTouchStart={this.handleClick}>
          { this.props.pgmText }
        </div>
    );
  }
});


ExamplesWidget = React.createClass ({
  render: function() {
    return (
      <div>
        <TryExample pgmText="#4{F100R90}" target={eADWComponent} />
        <TryExample pgmText="#20{F10R5}" target={eADWComponent} />
        <TryExample pgmText="#24{F100R75}" target={eADWComponent} />
        <TryExample pgmText="#8{R45#6{#90{F1R2}R90}}" target={eADWComponent} />
        <TryExample pgmText="#36{R10#8{F25L45}}" target={eADWComponent} />
      </div>
    );
  }
});

// --

eADWComponent = React.renderComponent
  ( <EvalAndDisplayWidget />,
    document.getElementById ('main-widget') 
  );

exWidgComponent = React.renderComponent 
  ( <ExamplesWidget />,
    document.getElementById ('examples') 
  );

