/**
 * @jsx React.DOM
 *
 */


// 
// Two top-level user interface views.
//
//     EvalAndDisplayWidget - Program text input, run button, svg
//     display element and associated bits.
//
//     ExamplesWidget - Clickable list of example programs .
//
//   Usage example:
//
//     Views.setDispatcher (dispatcher)
//     Views.setStore (store)
//
//      var eADWComponent = React.renderComponent
//        ( Views.EvalAndDisplayWidget ({}),
//          document.getElementById ('main-widget') );
//
//      var exWidgComponent = React.renderComponent 
//        ( Views.ExamplesWidget ({}),
//          document.getElementById ('examples-boxes') );
//

var ProgramInput, RunItButton, CharCount, ProgramGraphic,
    EvalAndDisplayWidget, TryExample, ExamplesWidget,
    dispatcher, store

// --

React.initializeTouchEvents(true);

// --

exports.setDispatcher = function (disp) {
  dispatcher = disp
}

exports.setStore = function (sto) {
  store = sto
}

// --


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
ProgramInput = React.createClass ({displayName: 'ProgramInput',
  handleChange: function(event) {
    dispatcher.PgmTextChange (event.target.value)
  },
  render: function() {
    var value = this.props.pgmText;
    return (
        React.DOM.textarea( {id:"program-input",
                  value:value,
                  onChange:this.handleChange}
        )
    );
  }
});

// Button to trigger program evaluation and graphic
// re-rendering. Expects a callback as an onClick prop.
//
RunItButton = React.createClass ({displayName: 'RunItButton',
  render: function() {
    return (
      React.DOM.div( {id:"run-button",
        onClick:this.props.onClick}, "RUN THE PROGRAM >")
    );
  }
});

// Character count display. Expects a 'c' property that is the
// character count we're showing
//
CharCount = React.createClass ({displayName: 'CharCount',
  render: function() {
    return (
        React.DOM.div( {id:"char-count"}, "[",this.props.c,"]")
    );
  }
});

// Container and SVG node for a program graphic. Expects a turtleSVG
// prop containing the dom children of an SVG tag.
// 
ProgramGraphic = React.createClass ({displayName: 'ProgramGraphic',
  componentDidMount: function () {
    // having trouble getting svg element to be 1:1 aspect ratio,
    // scalable according to viewport size in vanilla svg, so dropping
    // back to javacript. FIX: need to catch window resize event and
    // recalc this. (Of course, the real fix is to do all this in
    // css.)
    var el = this.getDOMNode().parentNode
    var width = window.getComputedStyle(el).getPropertyValue('width')
    var prmel = document.getElementById('eval-progress-meter')
    el.style.width = width
    el.style.height = width
    if (prmel) { prmel.style.lineHeight = width }
    console.log ("did mount", width, prmel, width )
  },

  render: function() {
    return (
      React.DOM.div(
       {dangerouslySetInnerHTML:
         {__html: '<svg id="program-graphic" viewbox="0,0,100,100">'
                  +  this.props.turtleSVG 
                  + '</svg>'}
         }
      )
    );
  }
});


// Container and data flow for the combined input text area, character
// count, run button, and svg graphical display.
//
EvalAndDisplayWidget = exports.EvalAndDisplayWidget =
React.createClass ({displayName: 'EvalAndDisplayWidget',
  getInitialState: function() {
    return store.getState ()
  },

  handleStateChange: function (newStates) {
    var el
    if ((this.state.pgmEvalProgress === -1) &&
        (newStates.pgmEvalProgress !== -1)) {
      el = document.getElementById('graphic-container')
      if (el) { el.scrollIntoView() }
    }
    this.setState (newStates)
  },

  render: function() {
    var progressMeterVisibility =
      (this.state.pgmEvalProgress > -1) ? 'visible' : 'hidden'

    return (
      React.DOM.div( {id:"main-widget-root-container"}, 
        ProgramInput( {ref:"programInput", // FIX: remove
                      pgmText:this.state.pgmText}
                      ),
        React.DOM.div( {id:"char-run-container"}, 
          CharCount( {c:this.state.pgmText.length}),
          RunItButton(
            {onClick:dispatcher.RunPgm,
            onTouchEnd:dispatcher.RunPgm}
          )
        ),
        React.DOM.div( {id:"graphic-container"}, 
          ProgramGraphic( {turtleSVG:this.state.pgmSVG} ),
          React.DOM.div( {id:"eval-progress-meter",
               style: {visibility: progressMeterVisibility} }, 
            '[' + this.state.pgmEvalProgress + ']'
          )
        )
      )
    );
  }
});


// --

// Expects a pgmText prop and a target prop
//
TryExample = React.createClass ({displayName: 'TryExample',
  handleClick: function (e) {
    e.preventDefault ()
    dispatcher.SetPgmTextAndRunPgm (removeZWB(this.props.pgmText))
  },
  render: function() {
    return (
        React.DOM.div( {className:"try-example",
             onClick:this.handleClick,
             onTouchEnd:this.handleClick,
             dangerouslySetInnerHTML:{__html: this.props.pgmText}}
        )
    );
  }
});


exports.ExamplesWidget = ExamplesWidget =
React.createClass ({displayName: 'ExamplesWidget',
  render: function() {
    var i=0
    var brkSpcStrings = this.props.pgmStrings.map ( insertZWB )
    var rows = brkSpcStrings.map (
      function(s) { return TryExample( {key:i++, pgmText:s} ) }
    )
    return (
      React.DOM.div(null, 
        rows
      )
    );
  }
});

// --

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
