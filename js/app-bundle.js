(function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);throw new Error("Cannot find module '"+o+"'")}var f=n[o]={exports:{}};t[o][0].call(f.exports,function(e){var n=t[o][1][e];return s(n?n:e)},f,f.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s})({1:[function(require,module,exports){

function Dispatcher (eventsArray) {
  var d = { _eventRecipients: [] }

  eventsArray.forEach (function (evtFunc) {
    if (! ((typeof evtFunc === 'function') && evtFunc.eventName)) { 
      throw (new Error("Dispatcher constructor not handed an event function: "
                       + evtFunc))
    }
    d[evtFunc.eventName] = function () {
      var e = evtFunc.apply (null, arguments)
      d._eventRecipients.forEach (function (store) {
        if (store.handle[e.eventName]) {
          store.handle[e.eventName] (e)
        }
      })
      return e
    }
      
  })

  d.registerForEvents = function (target) {
    this._eventRecipients.push (target)
    return this._eventRecipients.length
  }

  return d
}

module.exports = Dispatcher

dbg = function () { console.log (arguments) }

},{}],2:[function(require,module,exports){

// We're following the react/flux convention here and calling this
// creature a "store." It seems to me that a store is exactly what is
// meant by "model" in traditional MVC. But that's okay. My guess is
// that the flux folks are emphasizing that applications often have
// multiple stores, but not a one-to-one mapping of stores to views.
//
// This is a fairly simple implementation of the flux pattern. The
// Store(), here, maintains a stateful model of our app
// world. Whenever we change some piece of state, we generally want to
// broadcast the change to one or more views. The way this machinery
// is written now, every registered view gets every state
// change. Views need a handleStateChange(newState) function to
// receive change broadcasts from us. There is duplication between the
// state we maintain, here, and the state that the view hold onto
// internally. That's not ideal -- it's worth thinking about how to do
// better within the React abstraction. On the other hand, React's
// idea that views hold onto relevant state internally will scale as
// we add loosely coupled view components that have specific
// responsibilities.
//
// All events we expect to handle will need functions defined under
// public.handle.<eventName>
//
// Other public methods
//
//   getState () - returns current model state
//
//   registerViewToSeeAllStateChanges (view) - add this view to the
//     list of objects to which we send all state changes
//


function Store () {
  var state = { pgmText: ""
              , pgmSVG: ""
              , pgmEvalProgress: -1
              }

  var public = {}       // closure-riffic containerization
  public.handle = {}    //   handle incoming ui events
  timers = {}           //   internally manage timers
  broadcastTargets = []      //   list of view targets we broadcast state changes to

  public.getState = function () {
    return state
  }


  //
  //  user interface event handlers (called by a dispatcher). these
  //  are custom for this application
  //

  public.handle.RunPgm = function (e) {
    if (hasTimerP ('pgm-eval')) {
      disposeOfTimer ('pgm-eval')
    }

    var pgmState = window.  // fix: turn the turtle-bundle into a real module
      startProgramRun (state.pgmText,
                       function () { // yield test
                         return !(this.instructionCount % 100)
                       })
    startTimerLoop (
      'pgm-eval', 0,
      function () {
        pgmState.continue ()
        if (pgmState.done) {
          setAndBroadcast ( { pgmSVG: pgmState.SVGBody(),
                         pgmEvalProgress: -1 } )
          disposeOfTimer ('pgm-eval')
        } else {
          state.pgmEvalProgress = pgmState.instructionCount
          setAndBroadcast ( { pgmEvalProgress: pgmState.instructionCount } )
          continueTimer ('pgm-eval')
        }
      })
  }

  public.handle.PgmTextChange = function (e) {
    setAndBroadcast ( { pgmText: e.newText } )
  }

  public.handle.SetPgmTextAndRunPgm = function (e) {
    setAndBroadcast ( { pgmText: e.newText } )
    public.handle.RunPgm ()
  }

  //
  // general timer machinery. this is plumbing code (in theory, not
  // custom to this application)
  // 

  startTimerLoop = function (name, interval, f) {
    if (timers[name]) { throw (new Error ("already have timer " + name)) }
    timers[name] = { f: f, interval: interval }
    continueTimer (name)
  }

  continueTimer = function (name) {
    var t = timers[name]
    if (!t) { throw (new Error ("don't know about timer " + name)) }
    t.id = window.setTimeout (t.f, t.interval)
  }
 
  disposeOfTimer = function (name) {
    var t = timers[name]
    if (!t) { throw (new Error ("don't know about timer " + name)) }
    window.clearTimeout (t.id)
    timers[name] = null;
  }

  hasTimerP = function (name) {
    return !!timers[name] // (return as a boolean)
  }


  // general machinery for broadcastting state changes to views. again,
  // plumbing code.
  //
  //   broadcast (key1, key2, key3) - send a state change message to the
  //     view targets with a new-states object constructed like so.
  //     { key1: state[key1], key2: state[key2], key3: state[key3] }
  //
  //   setAndBroadcast ( {key1: value1, ...} ) - set our own state fields,
  //     then send a state change message to the view targets.

  broadcast = function () {
    var stateKeys = Array.prototype.slice.call (arguments)
      var newS = {}
    stateKeys.forEach (function (k) {
      if (! state.hasOwnProperty(k)) { 
        throw (new Error ("unrecognized state key " + k)) 
      }
      newS[k] = state[k]
    })
    broadcastTargets.map (function (target) {
      target.handleStateChange (newS)
    })
  }

  setAndBroadcast = function (newS) {
    Object.keys(newS).forEach (function (k) {
      if (! state.hasOwnProperty(k)) { 
        throw (new Error ("unrecognized state key " + k)) 
      }
      state[k] = newS[k]
    })
    broadcastTargets.map (function (target) {
      target.handleStateChange (newS)
    })
  }

  public.registerViewToSeeAllStateChanges = function (view) {
    broadcastTargets.push (view)
  }

  // --

  return public
}

module.exports = Store

},{}],3:[function(require,module,exports){

// 
// All the machinery below is just a way to give us event creation
// functions that we can use as functions that belong to a Dispatcher
// object, rather than calling functions directly or slinging strings
// around.
//
//   dispatcher.PgmTextChange("foo")
//
// The jury is out on whether this is a useful layer of hiding and
// abstraction.
//
// An event created by one of the factories below looks like so.
//
// { eventType: 'USER_EVENT',
//   eventName: 'RunPgm' }
//
// The magic here is that the event functions themselves know their
// names. So UserEvents.RunPgm.PgmName is 'PgmName'. This is used in
// wiring up machinery in the Dispatcher.
//

exports.USER_EVENT = 'USER_EVENT'

var UserEvent = function () {}
UserEvent.eventType = exports.USER_EVENT

function RegisterUserEvent (eventName, eventFunc) {
  if (! eventFunc) {
    eventFunc = function () {return this}
  }
  var newFunc = function () {
    var e = Object.create(UserEvent,
                   { eventName: { value: eventName} })
    eventFunc.apply (e, arguments)
    return e
  }
  newFunc.eventName = eventName
  newFunc.eventType = exports.USER_EVENT
  exports[eventName] = newFunc  
}

var events = {
  RunPgm: null,

  PgmTextChange: function (newText) {
    this.newText = newText
  },

  SetPgmTextAndRunPgm: function (newText) {
    this.newText = newText
  }
}

Object.keys(events).map (function (k) {
  RegisterUserEvent(k, events[k])
})

function dbg (str) { console.log(str) }

},{}],4:[function(require,module,exports){
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
    el.style.width = width
    el.style.height = width
    console.log ("did mount", width)
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
// FIX: need to add back in the scroll-into-view stuff.
//
EvalAndDisplayWidget = exports.EvalAndDisplayWidget =
React.createClass ({displayName: 'EvalAndDisplayWidget',
  getInitialState: function() {
    return store.getState ()
  },

  handleStateChange: function (newStates) {
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

},{}],5:[function(require,module,exports){

var Evt = require ('./UserEvents.js')
var Dispatcher = require ('./Dispatcher.js')
var Store      = require ('./Store.js')
var Views      = require ('./Views.js')


var examplePgmStrings = [
  "^B(-35)0^d=(2/13)i=0#27{K((i\\4)+1)*(4/19)#180{FdR2}d=d*(11/10)^B(-d)0^i=i+1}"
  ,  "#10{#3{F45R120}R36}"
  , "^B(-39)(-30)K(2/3)^#24{F60R75}"
  , "#8{R45#4{#90{F(3/5)R2}R90}}"
  , "K(1/5)#36{R10#8{F18L45}}"
]


var dispatcher
  , store


var dispatcher = Dispatcher ([ Evt.PgmTextChange,
                               Evt.RunPgm,
                               Evt.SetPgmTextAndRunPgm ])
Views.setDispatcher (dispatcher)


var store = Store ()
dispatcher.registerForEvents (store)
Views.setStore (store)


var eADWComponent = React.renderComponent
  ( Views.EvalAndDisplayWidget ({}),
    document.getElementById ('main-widget') );

var exWidgComponent = React.renderComponent 
  ( Views.ExamplesWidget ({pgmStrings: examplePgmStrings}),
    document.getElementById ('examples-boxes') );

store.registerViewToSeeAllStateChanges (eADWComponent)


dispatcher.SetPgmTextAndRunPgm (examplePgmStrings[0])


console.log ("ready steady") 

},{"./Dispatcher.js":1,"./Store.js":2,"./UserEvents.js":3,"./Views.js":4}]},{},[5])