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

var svg = require ('./lib/TurtleSVG')


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

    var pgmState = svg.
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

},{"./lib/TurtleSVG":12}],3:[function(require,module,exports){

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
  ,  "K(1/5) a=(17/10) #19{X5(a*5)R5a=a+1}"
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

},{"./Dispatcher.js":1,"./Store.js":2,"./UserEvents.js":3,"./Views.js":4}],6:[function(require,module,exports){

// Quick and dirty Color class for use by TurtlePrimitives.js
// Import as:
//
//   var Color = require ('./Color')
//
// Expects 1-99 values to be passed as arguments to constructor rather
// than 0-255 (to save characters in graphics programs). Indexed
// colors list is from the svg documentation.
// http://www.w3.org/TR/SVG/types.html#ColorKeywords

var namedColorsLookupTable

function Color (r, g, b, a) {
  var d = 255/99

  c = Object.create ( 
    { r: 0, g: 0, b: 0, a: 0
      , setRGB: function (R,G,B) { this.r=R*d; this.g=G*d; this.b=B*d }
      , setRGBFromBytes: function (R,G,B) { this.r=R; this.g=G, this.b=B }
      , toString: function () {
          return 'rgba(' + [this.r, this.g, this.b, this.a].
                    map(function(c){return Math.round(c)}).join(',') + ')' }
      , toStringNoAlpha: function () {
          return 'rgb(' + [this.r, this.g, this.b].
                    map(function(c){return Math.round(c)}).join(',') + ')' }
      , setFromIndex: function (idx) {
        return namedColorsLookupTable[idx][1](this)
      }
    }
  )
  if (r) { c.r = r*d }
  if (g) { c.g = g*d }
  if (b) { c.b = b*d }
  if (a) { c.a = a/99}

  return c
}


var rgb = function (R, G, B) {
  return function (color) {color.setRGBFromBytes(R,G,B)}
}

namedColorsLookupTable =
  [ ['aliceblue', rgb(240, 248, 255)]
  , ['antiquewhite', rgb(250, 235, 215)]
  , ['aqua', rgb( 0, 255, 255)]
  , ['aquamarine', rgb(127, 255, 212)]
  , ['azure', rgb(240, 255, 255)]
  , ['beige', rgb(245, 245, 220)]
  , ['bisque', rgb(255, 228, 196)]
  , ['black', rgb( 0, 0, 0)]
  , ['blanchedalmond', rgb(255, 235, 205)]
  , ['blue', rgb( 0, 0, 255)]
  , ['blueviolet', rgb(138, 43, 226)]
  , ['brown', rgb(165, 42, 42)]
  , ['burlywood', rgb(222, 184, 135)]
  , ['cadetblue', rgb( 95, 158, 160)]
  , ['chartreuse', rgb(127, 255, 0)]
  , ['chocolate', rgb(210, 105, 30)]
  , ['coral', rgb(255, 127, 80)]
  , ['cornflowerblue', rgb(100, 149, 237)]
  , ['cornsilk', rgb(255, 248, 220)]
  , ['crimson', rgb(220, 20, 60)]
  , ['cyan', rgb( 0, 255, 255)]
  , ['darkblue', rgb( 0, 0, 139)]
  , ['darkcyan', rgb( 0, 139, 139)]
  , ['darkgoldenrod', rgb(184, 134, 11)]
  , ['darkgray', rgb(169, 169, 169)]
  , ['darkgreen', rgb( 0, 100, 0)]
  , ['darkgrey', rgb(169, 169, 169)]
  , ['darkkhaki', rgb(189, 183, 107)]
  , ['darkmagenta', rgb(139, 0, 139)]
  , ['darkolivegreen', rgb( 85, 107, 47)]
  , ['darkorange', rgb(255, 140, 0)]
  , ['darkorchid', rgb(153, 50, 204)]
  , ['darkred', rgb(139, 0, 0)]
  , ['darksalmon', rgb(233, 150, 122)]
  , ['darkseagreen', rgb(143, 188, 143)]
  , ['darkslateblue', rgb( 72, 61, 139)]
  , ['darkslategray', rgb( 47, 79, 79)]
  , ['darkslategrey', rgb( 47, 79, 79)]
  , ['darkturquoise', rgb( 0, 206, 209)]
  , ['darkviolet', rgb(148, 0, 211)]
  , ['deeppink', rgb(255, 20, 147)]
  , ['deepskyblue', rgb( 0, 191, 255)]
  , ['dimgray', rgb(105, 105, 105)]
  , ['dimgrey', rgb(105, 105, 105)]
  , ['dodgerblue', rgb( 30, 144, 255)]
  , ['firebrick', rgb(178, 34, 34)]
  , ['floralwhite', rgb(255, 250, 240)]
  , ['forestgreen', rgb( 34, 139, 34)]
  , ['fuchsia', rgb(255, 0, 255)]
  , ['gainsboro', rgb(220, 220, 220)]
  , ['ghostwhite', rgb(248, 248, 255)]
  , ['gold', rgb(255, 215, 0)]
  , ['goldenrod', rgb(218, 165, 32)]
  , ['grey', rgb(128, 128, 128)]
  , ['green', rgb( 0, 128, 0)]
  , ['greenyellow', rgb(173, 255, 47)]
  , ['honeydew', rgb(240, 255, 240)]
  , ['hotpink', rgb(255, 105, 180)]
  , ['indianred', rgb(205, 92, 92)]
  , ['indigo', rgb( 75, 0, 130)]
  , ['ivory', rgb(255, 255, 240)]
  , ['khaki', rgb(240, 230, 140)]
  , ['lavender', rgb(230, 230, 250)]
  , ['lavenderblush', rgb(255, 240, 245)]
  , ['lawngreen', rgb(124, 252, 0)]
  , ['lemonchiffon', rgb(255, 250, 205)]
  , ['lightblue', rgb(173, 216, 230)]
  , ['lightcoral', rgb(240, 128, 128)]
  , ['lightcyan', rgb(224, 255, 255)]
  , ['lightgoldenrodyellow', rgb(250, 250, 210)]
  , ['lightgray', rgb(211, 211, 211)]
  , ['lightgreen', rgb(144, 238, 144)]
  , ['lightgrey', rgb(211, 211, 211)]
  , ['lightpink', rgb(255, 182, 193)]
  , ['lightsalmon', rgb(255, 160, 122)]
  , ['lightseagreen', rgb( 32, 178, 170)]
  , ['lightskyblue', rgb(135, 206, 250)]
  , ['lightslategray', rgb(119, 136, 153)]
  , ['lightslategrey', rgb(119, 136, 153)]
  , ['lightsteelblue', rgb(176, 196, 222)]
  , ['lightyellow', rgb(255, 255, 224)]
  , ['lime', rgb( 0, 255, 0)]
  , ['limegreen', rgb( 50, 205, 50)]
  , ['linen', rgb(250, 240, 230)]
  , ['magenta', rgb(255, 0, 255)]
  , ['maroon', rgb(128, 0, 0)]
  , ['mediumaquamarine', rgb(102, 205, 170)]
  , ['mediumblue', rgb( 0, 0, 205)]
  , ['mediumorchid', rgb(186, 85, 211)]
  , ['mediumpurple', rgb(147, 112, 219)]
  , ['mediumseagreen', rgb( 60, 179, 113)]
  , ['mediumslateblue', rgb(123, 104, 238)]
  , ['mediumspringgreen', rgb( 0, 250, 154)]
  , ['mediumturquoise', rgb( 72, 209, 204)]
  , ['mediumvioletred', rgb(199, 21, 133)]
  , ['midnightblue', rgb( 25, 25, 112)]
  , ['mintcream', rgb(245, 255, 250)]
  , ['mistyrose', rgb(255, 228, 225)]
  , ['moccasin', rgb(255, 228, 181)]
  , ['navajowhite', rgb(255, 222, 173)]
  , ['navy', rgb( 0, 0, 128)]
  , ['oldlace', rgb(253, 245, 230)]
  , ['olive', rgb(128, 128, 0)]
  , ['olivedrab', rgb(107, 142, 35)]
  , ['orange', rgb(255, 165, 0)]
  , ['orangered', rgb(255, 69, 0)]
  , ['orchid', rgb(218, 112, 214)]
  , ['palegoldenrod', rgb(238, 232, 170)]
  , ['palegreen', rgb(152, 251, 152)]
  , ['paleturquoise', rgb(175, 238, 238)]
  , ['palevioletred', rgb(219, 112, 147)]
  , ['papayawhip', rgb(255, 239, 213)]
  , ['peachpuff', rgb(255, 218, 185)]
  , ['peru', rgb(205, 133, 63)]
  , ['pink', rgb(255, 192, 203)]
  , ['plum', rgb(221, 160, 221)]
  , ['powderblue', rgb(176, 224, 230)]
  , ['purple', rgb(128, 0, 128)]
  , ['red', rgb(255, 0, 0)]
  , ['rosybrown', rgb(188, 143, 143)]
  , ['royalblue', rgb( 65, 105, 225)]
  , ['saddlebrown', rgb(139, 69, 19)]
  , ['salmon', rgb(250, 128, 114)]
  , ['sandybrown', rgb(244, 164, 96)]
  , ['seagreen', rgb( 46, 139, 87)]
  , ['seashell', rgb(255, 245, 238)]
  , ['sienna', rgb(160, 82, 45)]
  , ['silver', rgb(192, 192, 192)]
  , ['skyblue', rgb(135, 206, 235)]
  , ['slateblue', rgb(106, 90, 205)]
  , ['slategray', rgb(112, 128, 144)]
  , ['slategrey', rgb(112, 128, 144)]
  , ['snow', rgb(255, 250, 250)]
  , ['springgreen', rgb( 0, 255, 127)]
  , ['steelblue', rgb( 70, 130, 180)]
  , ['tan', rgb(210, 180, 140)]
  , ['teal', rgb( 0, 128, 128)]
  , ['thistle', rgb(216, 191, 216)]
  , ['tomato', rgb(255, 99, 71)]
  , ['turquoise', rgb( 64, 224, 208)]
  , ['violet', rgb(238, 130, 238)]
  , ['wheat', rgb(245, 222, 179)]
  , ['white', rgb(255, 255, 255)]
  , ['whitesmoke', rgb(245, 245, 245)]
  , ['yellow', rgb(255, 255, 0)]
  , ['yellowgreen', rgb(154, 205, 50)]
  ]


module.exports = Color

},{}],7:[function(require,module,exports){

var _st = require ('./SymbolTable')

exports.evaluate = function (exprl, symbolTable, extraFields, yieldTest) {
  var s = EvalState (symbolTable, extraFields, yieldTest)

  var inner_eval = function () {
    var e0 = exprl.shift ()
    if (!e0) { return { done: true } }
    return _evaluate (e0, s,
                      function (v) { s._values.push (v);
                                     return inner_eval })
  }
  // dbg ('calling continue')
  s.k = inner_eval
  s.done = false
  return s
}


function EvalState (symbolTable, extraFields, yieldTest) {
  var s = {
    _values:   [],        // output values from top-level expression list
    instructionCount: 0,

    eval: _evaluate,
    values: function () { return this._values },
    push: function (x) { return this.stack.push(x) },
    pop: function () { return this.stack.pop() }
  }

  s.symTab = symbolTable
  s.yieldTest = yieldTest || function () { return false }

  s.continue = function () {
    var next = this.k
    if (! next) { throw ("don't know how to continue (no k)") }

    while (true) {
      // dbg ("top of while")
      // dbg (next)
      // dbg (this)
      next = next ()
      if (next.done) {
        this.done = true
        return this
      }
      if (this.yieldTest ()) {
        this.done = false;
        this.k = next
        return this
      }
    }
  }


  Object.keys(extraFields).forEach (
    function (k) { s[k] = extraFields[k] }
  )
  return s
}


function _evaluate (expr, s, k) {
  var f = EvalTable[expr.type]
  // dbg ('_e ' + expr.type)
  if (! f) { throw 'unkown expression type - ' + expr.type }
  s.instructionCount++
  // here, rather than returning simply 'f (expr, s, k)', we wrap this
  // call in a lambda, making a top-level point that always returns to
  // the continue() routine. doing this simplifies the implementation
  // of all of the eval routines in the table, below, such that we
  // don't need to wrap every tail call we make to _evaluate. the cost
  // is that our yieldTest will get called twice for "simple" machine
  // instructions like Symbol and ConstantNumber. if we were
  // optimizing, we might choose to fix this.
  return function () { return f (expr, s, k) }
}



// pre-CPS commit 7235c300f0b95322db85c063851b92e67be70a3b


// FIX: RESULT is a global!
function eval_rpt (c, s, k, f, idx) {
  if (!idx) { idx = 0 }
      if (c > 0) {
        return f (
          function (v) { 
            result = v; return eval_rpt (c-1, s, k, f, idx+1) 
          },
          idx)
      } else {
        return k (result)
      }
    }



var EvalTable = {
  Assignment: function (e, s, k) {
    // var value = this.evaluate (e.e)
    // this.symTab.updateBindingForValue (e.v, value)
    // return value
    return _evaluate (e.e, s,
                 function (v) {
                   s.symTab.updateBindingForValue (e.v, v)
                   return k (v)
                 })
  },
  Symbol: function (e, s, k) {
    // var binding = this.symTab.retrBinding (e.v)
    // return binding.v
    // dbg ('in s : ' + k)
    return k (s.symTab.retrBinding(e.v).v)
  },
  Defun: function (e, s, k) {
    // here we can do things differently than on the haskell side
    // because we have references and mutable objects. we'll save our
    // current symbol table ref to use when we're funcall'ed, and
    // insert a new derived table to protect this saved symbol table
    // from getting new entries otherwise
    // this.symTab.updateBindingForDefun (e.v, e.arity, e.e, this.symTab)
    // this.symTab = this.symTab.derivedTable ()
    // return e.arity
    s.instructionCount++
    s.symTab.updateBindingForDefun (e.v, e.arity, e.e, s.symTab)
    s.symTab = s.symTab.derivedTable ()
    return k (e.arity)
  },
  Funcall: function (e, s, k) {
    var binding
    s.instructionCount++

    binding = s.symTab.retrBinding (e.v)
    if (binding.type == 'BoundBuiltin') {
      return binding.f (e.exprl, s, k)
    }

    var  callTimeSymTab = s.symTab,
      funcEvalSymTab,
      args,
      retval
    if (binding.type == 'BoundDefun') {
      funcEvalSymTab = binding.symTab.derivedTable ()      
      argLabels = "abcdefghijklmnopqrstuvwxyz"
      count = 0
      function eval_args () {
        if (count < e.exprl.length) {
          s.instructionCount++
          return _evaluate (
            e.exprl[count], s,
            function (v) { funcEvalSymTab[argLabels.charAt(count)] =
                           _st.BoundValue (v) 
                           count++
                           return eval_args
                         })
        } else {
          s.instructionCount++
          return callk ()
        }
      }
      return eval_args ()

      function callk () {
        s.symTab = funcEvalSymTab
        return _evaluate (binding.e, s,
                          function (v) {
                            s.symTab = callTimeSymTab
                            return k (v)
                          })
      }
    }
    // plan of action: derive a symbol table from our saved
    // defun-scope symbol table (which, remember, is just a parent
    // of the calling scope's table), make entries in that new table
    // for the function's arguments, swap in that symbol table, eval
    // the function body, put back the symbol table from the calling
    // scope, and return
  },
  BinaryOp: function (e, s, k) {
    return _evaluate (e.left, s,
               function (l) {
                 return _evaluate (e.right, s,
                                   function (r) { return k (e.f(l,r)) }
                 )
               })
  },
  UnaryOp: function (e, s, k) {
    return _evaluate (e.e, s, function (v) { return k ( e.f(v)) })
  },
  ConstantNumber: function (e, s, k) { 
    return k (e.v)
  },
  TernaryIf: function (e, s, k) {
    return _evaluate (e.econd, s,
                      function (v) {
                        if (v !=0) {
                          return _evaluate (e.eif, s,
                                            function (v) { return k (v) })
                        } else {
                          return _evaluate (e.ethen, s,
                                            function (v) { return k (v) })
                 }
               })
  },
  ExprTreeListNode: function (e, s, k) {
    var outerSymTab = s.symTab,
        result,
        eval_list,
        count
    s.symTab = s.symTab.derivedTable ()
    count = 0
    return function eval_list () {
      if (count < e.exprl.length) {
        return _evaluate (e.exprl[count++], s,
                          function (v) { result=v; return eval_list })
      } else {
        s.symTab = outerSymTab
        return k (result)
      }
    }
  },
  Repeat: function (e, s, k) {
    // need to handle expression list blocks differently from bare
    // expressions, here for a block, we want to repeat multiple times
    // with a stateful symbol table across all repeats, then "pop" that
    // symbol table and throw it away.


    function eval_single_expr (ntimes, k) {
      return eval_rpt (ntimes, s, k,
                       function (rpt_k) { return _evaluate (e.e, s, rpt_k) })
    }

    function eval_expr_list (ntimes, k) {
      var outerSymTab = s.symTab
      s.symTab = s.symTab.derivedTable ()
      return eval_rpt (
        ntimes, s, function(v) { s.symTab = outerSymTab; return k(v) },
        function (rpt_k_loop) {
          return eval_rpt (e.e.exprl.length, s, rpt_k_loop,
                           function (rpt_k_eval, i) {
                              return _evaluate (e.e.exprl[i], s, rpt_k_eval)
                           })
        })
     }

    return _evaluate (e.ntimes, s,
                      function (ntimes) {
                        if (e.e.is ('ExprTreeListNode')) {
                          return eval_expr_list (ntimes, k)
                        } else {
                          return eval_single_expr (ntimes, k)
                        }
                      })
  }
}



//

function dbg (s) { console.log(s) }




},{"./SymbolTable":9}],8:[function(require,module,exports){

exports.parse = parse


var tp = require('./TurtlePrimitives')


var ParseState = {
  tokens: undefined,    // these will be set by arguments to parse ()
  symTab: undefined,
  expressionList:       _expressionList,
  expression:           _expression,
  term:                 _term,
  expressionTail:       _expressionTail,
  comparison:           _comparison,
  factor:               _factor,
  factorExpressionList: _factorExpressionList,
  termTail:             _termTail
}


function parse (tokens, symbolTable) {
  var state = Object.create (ParseState,
                             { tokens: {value: tokens},
                               symTab: {value: symbolTable} })
  return _expressionList.apply (state)
}

function dbg (s) { console.log(s) }


function _expressionList () {
  if (this.tokens.length == 0) { return [] }
  return [this.expression()].concat (this.expressionList())
}

function _expression () {
  var t0 = this.tokens[0],
      t1 = this.tokens[1],
      t2 = this.tokens[2];
  if (t0 && t1 && t0.is('TokenSymbol') && t1.is('TokenEquals')) {
    this.tokens.shift(); this.tokens.shift()
    return Assignment (t0.v, this.expression())
  } else if (t0 && t0.is('TokenDefun')) {
    if (! (t1 && t1.is('TokenSymbol'))) {
      throw ("defun token '&' should be followed by a symbol")
    }
    if (! (t2 && t2.is('TokenNumber'))) {
      throw ("defun token '&' should be followed by a symbol and a number")
    }
    this.tokens.shift(); this.tokens.shift(); this.tokens.shift()
    this.symTab.updateBindingForDefun (t1.v, +t2.v)
    return Defun (t1.v, +t2.v, this.expression())
  } else {
    return this.comparison( this.expressionTail (this.term()))
  }
  // fix: should there be something here? an error or explicit return
}

function _term () {
  return this.termTail (this.factor())
}

function _expressionTail (expr) {
  var t0 = this.tokens[0]
  if (t0 && t0.is('TokenOperator')) {
    if (t0.v == 'Plus') {
      this.tokens.shift()
      return this.expressionTail (BinaryOp ('+', function(a,b){return a+b},
                                            expr, this.term()) )
    } else if (t0.v == 'Minus') {
      this.tokens.shift()
      return this.expressionTail (BinaryOp ('-', function(a,b){return a-b},
                                            expr, this.term()) )
    }
  } 
  return expr
}

function _comparison (expr) {
  var t0 = this.tokens[0]
  if (t0 && t0.is('TokenOperator')) {
    if (t0.v == 'GreaterThan') {
      this.tokens.shift()
      return this.expressionTail (BinaryOp ('>', function(a,b){return a>b},
                                            expr, this.term()) )
    } else if (t0.v == 'LessThan') {
      this.tokens.shift()
      return this.expressionTail (BinaryOp ('<', function(a,b){return a<b},
                                            expr, this.term()) )
    } else if (t0.v == 'Equals') {
      this.tokens.shift()
      return this.expressionTail (BinaryOp ('=', function(a,b){return a===b},
                                            expr, this.term()) )
    }

  } 
  return expr
}

function _factor () {
  var t0 = this.tokens.shift()
  var t1, expr, exprl, binding
  var exprl = []
  if (t0.is('TokenNumber')) { return ConstantNumber (t0.v) }
  if (t0.is('TokenSymbol')) {
    binding = this.symTab.retrBinding (t0.v)
    if (binding.type == 'BoundValue') {
      return Symbol (t0.v)
    } else if ((binding.type=='BoundBuiltin') ||
               (binding.type=='BoundDefun')) {
      for (var i=0; i<binding.arity; i++) { exprl.push (this.expression()) }
      return Funcall (binding.arity, t0.v, exprl)
    } else { throw 'unknown binding type - ' + binding.type }
  }
  if (t0.is('TokenLeftParen')) {
    expr = this.expression ()
    t1 = this.tokens.shift()
    if (! t1.is('TokenRightParen')) {
      throw ("expected right paren, not - " + t1.inspect())
    }
    return expr
  }
  if (t0.is('TokenLeftBrace')) {
    var outerSymTab = this.symTab
    this.symTab = this.symTab.derivedTable ()
    exprl = this.factorExpressionList()
    this.symTab = outerSymTab
    return ExprTreeListNode (exprl);
  }
  if (t0.is('TokenIf')) {
    return TernaryIf (this.expression(), this.expression(), this.expression());
  }
  if (t0.is('TokenRepeat')) {
    return Repeat (this.expression(), this.expression())
  }
  if (t0.is('TokenOperator') && (t0.v == 'Plus')) { return this.expression() }
  if (t0.is('TokenOperator') && (t0.v == 'Minus')) {
    return UnaryOp ('-', function(v) { return -(v) }, this.expression())
  }
  if (t0.is('TokenOperator') && (t0.v == 'Not')) {
    return UnaryOp ('!', function(v) { return !(v) }, this.expression())
  }
  throw ("incomplete pattern match in factor :) - " + t0.inspect())
}

function _factorExpressionList () {
  if (! this.tokens.length) {
    throw "unexpected end of token stream inside {braces}"
  }
  if (this.tokens[0].is('TokenRightBrace')) {
    this.tokens.shift()
    return []
  }
  return [this.expression()].concat (this.factorExpressionList())
}

function _termTail (expr) {
  var t0 = this.tokens[0]
  if (t0 && t0.is('TokenOperator')) {
    if (t0.v == 'Times') {
      this.tokens.shift()
      return this.expressionTail (BinaryOp ('*', function(a,b){return a*b},
                                            expr, this.term()) )
    } else if (t0.v == 'Div') {
      this.tokens.shift()
      return this.expressionTail (BinaryOp ('/', function(a,b){return a/b},
                                            expr, this.term()) )
    } else if (t0.v == 'Mod') {
      this.tokens.shift()
      return this.expressionTail (BinaryOp ('\\', function(a,b){return a%b},
                                            expr, this.term()) )
    }
  } 
  return expr
}


//


var ExprNode = {
  type: null,
  is:      function (s) { return s === this.type },
  inspect: function () { return (this.type + (this.v ? (' ' + this.v) : '')) }
}


function ConstantNumber (n) {
  return Object.create (ExprNode, { type: {value: 'ConstantNumber'},
                                    v: {value: n} } )
}

function Symbol (c) {
  return Object.create (ExprNode, { type: {value: 'Symbol'},
                                    v: {value: c},
                                    inspect: { value:
    function () { 
      return (this.type + (this.v ? (" '" + this.v + "'") : ''))
    }}})
}

function Defun (c, arity, expr) {
  return Object.create (ExprNode, { type: {value: 'Defun'},
                                    v: {value: c},
                                    arity: {value: arity},
                                    e: {value: expr},
                                    inspect: { value:
    function () {
      return (this.type + " '" + this.v + "' " + this.arity +
              ' (' + this.e.inspect() + ')')
    } } })
}

function Funcall (arity, sym, exprl) {
  return Object.create (ExprNode, { type: {value: 'Funcall'},
                                    v: {value: sym}, 
                                    exprl: {value: exprl}, // args list
                                    inspect: {value:
    function () {
      return (this.type + ' ' + arity + " '" + this.v + "' " + 
                '['  +
              this.exprl.map ( function(e){return e.inspect()} ).join(', ') +
                ']')
    } } })
}

function Assignment (sym, expr) {
  return Object.create (ExprNode, { type: {value: 'Assignment'},
                                    v: {value: sym},
                                    e: {value: expr},
                                    inspect: {value:
    function () {
      return (this.type + " '" + this.v + "' (" + this.e.inspect() + ')')
    } } })
}

function BinaryOp (s, f, left, right) {
  return Object.create (ExprNode, { type: {value: 'BinaryOp'},
                                    v: {value: s},
                                    f: {value: f},
                                    left: {value: left},
                                    right: {value: right},
                                    inspect: { value:
    function () {
      return (this.type + ' (' + this.v + ') (' + this.left.inspect() + ')' +
                                           ' (' + this.right.inspect() + ')' )
    } } })
}

function UnaryOp (s, f, expr) {
  return Object.create (ExprNode, { type: {value: 'UnaryOp'},
                                    v: {value: s},
                                    f: {value: f},
                                    e: {value: expr},
                                    inspect: { value:
    function () {
      return (this.type + ' (' + this.v + ') (' + this.e.inspect() + ')')
    } } })
}

function TernaryIf (econd, eif, ethen) {
  return Object.create (ExprNode, { type:    {value: 'TernaryIf'},
                                    econd:   {value: econd},
                                    eif:     {value: eif},
                                    ethen:   {value: ethen},
                                    inspect: { value:
    function () {
      return (this.type + ' (' + this.econd.inspect() + ') ' +
              '(' + this.eif.inspect() + ') ' +
              '(' + this.ethen.inspect() + ')' )
    } } })
}

function ExprTreeListNode (exprl) {
  return Object.create (ExprNode, { type: {value: 'ExprTreeListNode'},
                                    exprl: {value: exprl},
                                    inspect: { value:
    function () {
      return (this.type + ' ' + ' [' +
              this.exprl.map ( function(e){return e.inspect()} ).join(', ') + 
              ']')
    } } })
}

function Repeat (ntimes, expr) {
  return Object.create (ExprNode, { type: {value: 'Repeat'},
                                    ntimes: {value: ntimes},
                                    e: {value: expr},
                                    inspect: {value:
    function () {
      return (this.type + ' (' + this.ntimes.inspect() + ') ' +
              '(' + this.e.inspect() + ')')
    } } })
}

},{"./TurtlePrimitives":11}],9:[function(require,module,exports){

exports.BaseSymbolTable = BaseSymbolTable
exports.BoundValue = BoundValue
exports.BoundBuiltin = BoundBuiltin

var Binding = {
  inspect: function () { return this.type + ' ' + this.v }
}

var SymbolTable = {
  parent: null,

  retrBinding:           _retrBinding,
  updateBindingForValue: _updateBindingForValue,
  updateBindingForDefun: _updateBindingForDefun,

  derivedTable: _derivedTable,
}

function BoundValue (v) {
  return Object.create (Binding, { type: {value: 'BoundValue'},
                                   v:    {value: v} })
}

function BoundBuiltin (arity, f) {
  return Object.create (Binding,  { type:  {value: 'BoundBuiltin'},
                                    arity: {value: arity},
                                    f:     {value: f},
                                    v:     {value: arity} // for inspect
                                   })
}

function BoundDefun (arity, expr, symtab) {
  return Object.create (Binding, { type:   {value: 'BoundDefun'},
                                   arity:  {value: arity},
                                   e:      {value: expr},
                                   symTab: {value: symtab},
                                   v:      {value: arity} // for inspect
                                 })
}

function BaseSymbolTable () {
  return Object.create (SymbolTable)
}

function _retrBinding (sym) {
  // we'll use the js inheritance chain to do our lookups. easy-peasy.
  return this[sym] || BoundValue (0)
}

function _updateBindingForValue (sym, v) {
  _setBinding (this, sym, BoundValue (v))
  return v
}

function _updateBindingForDefun (sym, arity, expr, symtab) {
  // dbg ('defun st ' + sym + ' - ' + arity)
  _setBinding (this, sym, BoundDefun (arity, expr, symtab))
  return arity   // FIX: is this return value correct (and do we use it)?
}

var util = require('util')

function _setBinding (symTab, sym, binding) {
  if (! lookUpward (symTab)) {
    // dbg ('not found - setting in leaf table')
    symTab[sym] = binding
  }
  return

  function lookUpward (symTab) {
    // dbg ('lU: ' + util.inspect(symTab))
    if (!symTab) { return false }
    if (symTab.hasOwnProperty(sym)) {
      // dbg ('    found ' + sym + ' ' + util.inspect(symTab[sym]))
      symTab[sym] = binding
      return true
    } else {
      // dbg ('    recursing')
      return lookUpward (symTab.parent)
    }
  }
}

function _derivedTable () {
  var newST = Object.create (this)
  newST.parent = this
  return newST
}



function dbg (s) { console.log(s) }

},{"util":16}],10:[function(require,module,exports){

exports.tokenize = tokenize


var TokenCases = [
  // isSpace case is handled by a trim down in the driver function
  [ /^[a-zA-Z\^\%]/, function (m) { return TokenSymbol (m[0]) } ],  
  [ '=', TokenEquals ],
  [ '(', TokenLeftParen ],
  [ ')', TokenRightParen ],
  [ '{', TokenLeftBrace ],
  [ '}', TokenRightBrace ],
  [ '+', TokenOperator, 'Plus' ],
  [ '-', TokenOperator, 'Minus' ],
  [ '*', TokenOperator, 'Times' ],
  [ '/', TokenOperator, 'Div' ],
  [ '\\', TokenOperator, 'Mod' ],
  [ '~', TokenOperator, 'Equals' ],
  [ '!', TokenOperator, 'Not' ],
  [ '>', TokenOperator, 'GreaterThan' ],
  [ '<', TokenOperator, 'LessThan' ],
  [ '&', TokenDefun ],
  [ '?', TokenIf ],
  [ '#', TokenRepeat ],
  [ /^[0-9]+/,   function (m) { return TokenNumber (+m[0]) } ]
]


function tokenize (stream) {
  var r = {}
  stream = stream.replace (/^\s+/,'')
  if (! stream) {
    return []
  }
  r.stream = stream
  if (! TokenCases.some (tryCase, r)) {
    throw "could not tokenize: " + stream
  }

  
  return [r.result.token].concat(tokenize(r.result.stream))
}


function tryCase (tcase) {
  if (r = tryLiteral(this.stream,tcase) || tryRegExp(this.stream,tcase)) {
    this.result = r
    return true
  }
  return false
}

function tryLiteral (stream, tcase) {
  var cas = tcase[0]
  if (stream.indexOf(cas) == 0) {  // duck type, here (test only
                                   // succeeds if cas is a string)
    return { token: tcase[1](tcase.slice(2)),
             stream: stream.substr(cas.length) }
  }
  return false;
}

function tryRegExp (stream, tcase) {
  var cas = tcase[0]
  var m
  if (cas.test && (m=stream.match(cas))) {
    var matched_chars = m[0]
    var token = tcase[1](m)
    return { token: token, stream: stream.substr(matched_chars.length) }
  }
  return false;
}

//

var Token = {
  type:    null,
  is:      function (s) { return s === this.type },
  inspect: function () { return (this.type + (this.v ? (' ' + this.v) : '')) }
}

function TokenSymbol (c) {
  return Object.create (Token, { type: {value: 'TokenSymbol'},
                                 v:    {value: c },
                                 inspect: { value:
    function () { 
      return (this.type + (this.v ? (" '" + this.v + "'") : ''))
    }}})
}

function TokenNumber (num) {
  return Object.create (Token, { type: {value: 'TokenNumber'},
                                 v:    {value: num } })
}

function TokenEquals (c) {
  return Object.create (Token, { type: {value: 'TokenEquals'} })
}

function TokenLeftParen () {
  return Object.create (Token, { type: {value: 'TokenLeftParen'} })
}

function TokenRightParen () {
  return Object.create (Token, { type: {value: 'TokenRightParen'} })
}

function TokenLeftBrace () {
  return Object.create (Token, { type: {value: 'TokenLeftBrace'} })
}

function TokenRightBrace () {
  return Object.create (Token, { type: {value: 'TokenRightBrace'} })
}

function TokenOperator (op) {
  return Object.create (Token, { type: {value: 'TokenOperator'},
                                 v: {value: op} })
}

function TokenDefun (c) {
  return Object.create (Token, { type: {value: 'TokenDefun'} })
}

function TokenIf (c) {
  return Object.create (Token, { type: {value: 'TokenIf'} })
}

function TokenRepeat (c) {
  return Object.create (Token, { type: {value: 'TokenRepeat'} })
}
 


},{}],11:[function(require,module,exports){

exports.InitialSymbolTable = InitialSymbolTable
exports.Turtle = Turtle


var st = require ('./SymbolTable')
var Color = require ('./Color')

var TurtleGlobals = {
  // for functions defined here, "this" will be the current
  // EvalState. e is the passed-in exprl of args to the function
    P: st.BoundValue (Math.PI)
  , F: st.BoundBuiltin (1, _Forward)
  , R: st.BoundBuiltin (1, _RotateRight)
  , L: st.BoundBuiltin (1, _RotateLeft)

  , M: st.BoundBuiltin (2, _MoveTo)
  , B: st.BoundBuiltin (2, _MoveBy)
  , T: st.BoundBuiltin (1, _TurnTo)
  , H: st.BoundBuiltin (1, _TurnBy)

  , A: st.BoundBuiltin (1, _Alpha)
  , C: st.BoundBuiltin (3, _Color)
  , D: st.BoundBuiltin (1, _IndexedColor)
  , I: st.BoundBuiltin (3, _FillColor)
  , J: st.BoundBuiltin (1, _IndexedFillColor)

  , '^': st.BoundBuiltin (0, _PenToggle)
  , U: st.BoundBuiltin (0, _PenUp)
  , N: st.BoundBuiltin (0, _PenDown)
  , K: st.BoundBuiltin (1, _StrokeWidth)

  , X: st.BoundBuiltin (2, _Box)
}


function InitialSymbolTable () {
  var symTab = st.BaseSymbolTable ()
  Object.keys(TurtleGlobals).forEach (
    function (k) { symTab[k] = TurtleGlobals[k] } 
  )
  return symTab
}


// --

// FIX: respect pen position -- draw when pen is down
function _Forward (exprl, s, k) {
  var howFar, p0, p1
  return s.eval (
    exprl[0], s,
    function (howFar) {
      var p0 = s.turtle.pos.copy ()
      var p1 = p0.copy().
        plus (s.turtle.heading.directionVect().times(howFar))
      if (s.turtle.penIsDown) { lineFromTo (p0, p1, s) }
      s.turtle.pos = p1
      return k (howFar)
    });
}

function _RotateRight (exprl, s, k) {
  return s.eval (exprl[0], s,
                 function (howMuch) {
                   s.turtle.heading.rotate (howMuch)
                   return k (howMuch)
                 });
}

function _RotateLeft (exprl, s, k) {
  return s.eval (exprl[0], s,
                 function (howMuch) {
                   s.turtle.heading.rotate (-howMuch)
                   return k (howMuch)
                 });
}

function _MoveBy (exprl, s, k) {
  return s.eval (
    exprl[0], s,
    function (x) {
      return s.eval (exprl[1], s,
                         function (y) {
                           var p0 = s.turtle.pos.copy ()
                           var p1 = s.turtle.pos.plus (Point (x,y))
                           if (s.turtle.penIsDown) {
                             lineFromTo (p0, p1, s)
                           }
                           return k (Math.sqrt( Math.pow(p1.x-p0.x,2) + 
                                                Math.pow(p1.y-p0.y,2) ))
                         });
    });
}

function _MoveTo (exprl, s, k) {
  return s.eval (
    exprl[0], s,
    function (x) {
      return s.eval (exprl[1], s,
                     function (y) {
                       var p0 = s.turtle.pos.copy ()
                       var p1 = Point (x,y)
                       if (s.turtle.penIsDown) {
                         lineFromTo (p0, p1, s)
                       }
                       s.turtle.pos = p1
                       return k (Math.sqrt( Math.pow(p1.x-p0.x,2) + 
                                            Math.pow(p1.y-p0.y,2) ))
                     });
    });
}

function _TurnBy (exprl, s, k) {
  return s.eval (exprl[0], s,
                 function (d) { return k (s.turtle.heading.rotate (d)) })
}

function _TurnTo (exprl, s, k) {
  return s.eval (exprl[0], s,
                 function (d) { return k (s.turtle.heading.set (d)) })

}

function _Alpha (exprl, s, k) {
  return s.eval (exprl[0], s,
                 function (newA) { s.turtle.color.a = newA;
                                   return k (newA) })
}

function _Color (exprl, s, k) {
  return s.eval (
    exprl[0], s,
    function (r)
    { return s.eval (
      exprl[1], s,
      function (g)
      { return s.eval (
        exprl[2], s,
        function (b)
        { s.turtle.color.setRGB (r, g, b)
          return k (0.299*r + 0.587*g + 0.114*b) })
      })
    })
}

function _IndexedColor (exprl, s, k) {
  return s.eval (exprl[0], s, function (idx)
                 { s.turtle.color.setFromIndex (idx)
                   return k (idx) })
}

function _FillColor (exprl, s, k) {
  return s.eval (
    exprl[0], s,
    function (r)
    { return s.eval (
      exprl[1], s,
      function (g)
      { return s.eval (
        exprl[2], s,
        function (b)
        { s.turtle.fillColor.setRGB (r, g, b)
          return k (0.299*r + 0.587*g + 0.114*b) })
      })
    })
}

function _IndexedFillColor (exprl, s, k) {
  return s.eval (exprl[0], s, function (idx)
                 { s.turtle.fillColor.setFromIndex (idx)
                   return k (idx) })
}

function _PenToggle (exprl, s, k) {
  return k (s.turtle.penIsDown = !s.turtle.penIsDown)
  
}

function _PenUp (exprl, s, k) {
  return k (s.turtle.penIsDown = false)
}

function _PenDown (exprl, s, k) {
  return k (evalState.turtle.penIsDown = true)
}

function _StrokeWidth (exprl, s, k) {
  return s.eval (
    exprl[0], s,
    function (width) { return k (s.turtle.strokeWidth = width) })
}

function _Box (exprl, s, k) {
  return s.eval (
    exprl[0], s,
    function (width) {
      return s.eval (
        exprl[1], s,
        function (height) {
          var bl = s.turtle.pos.copy().plus(Point(-(width/2), -(height/2)))
          var str = ""
          str +=
  '<rect x="' + bl.x + '" y="' + bl.y + '" ' +
       ' width="' + width + '" height="' + height + '"' +
       ' fill="' + s.turtle.fillColor.toStringNoAlpha() + '"' +
       ' transform="rotate(' + (-s.turtle.heading.degrees()) + ',' + 
                               s.turtle.pos.x + ',' + s.turtle.pos.y + ')"'
          if (s.turtle.penIsDown) {
            str += ' style="stroke:' + s.turtle.color.toString() +
                   ';stroke-width:' + s.turtle.strokeWidth + '"'

          }
          str += (' />')
          s.svg.push (str)
          return k (width * height)
        });
    });  
}


function lineFromTo (p0, p1, evalState) {
  evalState.svg.push (
    '<line x1="' + p0.x + '" y1="' + p0.y +
      '" x2="' + p1.x + '" y2="' + p1.y +
      '" style="stroke:' + evalState.turtle.color.toString() + 
      ';stroke-width:' + evalState.turtle.strokeWidth + 
      ';stroke-linecap:round' +
      '" />'
  )
}

// --


function Turtle (heading, pos, color, strokeWidth, penState) {
  var t = Object.create (
    { _heading: 0.0,
      pos:       Point (50, 50),
      color:     Color (0, 0, 0, 99),
      fillColor: Color (50, 50, 50, 99),
      strokeWidth: 1.0,
      penIsDown: true,
      heading: {
        set: function(degrees) { return t._heading=degrees },
        rotate: function(degrees) { return t._heading+=degrees },
        degrees: function() {return t._heading},
        directionVect: function () {
          var theta = 2 * Math.PI * t._heading / 360
          return Point (Math.sin(theta), Math.cos(theta))
        }
      }
    });
  if (heading) { t._heading = heading }
  if (pos) { t.pos = pos }
  if (color) { t.color = color }
  return t
}

function Point (x, y) {
  p = Object.create (
    { x: 0, y: 0, 
      plus: function (p1) { this.x += p1.x; this.y += p1.y; return this },
      times: function (d) { this.x *= d; this.y *= d; return this },
      copy: function () { return Point (this.x, this.y) },
      toString: function () { return "(" + this.x + "," + this.y + ")" }
    });
  if (x) { p.x = x }
  if (y) { p.y = y }
  return p
}


function dbg (s) { console.log(s) }

},{"./Color":6,"./SymbolTable":9}],12:[function(require,module,exports){

exports.startProgramRun      = startProgramRun
exports.runProgram           = runProgram
exports.runProgramSVGElement = runProgramSVGElement
exports.runProgramValues     = runProgramValues

var tp = require ('./TurtlePrimitives')
var tokenizer = require ('./Tokenizer')
var parser = require ('./Parser')
var evaluator = require ('./Evaluator')


function startProgramRun (str, yieldTest) {
  var tokens, exprl, symTab, turtle, color, svg
  tokens = tokenizer.tokenize (str)
  exprl = parser.parse (tokens, tp.InitialSymbolTable ())
  symTab = tp.InitialSymbolTable ()
  turtle = tp.Turtle ()
  svg = []
  pgmState = evaluator.evaluate (exprl,
                                 symTab, { turtle: turtle, svg: svg },
                                 yieldTest
                                )
  pgmState.SVGBody = function () { return SVGBodyFromProgramState (this) }
  pgmState.SVGElement = function () { return SVGElementFromProgramState (this) }
  return pgmState
}

function runProgram (str) {
  pgmState = startProgramRun (str)
  while (! pgmState.done) {
    pgmState.continue ()
  }
  return pgmState
}

function runProgramValues (str) {
  return runProgram(str).values()
}

function runProgramSVGElement (str) {
  return runProgram(str).SVGElement()
}

function SVGBodyFromProgramState (s) {
  return '<g transform="translate(0,100)">' +
         '<g transform="scale(1,-1)">' +
         (s.svg.join("\n")) + 
         "\n" + '</g></g>';
}

function SVGElementFromProgramState (s, width, height) {
  w = width || 100; h = height || 100
  return '<svg width="' + w + '" height="' + h + 
            '" viewbox="0 0 100 100' + '">' +
         SVGBodyFromProgramState (s) +
         '</svg>'
}

},{"./Evaluator":7,"./Parser":8,"./Tokenizer":10,"./TurtlePrimitives":11}],13:[function(require,module,exports){
if (typeof Object.create === 'function') {
  // implementation from standard node.js 'util' module
  module.exports = function inherits(ctor, superCtor) {
    ctor.super_ = superCtor
    ctor.prototype = Object.create(superCtor.prototype, {
      constructor: {
        value: ctor,
        enumerable: false,
        writable: true,
        configurable: true
      }
    });
  };
} else {
  // old school shim for old browsers
  module.exports = function inherits(ctor, superCtor) {
    ctor.super_ = superCtor
    var TempCtor = function () {}
    TempCtor.prototype = superCtor.prototype
    ctor.prototype = new TempCtor()
    ctor.prototype.constructor = ctor
  }
}

},{}],14:[function(require,module,exports){
// shim for using process in browser

var process = module.exports = {};

process.nextTick = (function () {
    var canSetImmediate = typeof window !== 'undefined'
    && window.setImmediate;
    var canPost = typeof window !== 'undefined'
    && window.postMessage && window.addEventListener
    ;

    if (canSetImmediate) {
        return function (f) { return window.setImmediate(f) };
    }

    if (canPost) {
        var queue = [];
        window.addEventListener('message', function (ev) {
            var source = ev.source;
            if ((source === window || source === null) && ev.data === 'process-tick') {
                ev.stopPropagation();
                if (queue.length > 0) {
                    var fn = queue.shift();
                    fn();
                }
            }
        }, true);

        return function nextTick(fn) {
            queue.push(fn);
            window.postMessage('process-tick', '*');
        };
    }

    return function nextTick(fn) {
        setTimeout(fn, 0);
    };
})();

process.title = 'browser';
process.browser = true;
process.env = {};
process.argv = [];

function noop() {}

process.on = noop;
process.addListener = noop;
process.once = noop;
process.off = noop;
process.removeListener = noop;
process.removeAllListeners = noop;
process.emit = noop;

process.binding = function (name) {
    throw new Error('process.binding is not supported');
}

// TODO(shtylman)
process.cwd = function () { return '/' };
process.chdir = function (dir) {
    throw new Error('process.chdir is not supported');
};

},{}],15:[function(require,module,exports){
module.exports = function isBuffer(arg) {
  return arg && typeof arg === 'object'
    && typeof arg.copy === 'function'
    && typeof arg.fill === 'function'
    && typeof arg.readUInt8 === 'function';
}
},{}],16:[function(require,module,exports){
(function (process,global){
// Copyright Joyent, Inc. and other Node contributors.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit
// persons to whom the Software is furnished to do so, subject to the
// following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
// NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
// USE OR OTHER DEALINGS IN THE SOFTWARE.

var formatRegExp = /%[sdj%]/g;
exports.format = function(f) {
  if (!isString(f)) {
    var objects = [];
    for (var i = 0; i < arguments.length; i++) {
      objects.push(inspect(arguments[i]));
    }
    return objects.join(' ');
  }

  var i = 1;
  var args = arguments;
  var len = args.length;
  var str = String(f).replace(formatRegExp, function(x) {
    if (x === '%%') return '%';
    if (i >= len) return x;
    switch (x) {
      case '%s': return String(args[i++]);
      case '%d': return Number(args[i++]);
      case '%j':
        try {
          return JSON.stringify(args[i++]);
        } catch (_) {
          return '[Circular]';
        }
      default:
        return x;
    }
  });
  for (var x = args[i]; i < len; x = args[++i]) {
    if (isNull(x) || !isObject(x)) {
      str += ' ' + x;
    } else {
      str += ' ' + inspect(x);
    }
  }
  return str;
};


// Mark that a method should not be used.
// Returns a modified function which warns once by default.
// If --no-deprecation is set, then it is a no-op.
exports.deprecate = function(fn, msg) {
  // Allow for deprecating things in the process of starting up.
  if (isUndefined(global.process)) {
    return function() {
      return exports.deprecate(fn, msg).apply(this, arguments);
    };
  }

  if (process.noDeprecation === true) {
    return fn;
  }

  var warned = false;
  function deprecated() {
    if (!warned) {
      if (process.throwDeprecation) {
        throw new Error(msg);
      } else if (process.traceDeprecation) {
        console.trace(msg);
      } else {
        console.error(msg);
      }
      warned = true;
    }
    return fn.apply(this, arguments);
  }

  return deprecated;
};


var debugs = {};
var debugEnviron;
exports.debuglog = function(set) {
  if (isUndefined(debugEnviron))
    debugEnviron = process.env.NODE_DEBUG || '';
  set = set.toUpperCase();
  if (!debugs[set]) {
    if (new RegExp('\\b' + set + '\\b', 'i').test(debugEnviron)) {
      var pid = process.pid;
      debugs[set] = function() {
        var msg = exports.format.apply(exports, arguments);
        console.error('%s %d: %s', set, pid, msg);
      };
    } else {
      debugs[set] = function() {};
    }
  }
  return debugs[set];
};


/**
 * Echos the value of a value. Trys to print the value out
 * in the best way possible given the different types.
 *
 * @param {Object} obj The object to print out.
 * @param {Object} opts Optional options object that alters the output.
 */
/* legacy: obj, showHidden, depth, colors*/
function inspect(obj, opts) {
  // default options
  var ctx = {
    seen: [],
    stylize: stylizeNoColor
  };
  // legacy...
  if (arguments.length >= 3) ctx.depth = arguments[2];
  if (arguments.length >= 4) ctx.colors = arguments[3];
  if (isBoolean(opts)) {
    // legacy...
    ctx.showHidden = opts;
  } else if (opts) {
    // got an "options" object
    exports._extend(ctx, opts);
  }
  // set default options
  if (isUndefined(ctx.showHidden)) ctx.showHidden = false;
  if (isUndefined(ctx.depth)) ctx.depth = 2;
  if (isUndefined(ctx.colors)) ctx.colors = false;
  if (isUndefined(ctx.customInspect)) ctx.customInspect = true;
  if (ctx.colors) ctx.stylize = stylizeWithColor;
  return formatValue(ctx, obj, ctx.depth);
}
exports.inspect = inspect;


// http://en.wikipedia.org/wiki/ANSI_escape_code#graphics
inspect.colors = {
  'bold' : [1, 22],
  'italic' : [3, 23],
  'underline' : [4, 24],
  'inverse' : [7, 27],
  'white' : [37, 39],
  'grey' : [90, 39],
  'black' : [30, 39],
  'blue' : [34, 39],
  'cyan' : [36, 39],
  'green' : [32, 39],
  'magenta' : [35, 39],
  'red' : [31, 39],
  'yellow' : [33, 39]
};

// Don't use 'blue' not visible on cmd.exe
inspect.styles = {
  'special': 'cyan',
  'number': 'yellow',
  'boolean': 'yellow',
  'undefined': 'grey',
  'null': 'bold',
  'string': 'green',
  'date': 'magenta',
  // "name": intentionally not styling
  'regexp': 'red'
};


function stylizeWithColor(str, styleType) {
  var style = inspect.styles[styleType];

  if (style) {
    return '\u001b[' + inspect.colors[style][0] + 'm' + str +
           '\u001b[' + inspect.colors[style][1] + 'm';
  } else {
    return str;
  }
}


function stylizeNoColor(str, styleType) {
  return str;
}


function arrayToHash(array) {
  var hash = {};

  array.forEach(function(val, idx) {
    hash[val] = true;
  });

  return hash;
}


function formatValue(ctx, value, recurseTimes) {
  // Provide a hook for user-specified inspect functions.
  // Check that value is an object with an inspect function on it
  if (ctx.customInspect &&
      value &&
      isFunction(value.inspect) &&
      // Filter out the util module, it's inspect function is special
      value.inspect !== exports.inspect &&
      // Also filter out any prototype objects using the circular check.
      !(value.constructor && value.constructor.prototype === value)) {
    var ret = value.inspect(recurseTimes, ctx);
    if (!isString(ret)) {
      ret = formatValue(ctx, ret, recurseTimes);
    }
    return ret;
  }

  // Primitive types cannot have properties
  var primitive = formatPrimitive(ctx, value);
  if (primitive) {
    return primitive;
  }

  // Look up the keys of the object.
  var keys = Object.keys(value);
  var visibleKeys = arrayToHash(keys);

  if (ctx.showHidden) {
    keys = Object.getOwnPropertyNames(value);
  }

  // IE doesn't make error fields non-enumerable
  // http://msdn.microsoft.com/en-us/library/ie/dww52sbt(v=vs.94).aspx
  if (isError(value)
      && (keys.indexOf('message') >= 0 || keys.indexOf('description') >= 0)) {
    return formatError(value);
  }

  // Some type of object without properties can be shortcutted.
  if (keys.length === 0) {
    if (isFunction(value)) {
      var name = value.name ? ': ' + value.name : '';
      return ctx.stylize('[Function' + name + ']', 'special');
    }
    if (isRegExp(value)) {
      return ctx.stylize(RegExp.prototype.toString.call(value), 'regexp');
    }
    if (isDate(value)) {
      return ctx.stylize(Date.prototype.toString.call(value), 'date');
    }
    if (isError(value)) {
      return formatError(value);
    }
  }

  var base = '', array = false, braces = ['{', '}'];

  // Make Array say that they are Array
  if (isArray(value)) {
    array = true;
    braces = ['[', ']'];
  }

  // Make functions say that they are functions
  if (isFunction(value)) {
    var n = value.name ? ': ' + value.name : '';
    base = ' [Function' + n + ']';
  }

  // Make RegExps say that they are RegExps
  if (isRegExp(value)) {
    base = ' ' + RegExp.prototype.toString.call(value);
  }

  // Make dates with properties first say the date
  if (isDate(value)) {
    base = ' ' + Date.prototype.toUTCString.call(value);
  }

  // Make error with message first say the error
  if (isError(value)) {
    base = ' ' + formatError(value);
  }

  if (keys.length === 0 && (!array || value.length == 0)) {
    return braces[0] + base + braces[1];
  }

  if (recurseTimes < 0) {
    if (isRegExp(value)) {
      return ctx.stylize(RegExp.prototype.toString.call(value), 'regexp');
    } else {
      return ctx.stylize('[Object]', 'special');
    }
  }

  ctx.seen.push(value);

  var output;
  if (array) {
    output = formatArray(ctx, value, recurseTimes, visibleKeys, keys);
  } else {
    output = keys.map(function(key) {
      return formatProperty(ctx, value, recurseTimes, visibleKeys, key, array);
    });
  }

  ctx.seen.pop();

  return reduceToSingleString(output, base, braces);
}


function formatPrimitive(ctx, value) {
  if (isUndefined(value))
    return ctx.stylize('undefined', 'undefined');
  if (isString(value)) {
    var simple = '\'' + JSON.stringify(value).replace(/^"|"$/g, '')
                                             .replace(/'/g, "\\'")
                                             .replace(/\\"/g, '"') + '\'';
    return ctx.stylize(simple, 'string');
  }
  if (isNumber(value))
    return ctx.stylize('' + value, 'number');
  if (isBoolean(value))
    return ctx.stylize('' + value, 'boolean');
  // For some reason typeof null is "object", so special case here.
  if (isNull(value))
    return ctx.stylize('null', 'null');
}


function formatError(value) {
  return '[' + Error.prototype.toString.call(value) + ']';
}


function formatArray(ctx, value, recurseTimes, visibleKeys, keys) {
  var output = [];
  for (var i = 0, l = value.length; i < l; ++i) {
    if (hasOwnProperty(value, String(i))) {
      output.push(formatProperty(ctx, value, recurseTimes, visibleKeys,
          String(i), true));
    } else {
      output.push('');
    }
  }
  keys.forEach(function(key) {
    if (!key.match(/^\d+$/)) {
      output.push(formatProperty(ctx, value, recurseTimes, visibleKeys,
          key, true));
    }
  });
  return output;
}


function formatProperty(ctx, value, recurseTimes, visibleKeys, key, array) {
  var name, str, desc;
  desc = Object.getOwnPropertyDescriptor(value, key) || { value: value[key] };
  if (desc.get) {
    if (desc.set) {
      str = ctx.stylize('[Getter/Setter]', 'special');
    } else {
      str = ctx.stylize('[Getter]', 'special');
    }
  } else {
    if (desc.set) {
      str = ctx.stylize('[Setter]', 'special');
    }
  }
  if (!hasOwnProperty(visibleKeys, key)) {
    name = '[' + key + ']';
  }
  if (!str) {
    if (ctx.seen.indexOf(desc.value) < 0) {
      if (isNull(recurseTimes)) {
        str = formatValue(ctx, desc.value, null);
      } else {
        str = formatValue(ctx, desc.value, recurseTimes - 1);
      }
      if (str.indexOf('\n') > -1) {
        if (array) {
          str = str.split('\n').map(function(line) {
            return '  ' + line;
          }).join('\n').substr(2);
        } else {
          str = '\n' + str.split('\n').map(function(line) {
            return '   ' + line;
          }).join('\n');
        }
      }
    } else {
      str = ctx.stylize('[Circular]', 'special');
    }
  }
  if (isUndefined(name)) {
    if (array && key.match(/^\d+$/)) {
      return str;
    }
    name = JSON.stringify('' + key);
    if (name.match(/^"([a-zA-Z_][a-zA-Z_0-9]*)"$/)) {
      name = name.substr(1, name.length - 2);
      name = ctx.stylize(name, 'name');
    } else {
      name = name.replace(/'/g, "\\'")
                 .replace(/\\"/g, '"')
                 .replace(/(^"|"$)/g, "'");
      name = ctx.stylize(name, 'string');
    }
  }

  return name + ': ' + str;
}


function reduceToSingleString(output, base, braces) {
  var numLinesEst = 0;
  var length = output.reduce(function(prev, cur) {
    numLinesEst++;
    if (cur.indexOf('\n') >= 0) numLinesEst++;
    return prev + cur.replace(/\u001b\[\d\d?m/g, '').length + 1;
  }, 0);

  if (length > 60) {
    return braces[0] +
           (base === '' ? '' : base + '\n ') +
           ' ' +
           output.join(',\n  ') +
           ' ' +
           braces[1];
  }

  return braces[0] + base + ' ' + output.join(', ') + ' ' + braces[1];
}


// NOTE: These type checking functions intentionally don't use `instanceof`
// because it is fragile and can be easily faked with `Object.create()`.
function isArray(ar) {
  return Array.isArray(ar);
}
exports.isArray = isArray;

function isBoolean(arg) {
  return typeof arg === 'boolean';
}
exports.isBoolean = isBoolean;

function isNull(arg) {
  return arg === null;
}
exports.isNull = isNull;

function isNullOrUndefined(arg) {
  return arg == null;
}
exports.isNullOrUndefined = isNullOrUndefined;

function isNumber(arg) {
  return typeof arg === 'number';
}
exports.isNumber = isNumber;

function isString(arg) {
  return typeof arg === 'string';
}
exports.isString = isString;

function isSymbol(arg) {
  return typeof arg === 'symbol';
}
exports.isSymbol = isSymbol;

function isUndefined(arg) {
  return arg === void 0;
}
exports.isUndefined = isUndefined;

function isRegExp(re) {
  return isObject(re) && objectToString(re) === '[object RegExp]';
}
exports.isRegExp = isRegExp;

function isObject(arg) {
  return typeof arg === 'object' && arg !== null;
}
exports.isObject = isObject;

function isDate(d) {
  return isObject(d) && objectToString(d) === '[object Date]';
}
exports.isDate = isDate;

function isError(e) {
  return isObject(e) &&
      (objectToString(e) === '[object Error]' || e instanceof Error);
}
exports.isError = isError;

function isFunction(arg) {
  return typeof arg === 'function';
}
exports.isFunction = isFunction;

function isPrimitive(arg) {
  return arg === null ||
         typeof arg === 'boolean' ||
         typeof arg === 'number' ||
         typeof arg === 'string' ||
         typeof arg === 'symbol' ||  // ES6 symbol
         typeof arg === 'undefined';
}
exports.isPrimitive = isPrimitive;

exports.isBuffer = require('./support/isBuffer');

function objectToString(o) {
  return Object.prototype.toString.call(o);
}


function pad(n) {
  return n < 10 ? '0' + n.toString(10) : n.toString(10);
}


var months = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep',
              'Oct', 'Nov', 'Dec'];

// 26 Feb 16:19:34
function timestamp() {
  var d = new Date();
  var time = [pad(d.getHours()),
              pad(d.getMinutes()),
              pad(d.getSeconds())].join(':');
  return [d.getDate(), months[d.getMonth()], time].join(' ');
}


// log is just a thin wrapper to console.log that prepends a timestamp
exports.log = function() {
  console.log('%s - %s', timestamp(), exports.format.apply(exports, arguments));
};


/**
 * Inherit the prototype methods from one constructor into another.
 *
 * The Function.prototype.inherits from lang.js rewritten as a standalone
 * function (not on Function.prototype). NOTE: If this file is to be loaded
 * during bootstrapping this function needs to be rewritten using some native
 * functions as prototype setup using normal JavaScript does not work as
 * expected during bootstrapping (see mirror.js in r114903).
 *
 * @param {function} ctor Constructor function which needs to inherit the
 *     prototype.
 * @param {function} superCtor Constructor function to inherit prototype from.
 */
exports.inherits = require('inherits');

exports._extend = function(origin, add) {
  // Don't do anything if add isn't an object
  if (!add || !isObject(add)) return origin;

  var keys = Object.keys(add);
  var i = keys.length;
  while (i--) {
    origin[keys[i]] = add[keys[i]];
  }
  return origin;
};

function hasOwnProperty(obj, prop) {
  return Object.prototype.hasOwnProperty.call(obj, prop);
}

}).call(this,require("K/m7xv"),typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{"./support/isBuffer":15,"K/m7xv":14,"inherits":13}]},{},[5])