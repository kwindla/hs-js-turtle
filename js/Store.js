
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
