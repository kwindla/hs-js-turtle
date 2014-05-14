
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
