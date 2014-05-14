
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
