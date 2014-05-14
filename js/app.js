
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