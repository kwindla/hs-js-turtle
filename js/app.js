
var Evt = require ('./UserEvents.js')
var Dispatcher = require ('./Dispatcher.js')
var Store      = require ('./Store.js')
var Views      = require ('./Views.js')


var examplePgmStrings = [
  "&s0{a=(17/10)#19{T5(a*5)R5a=a+1}'0}E(1/5)^M50,22^A25s^M50,50^A50s^M50,78^A99s"
  , "E(1/8)i=-1^M5,5^#10{#10{Di=i+1 T10,10^N10 0^}^N(-100)10^}"
  , "#8{R45#4{#90{F(3/5)R2}R90}}"
  , "E(1/5)#36{R10#8{F18L45}}"
  , "^N(-50)0^d=(2/13)i=0#27{E((i\\4)+1)*(4/19)#180{FdR2}d=d*(11/10)^B(-d)0^i=i+1}"
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
