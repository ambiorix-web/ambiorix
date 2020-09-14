// get websocket
let ambiorixSocket = new WebSocket('ws://' + window.location.host);

class Ambiorix {
  constructor(){
    this._handlers = new Map()
  }
  // send
  static send(name, message) {

    // build message
    let msg = {name: name, message: message, isAmbiorix: true};
  
    ambiorixSocket.send(JSON.stringify(msg));
  
  }
  start(){
    var that = this;
    ambiorixSocket.onmessage = function(msg){
      let msgParsed = JSON.parse(msg.data);

      if(!msgParsed.isAmbiorix)
        return ;

      if(that._handlers.has(msgParsed.name)){
        that._handlers.get(msgParsed.name)(msgParsed.message);
      }
    }
  }
  // receiver
  receive(name, fun){
    this._handlers.set(name, fun)
  }
}
