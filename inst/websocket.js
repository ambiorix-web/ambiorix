window.ws = new WebSocket('ws://' + window.location.host);

var wss = class {
  constructor() {
    this._ws = window.ws;
  }
  // Get names of datasets
  send(name, message) {
    let msg = {name: name, message: message};
    this._ws.send(JSON.stringify(msg));
  }
}

window.wss = new wss();
