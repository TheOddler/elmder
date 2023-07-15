import { Elm } from "./Main.elm";

const app = Elm.Main.init({
  node: document.getElementById("root")
});

// app.ports.setPointerCapture.subscribe(function (event) {
//   event.target.setPointerCapture(event.pointerId);
// });

// app.ports.releasePointerCapture.subscribe(function (event) {
//   event.target.releasePointerCapture(event.pointerId);
// });
