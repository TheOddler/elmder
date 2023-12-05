import { Elm } from "./Main.elm";
import { register } from 'swiper/swiper-element-bundle';

register();

const app = Elm.Main.init({
  node: document.getElementById("root"),
  flags: {
    backendUrl: window.location.origin.replace('8080', '8081'),
  },
});

// app.ports.setPointerCapture.subscribe(function (event) {
//   event.target.setPointerCapture(event.pointerId);
// });

// app.ports.releasePointerCapture.subscribe(function (event) {
//   event.target.releasePointerCapture(event.pointerId);
// });
