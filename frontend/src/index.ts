// @ts-ignore
import { Elm } from "./Main.elm";
import { register, SwiperContainer } from 'swiper/swiper-element-bundle';

register();

const app = Elm.Main.init({
  node: document.getElementById("root"),
  flags: {
    backendUrl: window.location.origin.replace('8080', '8081'),
  },
});

app.ports.swiperSlideNext.subscribe(function (maybeId: string | null) {
  const selector = 'swiper-container' + maybeId ? `#${maybeId}` : '';
  const swiperEl = document.querySelector(selector) as SwiperContainer | null;
  swiperEl?.swiper.slideNext(800);
});
