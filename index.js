console.log("👋 Ohai.");

import { Elm } from "./src/Main.elm"

Elm.Main.init({
  node: document.querySelector("main")
});
