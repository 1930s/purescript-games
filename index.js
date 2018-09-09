// Id of the root application element
var appElementId = "app";

function main() {
    require("./index.css");
    var game = require("./output/Games.TicTacToe.UI/index.js");
    game.main();
}

function createElementWithId(id) {
    var div = document.createElement("div");
    div.id = id;
    return div;
}

// See: https://parceljs.org/hmr.html
if (module.hot) {
    module.hot.dispose(function() {
        // Module is about to be replaced
    });

    module.hot.accept(function() {
        // Module or one of its dependencies was just updated
        document
            .getElementById(appElementId)
            .replaceWith(createElementWithId(appElementId));
        main();
    });
}

document.addEventListener("DOMContentLoaded", function(event) {
    main();
});
