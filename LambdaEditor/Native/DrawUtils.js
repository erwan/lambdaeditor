Elm.Native.DrawUtils = {};
Elm.Native.DrawUtils.make = function(elm) {
    "use strict";

    elm.Native = elm.Native || {};
    elm.Native.DrawUtils = elm.Native.DrawUtils || {};
    if (elm.Native.DrawUtils.values) {
        return elm.Native.DrawUtils.values;
    }

    var cache = {};

    function offscreen() {
        var os = document.getElementById("le_offscreen");
        if (!os) {
            os = document.createElement("div");
            os.id = "le_offscreen";
            os.setAttribute('style', 'display: inline; position: absolute; top: 0; opacity: 0; width: 80000px; height: 100px;');
            var span = document.createElement("span");
            os.appendChild(span);
            document.body.appendChild(os);
        }
        return os;
    }

    function sizeOf(character, style) {
        var repeatChar = function(c, n) {
            var repeated = '';
            for(var i = 0; i < n; i++) {
                repeated += c;
            }
            return repeated;
        };
        var key = character + '/' + style;
        var w = cache[key];
        if (!w) {
            var toDraw = (character == ' ' | character == '\t' ? '&nbsp;' : character),
                nToDraw = 10;

            var span = offscreen().firstChild;
            span.innerHTML = repeatChar(toDraw, nToDraw);
            span.setAttribute('style', style || '');
            w = span.offsetWidth / nToDraw;
            cache[key] = w
        }
        return w;
    }

    function labelSize(txt) {
        return 10 /* padding + margin */ + sizeOf(txt, 'font-family: monospace; font-size: 12px;');
    }

    return elm.Native.DrawUtils.values = {
        sizeOf: F2(sizeOf),
        labelSize: labelSize
    };
};
