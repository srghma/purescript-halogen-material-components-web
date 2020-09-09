const MDCRipple = require('@material/ripple/component').MDCRipple;

const MDCRipple = require('@material/ripple/component').MDCRipple;

const matches = require('@material/dom/ponyfill').matches;

const applyPassive = require('@material/dom/events').applyPassive; // Result can be cached

// XXX: getRoot is Nullary, but it's impossible
exports.attachFoundation = function(htmlElement, unbounded, options) {
  return function() {
    const adapter = {
      addClass:                             function(className) { options.addClass(className)() },
      browserSupportsCssVars:               function() { return util.supportsCssVariables(window) },
      computeBoundingRect:                  function() { return options.root.getBoundingClientRect() },
      containsEventTarget:                  function(target) { return options.root.contains(target) },
      deregisterDocumentInteractionHandler: function(evtType, handler) { return document.documentElement.removeEventListener(evtType, handler, applyPassive()) },
      deregisterInteractionHandler:         function(evtType, handler) { return options.root.removeEventListener(evtType, handler, applyPassive()) },
      deregisterResizeHandler:              function(handler) { return window.removeEventListener('resize', handler) },
      getWindowPageOffset:                  function() { return { x: window.pageXOffset, y: window.pageYOffset } },
      isSurfaceActive:                      function() { return matches(options.root, ':active') },
      isSurfaceDisabled:                    options.isSurfaceDisabled,
      isUnbounded:                          function() { return unbounded },
      registerDocumentInteractionHandler:   function(evtType, handler) { return document.documentElement.addEventListener(evtType, handler, applyPassive()) },
      registerInteractionHandler:           function(evtType, handler) { return options.root.addEventListener(evtType, handler, applyPassive()) }, // TODO: support user events other than click
      registerResizeHandler:                function(handler) { return window.addEventListener('resize', handler) },
      removeClass:                          function(className) { options.removeClass(className)() },
      updateCssVariable:                    function(varName, value) { return options.root.style.setProperty(varName, value) }, // TODO: support user style prop
    };

    const foundation = new MDCRippleFoundation(adapter);

    foundation.init();

    foundation.unbounded = unbounded

    return new MDCRipple(htmlElement);
  }
}
