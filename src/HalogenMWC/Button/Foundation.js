const MDCRipple = require('@material/ripple/component').MDCRipple;

const MDCRipple = require('@material/ripple/component').MDCRipple;

const matches = require('@material/dom/ponyfill').matches;


exports.attachFoundation = function(htmlElement, isUnbounded) {
  return function() {
    const adapter = {
      addClass:                             (className) => instance.root.classList.add(className),
      browserSupportsCssVars:               () => util.supportsCssVariables(window),
      computeBoundingRect:                  () => instance.root.getBoundingClientRect(),
      containsEventTarget:                  (target) => instance.root.contains(target),
      deregisterDocumentInteractionHandler: (evtType, handler) => document.documentElement.removeEventListener(evtType, handler, applyPassive()),
      deregisterInteractionHandler:         (evtType, handler) => instance.root.removeEventListener(evtType, handler, applyPassive()),
      deregisterResizeHandler:              (handler) => window.removeEventListener('resize', handler),
      getWindowPageOffset:                  () => ({ x: window.pageXOffset, y: window.pageYOffset }),
      isSurfaceActive:                      () => matches(instance.root, ':active'),
      isSurfaceDisabled:                    () => Boolean(instance.disabled),
      isUnbounded:                          () => Boolean(instance.unbounded),
      registerDocumentInteractionHandler:   (evtType, handler) => document.documentElement.addEventListener(evtType, handler, applyPassive()),
      registerInteractionHandler:           (evtType, handler) => instance.root.addEventListener(evtType, handler, applyPassive()),
      registerResizeHandler:                (handler) => window.addEventListener('resize', handler),
      removeClass:                          (className) => instance.root.classList.remove(className),
      updateCssVariable:                    (varName, value) => instance.root.style.setProperty(varName, value),
    };

    const foundation = new MDCRippleFoundation(adapter);

    foundation.init();

    foundation.unbounded = unbounded

    return new MDCRipple(htmlElement);
  }
}
