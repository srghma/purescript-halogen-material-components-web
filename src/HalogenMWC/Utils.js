// from https://developer.mozilla.org/en-US/docs/Web/API/EventTarget/addEventListener#Improving_scrolling_performance_with_passive_listeners
exports.checkBrowserSupportsPassive = function() {
  var passiveSupported = false;

  try {
    window.addEventListener("test", null,
      Object.defineProperty(
        {},
        "passive",
        {
          get: function() { passiveSupported = true; }
        }
      )
    );
  } catch(err) {}

  return passiveSupported
}

// from https://github.com/purescript-web/purescript-web-events/blob/v2.0.1/src/Web/Event/EventTarget.js

exports.addEventListenerWithOptions = function (type) {
  return function (listener) {
    return function (options) {
      return function (target) {
        return function () {
          return target.addEventListener(type, listener, options);
        };
      };
    };
  };
};

exports.removeEventListenerWithOptions = function (type) {
  return function (listener) {
    return function (options) {
      return function (target) {
        return function () {
          return target.removeEventListener(type, listener, options);
        };
      };
    };
  };
};
