exports.attachFoundation = function(htmlElement) {
  return function() {
    const MDCRipple = require('@material/ripple/component').MDCRipple;

    return new MDCRipple(htmlElement);
  }
}
