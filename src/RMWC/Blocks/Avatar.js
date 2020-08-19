// from https://github.com/jamesmfriedman/rmwc/blob/bebaef72c98c613b6bd1ad5fb4ef4f44638fcf7f/src/avatar/index.tsx#L28
exports.getInitialsForName = function(name) {
  if (name) {
    const parts = name.split(' ');
    /* istanbul ignore next */
    let letters = (parts[0] || '')[0];
    if (parts.length > 1) {
      const part = parts.pop();
      /* istanbul ignore next */
      if (part) {
        letters += part[0];
      }
    }
    return letters;
  }

  return '';
};
