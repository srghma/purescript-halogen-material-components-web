export default function webpackGetError(err, stats) {
  if (err) {
    return err // this is error object
  }

  // multiple compilers
  if (stats.stats) {
    for (let stat of stats.stats) {
      const errors = stat.compilation.errors

      if (errors.length != 0) {
        return errors // this is an array
      }
    }
  } else {
    const errors = stats.compilation.errors

    if (errors.length != 0) {
      return errors // this is an array
    }
  }
}
