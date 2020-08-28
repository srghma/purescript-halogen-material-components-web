import webpack from 'webpack'
import http from 'http'
import serveHandler from 'serve-handler'

import createConfig from './config'
import webpackGetError from './lib/webpackGetError'

(async function () {
  const config = await createConfig({ production: false })

  http
    .createServer((request, response) => {
      return serveHandler(
        request,
        response,
        {
          public: config.output.path,
          cleanUrls: true,
        }
      )
    })
    .listen(3000, () => {
      console.log('Running at http://localhost:3000')
    })

  require('webpack-spago-loader/watcher-job')({
    additionalWatchGlobs: ['(app|src)/**/*.(css|sass|scss)'],
    options: require('./lib/spago-options'),
    onStart: () => {},
    onError: () => {},
    onSuccess: async () => {
      const compiler = webpack(config)

      console.log('[webpack] Compiling...')

      compiler.run((err, stats) => {
        const error = webpackGetError(err, stats)

        if (error) {
          console.log(stats)
          console.error(error)
          return
        }

        console.log('[webpack] Compiling done')
      })
    }
  })
})()
