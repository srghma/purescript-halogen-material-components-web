import MiniCssExtractPlugin from 'mini-css-extract-plugin'
import * as path from 'path'
import * as webpack from 'webpack'

import root from '../lib/root'

export default async function({
  watch,
  production,
}) {
  return {
    watch: watch,
    target: 'web',
    node: false,

    mode: production ? 'production' : 'development',
    // mode: 'production',
    // mode: 'development',

    output: {
      path: path.resolve(root, production ? 'docs' : 'docs-dev'),
      filename: 'index.js',
      publicPath: '',
    },

    entry: { main: path.resolve(root, "demo", "index.js") },

    bail: true,
    profile: false,
    stats: 'errors-only',

    context: root,

    devtool: production ? false : 'eval',

    module: {
      rules: require('./rules')({ production })
    },

    resolve: {
      modules: [ 'node_modules' ],
      extensions: [ '.purs', '.js']
    },

    plugins: [
      new MiniCssExtractPlugin({
        filename: 'index.css',
      }),

      new webpack.NoEmitOnErrorsPlugin(),

      // new webpack.ProvidePlugin({
      //   'module.require': false,
      // }),

      new (require('clean-webpack-plugin').CleanWebpackPlugin)(),

      new (require('html-webpack-plugin'))({
        minify: false,
        inject: false, // dont inject headTags and bodyTags after template is generated - we will do that ourselves
        templateContent: (options) => {
          return require(path.resolve(root, 'demo', 'template')).template({
            headTags: options.htmlWebpackPlugin.tags.headTags.toString(),
            bodyTags: options.htmlWebpackPlugin.tags.bodyTags.toString(),
          })
        },
      }),
    ],

    optimization: {
      nodeEnv: false,
      noEmitOnErrors: true,
    }
  }
}
