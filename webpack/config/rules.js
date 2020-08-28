import MiniCssExtractPlugin from 'mini-css-extract-plugin'

module.exports = function({ target, production }) {
  return [
    ...(require('webpack-spago-loader/rules')({ spagoAbsoluteOutputDir: require('../lib/spago-options').output })),

    {
      test: /\.css$/i,
      use: [
        MiniCssExtractPlugin.loader,
        {
          loader: 'css-loader',
          options: {
            importLoaders: 1,
          },
        },
        {
          loader: 'postcss-loader',
        }
      ],
    },

    {
      test: /\.(png|jpg|gif|svg)$/i,
      use: [
        {
          loader: 'url-loader',
          options: {
            limit: 8192,
          },
        },
      ],
    },
  ]
}
