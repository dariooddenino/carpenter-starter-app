var path = require('path');
var webpack = require('webpack');
var HtmlWebpackPlugin = require('html-webpack-plugin');
var ExtractTextPlugin = require('extract-text-webpack-plugin');

module.exports = {
  entry: [ path.join(__dirname, 'index.js') ],
  debug: false,
  output: {
    path: path.resolve('./dist'),
    filename: '[name]-[hash].min.js',
    publicPath: '/purescript-carpenter-todomvc/'
  },
  module: {
    loaders: [
      {
        test: /\.js$/,
        loader: 'source-map-loader',
        include: [ path.resolve(__dirname, 'node_modules/todomvc-common') ]
      },
      {
        test: /\.css/,
        loader: ExtractTextPlugin.extract('style-loader', 'css-loader'),
        include: [
          path.resolve(__dirname, 'node_modules/todomvc-common'),
          path.resolve(__dirname, 'node_modules/todomvc-app-css')
        ]
      },
      {
        test: /\.purs$/,
        loader: 'purs-loader',
        exclude: /node_modules/,
        query: {
          psc: 'psa',
          bundle: true,
          warnings: false
        }
      }
    ]
  },
  plugins: [
    new webpack.DefinePlugin({
      'process.env.NODE_ENV': JSON.stringify('production')
    }),
    new webpack.optimize.OccurrenceOrderPlugin(true),
    new webpack.optimize.UglifyJsPlugin({
      minimize: true,
      compress: {
        warnings: false
      }
    }),
    new HtmlWebpackPlugin({
      template: 'index.html',
      inject: 'body',
      filename: 'index.html'
    }),
    new ExtractTextPlugin('[name].css')
  ],
  resolveLoader: {
    root: path.join(__dirname, 'node_modules')
  },
  resolve: {
    modulesDirectories: [
      'node_modules',
      'bower_components'
    ],
    extensions: ['', '.js', '.purs']
  }
};
