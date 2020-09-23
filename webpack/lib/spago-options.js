const spagoConfig = './spago.dhall'

module.exports = {
  compiler:  'psa',
  output:    require('webpack-spago-loader/lib/getAbsoluteOutputDirFromSpago')(spagoConfig),
  pursFiles: require('webpack-spago-loader/lib/getSourcesFromSpago')(spagoConfig),

  // note that warnings are shown only when file is recompiled, delete output folder to show all warnings
  compilerOptions: {
    censorCodes: ['ImplicitQualifiedImport', 'UnusedImport', 'ImplicitImport'].join(','),

    // strict: true
  }
}
