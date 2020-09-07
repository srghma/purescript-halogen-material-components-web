let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20200831/packages.dhall sha256:cdb3529cac2cd8dd780f07c80fd907d5faceae7decfcaa11a12037df68812c83

let overrides = {=}

let additions = {=}

in      upstream
    //  https://raw.githubusercontent.com/srghma/my-purescript-package-sets/master/packages.dhall sha256:6d1a867eb821ae07fea3021b3df23c083c30137d644767e271503c32aa460fd4
          upstream.(https://raw.githubusercontent.com/srghma/my-purescript-package-sets/master/upstreamTypeChunk.dhall sha256:1f07f2737ec9a052fa448d4f2c3058ed5bb68ea66622ac3d1f74bd78eeeac09b)
    //  overrides
    //  additions
