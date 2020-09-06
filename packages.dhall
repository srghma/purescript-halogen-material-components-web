let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20200831/packages.dhall sha256:cdb3529cac2cd8dd780f07c80fd907d5faceae7decfcaa11a12037df68812c83

let overrides = {=}

let additions = {=}

in      upstream
    //  https://raw.githubusercontent.com/srghma/dotfiles/master/spago/packages.dhall sha256:da57a54570c83b3d6ba32782ef80ddcf26237361d5d83bfeb8734cc1cd1c3d30
          upstream.(https://raw.githubusercontent.com/srghma/dotfiles/master/spago/upstreamTypeChunk.dhall sha256:1f07f2737ec9a052fa448d4f2c3058ed5bb68ea66622ac3d1f74bd78eeeac09b)
    //  overrides
    //  additions
