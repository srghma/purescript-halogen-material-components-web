export function template({
  headTags,
  bodyTags,
}) {
  return `
  <!DOCTYPE html>
  <html>
    <head>
      <meta name="viewport" content="initial-scale=1,width=device-width"/>
      <meta charset="utf-8"/>
      <title>purescript-halogen-material-components-web</title>
      <link href="https://fonts.googleapis.com/css?family=Roboto:300,400,500" rel="stylesheet">
      <link href="https://fonts.googleapis.com/css?family=Material+Icons&display=block" rel="stylesheet">
      ${headTags}
    </head>
    <body class="mdc-typography mdc-theme--background" style="margin: 0;">
      ${bodyTags}
    </body>
  </html>
  `
}
