# Material Components for Halogen

how to regenerate classes

```
gen () {
  # e.g. $2 = "Classes.RMWC.Avatar"
  # e.g. filedir = "Classes/RMWC/Avatar.purs"
  filedir=$(echo -n "$2" | sd --string-mode '.' '/' && echo -n ".purs")
  output="/home/srghma/projects/purescript-halogen-nextjs/src/$filedir"
  mkdir -p $(dirname $output)

  /home/srghma/projects/generate-halogen-generate-css-classes/.stack-work/dist/x86_64-linux-nix/Cabal-2.4.0.1/build/generate-halogen-generate-css-classes-exe/generate-halogen-generate-css-classes-exe \
    --input "/home/srghma/projects/purescript-halogen-nextjs/node_modules/$1" \
    --output "$output" \
    --module-name "$2"
}

gen @rmwc/avatar/avatar.css                                RMWC.Classes.Avatar
gen @rmwc/badge/badge.css                                  RMWC.Classes.Badge
gen @rmwc/circular-progress/circular-progress.css          RMWC.Classes.CircularProgress
gen @rmwc/data-table/data-table.css                        RMWC.Classes.DataTable
gen @rmwc/icon/icon.css                                    RMWC.Classes.Icon
gen @rmwc/list/collapsible-list.css                        RMWC.Classes.List
gen @rmwc/select/select.css                                RMWC.Classes.Select
gen @rmwc/theme/theme.css                                  RMWC.Classes.Theme
gen @rmwc/tooltip/tooltip.css                              RMWC.Classes.Tooltip
gen @material/button/dist/mdc.button.css                   Material.Classes.Button
gen @material/card/dist/mdc.card.css                       Material.Classes.Card
gen @material/checkbox/dist/mdc.checkbox.css               Material.Classes.Checkbox
gen @material/chips/dist/mdc.chips.css                     Material.Classes.Chips
gen @material/data-table/dist/mdc.data-table.css           Material.Classes.DataTable
gen @material/dialog/dist/mdc.dialog.css                   Material.Classes.Dialog
gen @material/drawer/dist/mdc.drawer.css                   Material.Classes.Drawer
gen @material/elevation/dist/mdc.elevation.css             Material.Classes.Elevation
gen @material/fab/dist/mdc.fab.css                         Material.Classes.Fab
gen @material/floating-label/dist/mdc.floating-label.css   Material.Classes.FloatingLabel
gen @material/form-field/dist/mdc.form-field.css           Material.Classes.FormField
gen @material/grid-list/dist/mdc.grid-list.css             Material.Classes.GridList
gen @material/icon-button/dist/mdc.icon-button.css         Material.Classes.IconButton
gen @material/image-list/dist/mdc.image-list.css           Material.Classes.ImageList
gen @material/layout-grid/dist/mdc.layout-grid.css         Material.Classes.LayoutGrid
gen @material/line-ripple/dist/mdc.line-ripple.css         Material.Classes.LineRipple
gen @material/linear-progress/dist/mdc.linear-progress.css Material.Classes.LinearProgress
gen @material/list/dist/mdc.list.css                       Material.Classes.List
gen @material/menu/dist/mdc.menu.css                       Material.Classes.Menu
gen @material/menu-surface/dist/mdc.menu-surface.css       Material.Classes.MenuSurface
gen @material/notched-outline/dist/mdc.notched-outline.css Material.Classes.NotchedOutline
gen @material/radio/dist/mdc.radio.css                     Material.Classes.Radio
gen @material/ripple/dist/mdc.ripple.css                   Material.Classes.Ripple
gen @material/select/dist/mdc.select.css                   Material.Classes.Select
gen @material/slider/dist/mdc.slider.css                   Material.Classes.Slider
gen @material/snackbar/dist/mdc.snackbar.css               Material.Classes.Snackbar
gen @material/switch/dist/mdc.switch.css                   Material.Classes.Switch
gen @material/tab/dist/mdc.tab.css                         Material.Classes.Tab
gen @material/tab-bar/dist/mdc.tab-bar.css                 Material.Classes.TabBar
gen @material/tab-indicator/dist/mdc.tab-indicator.css     Material.Classes.TabIndicator
gen @material/tab-scroller/dist/mdc.tab-scroller.css       Material.Classes.TabScroller
gen @material/textfield/dist/mdc.textfield.css             Material.Classes.Textfield
gen @material/theme/dist/mdc.theme.css                     Material.Classes.Theme
gen @material/top-app-bar/dist/mdc.top-app-bar.css         Material.Classes.TopAppBar
gen @material/typography/dist/mdc.typography.css           Material.Classes.Typography
```
