# shinyTinker 0.1.0

- Initial release with tri-state checkbox and info radio buttons controls.
- Documentation and tests.

# shinyTinker 0.3.0

- Added code for download button.

# shinyTinker 0.4.0

- `infoRadioButtons()` gains a `label_width` argument: set a fixed width for
  choice labels so the info bubbles align in a vertical column.
- Small spacing fix: a 2px gap between labels and info bubbles.
- Fixed the info bubble icon call to use `shiny::icon()` explicitly; previously
  it errored when `shiny` was not attached or when a custom `icon` was passed.
- Added license info
