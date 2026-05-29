# Reproducible headless screenshot of the Shiny dashboard.
# Run from the assignment root:  Rscript scripts/capture_screenshot.R
#
# Requires: webshot2 + chromote (install.packages(c("webshot2","chromote")))
# and a Chrome/Chromium install (auto-detected by chromote).
#
# appshot() launches app.R in a background process, waits `delay` seconds for
# the plotly/htmlwidgets charts to finish rendering over the websocket, then
# captures the Overview tab and shuts the app down.
suppressPackageStartupMessages(library(webshot2))

app_dir <- normalizePath(".")
out     <- file.path(app_dir, "report", "dashboard_screenshot.png")

webshot2::appshot(
  app     = app_dir,
  file    = out,
  delay   = 9,
  vwidth  = 1400,
  vheight = 1500,
  zoom    = 2
)

cat("Saved:", out, "(", file.info(out)$size, "bytes )\n")
