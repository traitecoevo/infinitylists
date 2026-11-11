# Redirect for the retired Shiny deployments of infinitylists.
#
# The app now runs as a static site on GitHub Pages. This tiny Shiny app is
# deployed in place of the old one (same app IDs, so the same URLs, including
# the one cited in the paper) and sends visitors to the new address.
# See redirect/README.md for how to deploy it.
#
# It depends only on shiny, so the bundle is small and fast to start.

library(shiny)

target <- "https://traitecoevo.github.io/infinitylists/app/"

ui <- fluidPage(
  title = "Infinity Lists has moved",
  tags$head(
    # Immediate redirect without JavaScript...
    tags$meta(`http-equiv` = "refresh", content = paste0("0; url=", target)),
    tags$link(rel = "canonical", href = target),
    # ...and with it. If the page is embedded in a frame (as some hosts do),
    # try to move the whole window; browsers may block that, in which case
    # the visible link below still works.
    tags$script(HTML(sprintf(
      "try { window.top.location.replace('%1$s'); } catch (e) { window.location.replace('%1$s'); }",
      target
    )))
  ),
  div(
    style = "max-width: 40em; margin: 15vh auto; text-align: center; font-size: 1.2em;",
    h2("Infinity Lists has moved"),
    p("The app now lives at"),
    p(tags$a(href = target, target = "_top", style = "font-size: 1.3em;", target)),
    p(style = "color: #6c757d;", "You should be redirected automatically. Please update any bookmarks.")
  )
)

server <- function(input, output, session) {}

shinyApp(ui, server)
