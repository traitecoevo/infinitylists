# Redirect for the retired Shiny deployments of infinitylists.
#
# The app now runs as a static site on GitHub Pages. This tiny Shiny app is
# deployed in place of the old one (same app IDs, so the same URLs, including
# the one cited in the paper) and sends visitors to the new address.
# See redirect/README.md for how to deploy it.
#
# It depends only on shiny, so the bundle is small and fast to start.
#
# The URL https://unsw.shinyapps.io/infinitylists/ is cited in the paper
# (doi:10.1002/aps3.70012), so this redirect must stay deployed.

library(shiny)

# Permanent address (w3id.org), so that if the app ever moves, only the w3id
# redirect needs changing, not this deployment.
target <- "https://w3id.org/infinitylists/"
# Shown as a second link in case w3id.org is ever unavailable.
fallback <- "https://traitecoevo.github.io/infinitylists/app/"

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
    p(style = "color: #6c757d;", "If that link doesn't work, try ",
      tags$a(href = fallback, target = "_top", fallback)),
    p(style = "color: #6c757d;", "You should be redirected automatically. Please update any bookmarks.")
  )
)

server <- function(input, output, session) {}

shinyApp(ui, server)
