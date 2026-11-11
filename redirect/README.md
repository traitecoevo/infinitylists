# Redirect for the old Shiny deployments

`app.R` is a minimal Shiny app that sends visitors to the new static site,
<https://traitecoevo.github.io/infinitylists/app/>. Deploy it **over** each
old deployment, using the same app ID so the URL stays the same. Links in the
paper, the README and bookmarks then keep working.

Deploy only at cutover (#116), once the new site is final.

## Old deployments

| URL | Server | App ID |
|---|---|---|
| <https://unsw.shinyapps.io/infinitylists/> | shinyapps.io | 11400862 |
| <https://unsw.shinyapps.io/infinitylists-current-location/> | shinyapps.io | 12113944 |
| <https://posit-connect-unsw.intersect.org.au/infinitylists/> | posit-connect-unsw.intersect.org.au | 1 |

These come from the `rsconnect/` records in the repo root, which were made
under the `fontikar` account. Whoever deploys needs access to that account,
or to the `unsw` shinyapps.io organisation.

## Deploy

From the repo root, in R, with the account connected
(`rsconnect::setAccountInfo()` for shinyapps.io, `rsconnect::connectUser()`
for Posit Connect):

```r
# Check it locally first: the browser should land on the new site.
shiny::runApp("redirect")

# shinyapps.io: main app (the URL in the paper)
rsconnect::deployApp("redirect", appId = 11400862, server = "shinyapps.io",
                     account = "fontikar", appFiles = "app.R", forceUpdate = TRUE)

# shinyapps.io: current-location variant
rsconnect::deployApp("redirect", appId = 12113944, server = "shinyapps.io",
                     account = "fontikar", appFiles = "app.R", forceUpdate = TRUE)

# UNSW Posit Connect
rsconnect::deployApp("redirect", appId = 1,
                     server = "posit-connect-unsw.intersect.org.au",
                     account = "fontikar", appFiles = "app.R", forceUpdate = TRUE)
```

Then open each URL and check it lands on the new site.

## Keeping cost at zero

The redirect only needs the smallest instance. On shinyapps.io:

- In the dashboard, set **Instance Size** to the smallest and **Instance Idle
  Timeout** to the minimum, so each visit uses as little active time as
  possible.
- The account can then go down to the **Free** plan (5 apps, 25 active hours
  per month). If the free hours ever run out, the old URL stops redirecting
  until the month resets. The new site itself is unaffected.
- Archive any other old apps on the account that are no longer needed.
