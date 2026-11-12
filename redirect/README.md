# Redirect for the old Shiny deployments

`app.R` is a minimal Shiny app that sends visitors to the app's permanent
address, <https://w3id.org/infinitylists/>, which forwards to the static site
at <https://traitecoevo.github.io/infinitylists/app/>. Deploy it **over** each
old deployment, using the same app ID so the URL stays the same. Links in the
paper, the README and bookmarks then keep working.

> **Keep <https://unsw.shinyapps.io/infinitylists/> deployed.** It is the URL
> printed in the paper (Cornwell, Kar & Mesaglio 2025, *Applications in Plant
> Sciences* 13(3): e70012, <https://doi.org/10.1002/aps3.70012>). The monthly
> `link-check` workflow opens an issue if it stops forwarding.

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

Needs R with the `shiny` and `rsconnect` packages, and an admin login for the
`unsw` shinyapps.io account. You don't need the rest of the repo.

```r
install.packages(c("shiny", "rsconnect"))

# 1. Get the redirect app
dir.create("redirect")
download.file(
  "https://raw.githubusercontent.com/traitecoevo/infinitylists/master/redirect/app.R",
  "redirect/app.R"
)

# 2. Optional: check it locally. The browser should land on the new site.
shiny::runApp("redirect")

# 3. Connect R to the shinyapps.io account. In the dashboard
#    (https://www.shinyapps.io/admin/#/tokens, signed in to the unsw account):
#    Account > Tokens > Show > Copy to clipboard. Paste and run the line it
#    gives you, which looks like:
#    rsconnect::setAccountInfo(name = "unsw", token = "...", secret = "...")
rsconnect::accounts()  # note the account name it lists (expected: "unsw")

# 4. Deploy over the old apps. appId pins the exact app, so the URLs stay
#    the same. Change `account` if accounts() showed a different name.
rsconnect::deployApp("redirect", appId = 11400862, account = "unsw",
                     server = "shinyapps.io", appFiles = "app.R",
                     forceUpdate = TRUE, launch.browser = FALSE)  # the paper's URL
rsconnect::deployApp("redirect", appId = 12113944, account = "unsw",
                     server = "shinyapps.io", appFiles = "app.R",
                     forceUpdate = TRUE, launch.browser = FALSE)  # current-location variant
```

Then open <https://unsw.shinyapps.io/infinitylists/> and
<https://unsw.shinyapps.io/infinitylists-current-location/>. Each should land
on the new app within a few seconds.

The UNSW Posit Connect copy
(<https://posit-connect-unsw.intersect.org.au/infinitylists/>, app ID 1) isn't
in the paper. If someone has an account on that server, deploy the same way
(`rsconnect::connectUser(server = "posit-connect-unsw.intersect.org.au")`,
then `deployApp("redirect", appId = 1, server = "posit-connect-unsw.intersect.org.au", appFiles = "app.R", forceUpdate = TRUE)`).
Otherwise, ask the server's admins to remove it.

## The chain of addresses

```
paper → unsw.shinyapps.io/infinitylists/   (this app; set once)
      → w3id.org/infinitylists/            (redirect rules in perma-id/w3id.org,
                                             ids/infinitylists/.htaccess)
      → traitecoevo.github.io/infinitylists/app/
```

If the app ever moves (new repo, custom domain, other host), change only the
w3id rules, by pull request to <https://github.com/perma-id/w3id.org>. This
deployment and the paper link need no change.

Keep the repository at `traitecoevo/infinitylists`. GitHub forwards renamed or
transferred repositories, but not their GitHub Pages sites.

## Who can deploy

Losing access to the shinyapps.io account means losing the paper URL. Make
sure at least two current lab members are admins of the `unsw` shinyapps.io
account, and update that list when people leave.

If shinyapps.io itself ever shuts down, the only remedy is a correction
notice to the journal giving <https://w3id.org/infinitylists/>.

## Keeping cost at zero

The redirect only needs the smallest instance. On shinyapps.io:

- In the dashboard, set **Instance Size** to the smallest and **Instance Idle
  Timeout** to the minimum, so each visit uses as little active time as
  possible.
- The account can then go down to the **Free** plan (5 apps, 25 active hours
  per month). If the free hours ever run out, the old URL stops redirecting
  until the month resets. The new site itself is unaffected.
- Archive any other old apps on the account that are no longer needed.
