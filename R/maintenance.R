# requireNamespace("job", quietly = TRUE)
# job::job(packages = c("purrr", "dplyr", "arrow", "janitor", "galah", "stringr", "lubridate", "infinitylists"), {
#   
#   galah::galah_config(email = "wcornwell@gmail.com")
#   
#   download_ala_obs(taxon = "Papilionoidea")
#   download_ala_obs(taxon = "Odonata")
#   download_ala_obs(taxon = "Marsupialia")
#   download_ala_obs(taxon = "Cicadoidea")
#   download_ala_obs(taxon = "Plantae")
# })
# 
# 
# job::job(packages = c("purrr", "dplyr", "arrow", "janitor", "galah", "stringr", "lubridate", "infinitylists"), {
#   
#   galah::galah_config(atlas = "Global",
#                         username = Sys.getenv("GBIF_USERNAME"),
#                         password = Sys.getenv("GBIF_PWD"),
#                         email = Sys.getenv("GBIF_EMAIL")
#                       )
#   
#   download_gbif_obs(taxon = "Arachnida", country_code = "FR")
#   download_gbif_obs(taxon = "Squamata", min_year = 2023)
#   # download_gbif_obs(taxon = "Squamata", country_code = "ES")
# })
# 
# 
