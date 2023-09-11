### Clean and Join by-year downloads and save as a parquet for the app

library(stringr)
library(arrow)
library(purrr)
library(dplyr)

# # Shandiya's suggestion: 
# 
# # Create a global scheme
# # This is needed because in some years references is a bool() and other years it is a string()
# infinity_schema <- schema(
#     field("recordID", string()),
#     field("species", string()),
#     field("genus", string()),
#     field("family", string()),
#     field("decimalLatitude", double()),
#     field("decimalLongitude", double()),
#     field("coordinateUncertaintyInMeters", double()),
#     field("eventDate", timestamp("us", timezone="UTC")),
#     field("datasetName", string()),
#     field("basisOfRecord", string()),
#     field("references", string()),
#     field("institutionCode", string()),
#     field("recordedBy", string()),
#     field("outlierLayerCount", double()),
#     field("isDuplicateOf", string())
# )

# Open all the files with our schema
x <- open_dataset(sources = "ignore/") 
x |> write_parquet(sink = paste0("infinity-app/data/Australia-Plantae-", Sys.Date(), ".parquet")) # That works! 
