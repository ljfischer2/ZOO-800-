# EDI started out of a desire to maintain LTER data
# Satisfies Data Cite standards
# Publications are Data packages, transformed to be easily interpretable.
# Contain anything and everything in terms of file type.
# EML is uploaded, and cycles through evaluations and validations until publish
# Automate Workflows will send you notifications of updates to datasets
# Can filter by custom collections

#
install.packages('gbifdb')
library(gbifdb)
library(tidyverse)

gbif <- gbif_remote(backend = "duckdb", bucket = 'gbif-open-data-us-east-1')

tam <- gbif %>%
  filter(genus == "Tamandua",
         countrycode == "EG",
         year > 2000) %>%
  select(species, countrycode, decimallatitude, decimallongitude, eventdate) %>%
  head(500)

df <- collect(tam)
summary(df)
