
## internal functions for metadata

# neon geographic coverage
# 
# @param sites vector of NEON siteID codes
# @noRd
# 
# neon_geographic_coverage(c("BART", "KONZ", "SRER", "OSBS"))
neon_geographic_coverage <- function(sites){
  geo <- jsonlite::read_json(system.file("extdata/geo.json", package="neon4cast"))
  site_ids <- purrr::map_chr(purrr::map(geo, "geographicDescription"), 1)
  site_ids <- purrr::map_chr(strsplit(site_ids, ","), 1)
  names(geo) <- site_ids
  geo[sites]
}


theme_sites <- function(theme){
  switch(theme,
    terrestrial = c("BART", "KONZ", "SRER", "OSBS"),
    aquatic = c("BARC", "POSE"),
    beetles = c("BART", "HARV", "BLAN", "SCBI", "SERC", "DSNY", "JERC", "OSBS",
                "GUAN", "LAJA", "STEI", "TREE", "UNDE", "KONA", "KONZ","UKFS",
                "GRSM", "MLBS", "ORNL", "DELA", "LENO", "TALL", "DCFS", "NOGP",
                "WOOD", "CPER", "RMNP", "STER", "CLBJ", "OAES","YELL", "MOAB",
                "NIWO", "JORN", "SRER", "ONAQ", "ABBY", "WREF", "SJER", "SOAP",
                "TEAK", "BARR", "TOOL", "BONA", "DEJU", "HEAL", "PUUM"),
    tick =  c("BLAN", "ORNL", "SCBI", "SERC", "KONZ", "TALL", "UKFS"),
    phenology = c("HARV", "BART","SCBI","STEI","UKFS","GRSM","DELA","CLBJ")
  )
}
