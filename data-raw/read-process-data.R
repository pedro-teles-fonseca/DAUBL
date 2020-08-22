
library(usethis)

data <- read.delim(
  file = "data-raw/data-raw.tsv",
  dec = ".",
  na.strings = ":",
  header = TRUE
)

data <- data.frame(lapply(data, blanks_rm))

data[, 1] <- substr(data[, 1], 1, 5)

country_codes <- sort(unique(data[, 1]))

countries <- c(
  "austria",
  "belgium",
  "cyprus",
  "finland",
  "france",
  "germany",
  "greece",
  "ireland",
  "italy",
  "luxembourg",
  "malta",
  "netherlands",
  "portugal",
  "slovakia",
  "slovenia",
  "spain"
)

data_list <- list()

for(j in seq_along(countries)){

  data_list[[j]] <- as.numeric(unlist(data[data$GEO.UNIT.SECTOR.NA_ITEM.TIME == country_codes[j], -1]))
  names(data_list)[j] <- countries[j]
}

for (col in seq_along(data_list)) {
  assign(paste0(names(data_list)[col], "_bl1"), msdigit(na_rm(data_list[[col]])))
  assign(paste0(names(data_list)[col], "_bl2"), smsdigit(na_rm(data_list[[col]])))
}

pooled.sample <- do.call("c", data_list)
pooled_sample_bl1 <- na_rm(msdigit(pooled.sample))
pooled_sample_bl2 <- na_rm(smsdigit(pooled.sample))

usethis::use_data(
  austria_bl1,
  belgium_bl1,
  cyprus_bl1,
  finland_bl1,
  france_bl1,
  germany_bl1,
  greece_bl1,
  ireland_bl1,
  italy_bl1,
  luxembourg_bl1,
  malta_bl1,
  netherlands_bl1,
  portugal_bl1,
  slovakia_bl1,
  slovenia_bl1,
  spain_bl1,
  pooled_sample_bl1,
  austria_bl2,
  belgium_bl2,
  cyprus_bl2,
  finland_bl2,
  france_bl2,
  germany_bl2,
  greece_bl2,
  ireland_bl2,
  italy_bl2,
  luxembourg_bl2,
  malta_bl2,
  netherlands_bl2,
  portugal_bl2,
  slovakia_bl2,
  slovenia_bl2,
  spain_bl2,
  pooled_sample_bl2,
  overwrite = TRUE)








