
library(usethis)

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

data <- read.delim(file = "data-raw/data-raw.tsv",
                   dec = ".",
                   na.strings = ":",
                   header = TRUE)

data[, 1] <- substr(data[, 1], 1, 5)

data <- data.frame(lapply(data, function(x) {
  gsub(" ", "", x)
}))

country_codes <- sort(unique(data[, 1]))

data_df <- data.frame(rep(0, 2646))

for(j in seq_along(countries)){
  assign(countries[j], as.numeric(as.matrix(data[data$GEO.UNIT.SECTOR.NA_ITEM.TIME == country_codes[j], -1])))
  data_df[[j]] <- eval(parse(text = countries[j]))
  names(data_df)[j] <- countries[j]
}

for (col in seq_along(data_df)) {
  assign(paste0(names(data_df)[col], "_bl1"), na_rm(msdigit(data_df[, col])))
  assign(paste0(names(data_df)[col], "_bl2"), na_rm(smsdigit(data_df[, col])))
}

datalist <- list()

for (c in 1:ncol(data_df)) {
  assign(names(data_df)[c], data_df[, c])
  datalist[[c]] <- data_df[, c]
}

pooled.sample <- do.call("c", datalist)
pooled.sample_bl1 <- na_rm(msdigit(pooled.sample))
pooled.sample_bl2 <- na_rm(smsdigit(pooled.sample))

rm(c)

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
  pooled.sample_bl1,
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
  pooled.sample_bl2,
  overwrite = TRUE)








