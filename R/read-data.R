
data <- read.delim("data/data.txt")

datalist <- list()

for (c in 1:ncol(data)) {
  assign(names(data)[c], data[, c])
  datalist[[c]] <- data[, c]
}

Pooled.sample <- do.call("c", datalist)

rm(c)

