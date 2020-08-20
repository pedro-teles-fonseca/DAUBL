
rm(list = ls())

source("R/read-data.r")
source("R/auxiliary-functions.r")

Sample_sizes <- rbind(
  t(apply(data, 2, bl_length_without_NAs)),
  bl_length_without_NAs(Pooled.sample)
  )

colnames(Sample_sizes) <- paste0("BL", 1:2, " sample size")
rownames(Sample_sizes) <- c(names(data), "Pooled sample")

# Export tables to .tex files

library("Hmisc")

latex(Sample_sizes, file = "tables/sample_sizes.tex")


