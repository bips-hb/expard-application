library(expard)
library(readr)

# read in data processed in 'process-data'
pair <- readr::read_rds("results/processed_data_pen.rds")

t2x2 <- expard::create2x2table(pair)

# Apply Fisher's exact test. Also reports the odds ratio

m <- matrix(c(t2x2$a, t2x2$b, t2x2$c, t2x2$d), nrow = 2)
fisher.test(m)
