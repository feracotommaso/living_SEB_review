#### --------------------------------------------------- 25/01/31 --------------------------------------------------- ####
d <- readxl::read_excel("data/1.abstracting/25_01_31_deduplicated_processed.xlsx")
nrow1 <- nrow(d)
d <- d[d$decision != "exclude", ]
nrow2 <- nrow(d)
writexl::write_xlsx(d, "data/2.full_abstracting/25_01_31_fullabstracting.xlsx")
nrow1-nrow2 # Excluded

#### --------------------------------------------------- 25/04/30 --------------------------------------------------- ####
d <- readxl::read_excel("data/1.abstracting/25_04_30_deduplicated_processed.xlsx")
nrow1 <- nrow(d)
d <- d[d$decision != "exclude", ]
nrow2 <- nrow(d)
writexl::write_xlsx(d, "data/2.full_abstracting/25_04_30_fullabstracting.xlsx")
nrow1-nrow2 # Excluded
#### --------------------------------------------------- 25/09/01 --------------------------------------------------- ####
d <- readxl::read_excel("data/1.abstracting/25_09_01_deduplicated_processed.xlsx")
nrow1 <- nrow(d)
d <- d[d$decision != "exclude", ]
nrow2 <- nrow(d)
writexl::write_xlsx(d, "data/2.full_abstracting/25_09_01_fullabstracting.xlsx")
nrow1-nrow2 # Excluded
#### --------------------------------------------------- 25/11/01 --------------------------------------------------- ####
d <- readxl::read_excel("data/1.abstracting/25_11_01_deduplicated_processed.xlsx")
nrow1 <- nrow(d)
d <- d[d$decision != "exclude", ]
nrow2 <- nrow(d)
writexl::write_xlsx(d, "data/2.full_abstracting/25_11_01_fullabstracting.xlsx")
nrow1-nrow2 # Excluded
#### --------------------------------------------------- 25/12/01 --------------------------------------------------- ####
d <- readxl::read_excel("data/1.abstracting/25_12_01_deduplicated_processed.xlsx")
nrow1 <- nrow(d)
d <- d[d$decision != "exclude", ]
nrow2 <- nrow(d)
writexl::write_xlsx(d, "data/2.full_abstracting/25_12_01_fullabstracting.xlsx")
nrow1-nrow2 # Excluded
#### --------------------------------------------------- 25/12/01 --------------------------------------------------- ####
d <- readxl::read_excel("data/1.abstracting/26_01_06_deduplicated_processed.xlsx")
nrow1 <- nrow(d)
d <- d[d$decision != "exclude", ]
nrow2 <- nrow(d)
writexl::write_xlsx(d, "data/2.full_abstracting/26_01_06_fullabstracting.xlsx")
nrow1-nrow2 # Excluded
