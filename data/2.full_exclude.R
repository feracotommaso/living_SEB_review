#### --------------------------------------------------- 25/01/31 --------------------------------------------------- ####
d <- readxl::read_excel("data/2.full_abstracting/25_01_31_decision_progress.xlsx")
d <- d[d$final_decision != "exclude", ]
writexl::write_xlsx(d, "data/3.meta_data/basic_info/25_01_31_basic_info.xlsx")

#### --------------------------------------------------- 25/04/30 --------------------------------------------------- ####
d <- readxl::read_excel("data/2.full_abstracting/25_04_30_full_decisions.xlsx")
d <- d[d$final_decision != "exclude", ]
writexl::write_xlsx(d, "data/3.meta_data/basic_info/25_04_30_basic_info.xlsx")

#### --------------------------------------------------- 25/09/01 --------------------------------------------------- ####
d <- readxl::read_excel("data/2.full_abstracting/25_09_01_full_decisions.xlsx")
d <- d[!is.na(d$paper_id),]
d <- d[d$final_decision != "exclude", ]
writexl::write_xlsx(d, "data/3.meta_data/basic_info/25_09_01_basic_info.xlsx")

#### --------------------------------------------------- 25/11/01 --------------------------------------------------- ####
d <- readxl::read_excel("data/2.full_abstracting/25_11_01_full_decisions.xlsx")
d <- d[!is.na(d$paper_id),]
d <- d[d$final_decision != "exclude", ]
writexl::write_xlsx(d, "data/3.meta_data/basic_info/25_11_01_basic_info.xlsx")

#### --------------------------------------------------- 25/12/01 --------------------------------------------------- ####
d <- readxl::read_excel("data/2.full_abstracting/25_12_01_full_decisions.xlsx")
d <- d[!is.na(d$paper_id),]
d <- d[d$final_decision != "exclude", ]
writexl::write_xlsx(d, "data/3.meta_data/basic_info/25_12_01_basic_info.xlsx")
#### --------------------------------------------------- 26/01/06 --------------------------------------------------- ####
d <- readxl::read_excel("data/2.full_abstracting/26_01_06_full_decisions.xlsx")
d <- d[!is.na(d$paper_id),]
d <- d[d$final_decision != "exclude", ]
writexl::write_xlsx(d, "data/3.meta_data/basic_info/26_01_06_basic_info.xlsx")
