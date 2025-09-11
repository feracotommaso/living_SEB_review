#### --------------------------------------------------- 25/01/31 --------------------------------------------------- ####
d <- readxl::read_excel("data/2.full_abstracting/25_01_31_decision_progress.xlsx")
d <- d[d$final_decision != "exclude", ]
writexl::write_xlsx(d, "data/3.meta_data/basic_info/25_01_31_basic_info.xlsx")

#### --------------------------------------------------- 25/04/30 --------------------------------------------------- ####
d <- readxl::read_excel("data/2.full_abstracting/25_04_30_full_decisions.xlsx")
d <- d[d$final_decision != "exclude", ]
writexl::write_xlsx(d, "data/3.meta_data/basic_info/25_04_30_basic_info.xlsx")

#### --------------------------------------------------- 25/04/30 --------------------------------------------------- ####
d <- readxl::read_excel("data/2.full_abstracting/25_09_01_full_decisions.xlsx")
d <- d[!is.na(d$paper_id),]
d <- d[d$final_decision != "exclude", ]
writexl::write_xlsx(d, "data/3.meta_data/basic_info/25_09_01_basic_info.xlsx")
