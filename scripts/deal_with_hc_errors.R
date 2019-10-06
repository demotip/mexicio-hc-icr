
full_sample_post_p1b4 <- readRDS("~/Documents/data_project/mexico_hand_coding/mexico-hc-icr/data_raw/full_sample_post_p1b4.rds")
clean_handcoding <- readRDS("./data_clean/handcoding_all.rds")

#first set of missing values here, was done from no upds from some reason
write.csv(filter(icr2_nodups, str_length(S1) > 14), file = "./data_clean/id_errors.csv")

id_errors <- read_csv("./data_raw/id_errors_BPR.csv") %>% select(S1, `correct folio`, ResponseId)
delete <- filter(id_errors, `correct folio` == "?") %>% select(ResponseId) %>% unlist()
keep <- filter(id_errors, `correct folio` != "?") %>% select(ResponseId) %>% unlist()

#missing_main <- anti_join(clean_handcoding, full_sample_post_p1b4, by = c("S1" = "FOLIO"))

clean_handcoding <- clean_handcoding %>% full_join(id_errors, by = "ResponseId", suffix = c("", "e")) %>%
  mutate(S1 = ifelse(!is.na(`correct folio`), `correct folio`, S1))

missing_main <- anti_join(clean_handcoding, full_sample_post_p1b4, by = c("folio_id" = "FOLIO"))

write_rds(clean_handcoding, "./data_clean/handcoding_all_noerrors.rds")
clean_handcoding_dups<- clean_handcoding %>% filter(n>1)
clean_handcoding_nodups <- clean_handcoding %>% filter(n==1)

write_csv(x = clean_handcoding_nodups, "./data_clean/handcoding_once_only.csv")
write_rds(x = clean_handcoding_nodups, "./data_clean/handcoding_once_only.rds")

write_csv(x = clean_handcoding_dups, "./data_clean/handcoding_dups.csv")
write_rds(x = clean_handcoding_dups, "./data_clean/handcoding_dups.rds")