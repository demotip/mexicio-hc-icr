
full_sample_post_p1b4 <- readRDS("./data_raw/full_sample_post_p1b4.rds")
clean_handcoding <- readRDS("./data_clean/handcoding_all.rds")

#first set of missing values here, was done from no upds from some reason
#write.csv(filter(icr2_nodups, str_length(S1) > 14), file = "./data_clean/id_errors.csv")

id_errors <- read_csv("./data_raw/id_errors_BPR.csv") %>% select(S1, `correct folio`, ResponseId)
delete <- filter(id_errors, `correct folio` == "?") %>% select(ResponseId) %>% unlist()
keep <- filter(id_errors, `correct folio` != "?") %>% select(ResponseId) %>% unlist()

# 
# It looks like the first of those should be: 
#   '1219700039614 WHAT IS THERE "'1219700000000"
# 
# This is the folio for the row above it, which looks like the coder aborted midway through. So she probably just decided to redo the same folio. 
# 
# For the second one, it looks like she just missed the apostrophe so the zeros got cut off: '0001300003707

#missing_main <- anti_join(clean_handcoding, full_sample_post_p1b4, by = c("S1" = "FOLIO"))
#this was double coded by the same 
clean_handcoding <- filter(clean_handcoding, ResponseId != "R_28CiiVdjb0FEiB6")
clean_handcoding$folio_id[clean_handcoding$folio_id == "'1219700000000"] <- "'1219700039614"

clean_handcoding$folio_id[clean_handcoding$folio_id == "'1300003707"] <- "'0001300003707"
clean_handcoding <- clean_handcoding %>% full_join(id_errors, by = "ResponseId", suffix = c("", "e")) %>%
  mutate(S1 = ifelse(!is.na(`correct folio`), `correct folio`, S1)) %>%
  mutate(folio_id = ifelse(!is.na(`correct folio`), `correct folio`, folio_id))



missing_main <- anti_join(clean_handcoding, full_sample_post_p1b4, by = c("folio_id" = "FOLIO"))



#remove a few with an extra quotation
clean_handcoding <- clean_handcoding %>% mutate(folio_id = gsub("''", "'", folio_id )) %>%
  mutate(folio_id = trimws(folio_id, which = "both"))

#clean_handcoding$folio_id[clean_handcoding$folio_id =="'https://www.infomex.org.mx/gobiernofederal/moduloPublico/MimeAdjuntoSeguimientoGenerator.action?folio=0001300001114&respuesta=6"] <- "'0001300001114"

#find screwed up folios
messup <- clean_handcoding %>% filter(grepl("[a-zA-Z\\?]", folio_id) | str_length(folio_id) > 14 | str_length(folio_id) <  3) %>%
  select(coder_id, S1, `correct folio`, folio_id)

#none left
if(nrow(messup) > 0) write_excel_csv2(messup, "./data_clean/bad_folios2.csv")


setdiff(x = clean_handcoding$folio_id, y= full_sample_post_p1b4$FOLIO)
#setdiff(y = icr2$folio_id, x= sampled_ids$FOLIO)


unique_handcoded_ids <- clean_handcoding %>% distinct(folio_id) %>% pull() %>% tibble()
write_csv(unique_handcoded_ids, "./data_clean/unique_handcoded_folio_ids.csv")


write_rds(clean_handcoding, "./data_clean/handcoding_all_noerrors.rds")
write_csv(clean_handcoding, "./data_clean/handcoding_all_noerrors.csv")

clean_handcoding_dups<- clean_handcoding %>% filter(n>1)
clean_handcoding_nodups <- clean_handcoding %>% filter(n==1)

write_csv(x = clean_handcoding_nodups, "./data_clean/handcoding_once_only.csv")
write_rds(x = clean_handcoding_nodups, "./data_clean/handcoding_once_only.rds")

write_csv(x = clean_handcoding_dups, "./data_clean/handcoding_dups.csv")
write_rds(x = clean_handcoding_dups, "./data_clean/handcoding_dups.rds")
