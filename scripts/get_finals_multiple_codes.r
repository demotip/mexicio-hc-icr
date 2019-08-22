pacman::p_load(tidyverse, tidyselect, irr, kableExtra, irrNA, lubridate, splitstackshape)

icr <- read_csv("./data_raw/Big Data y Acceso a Info en México_August 9, 2019_14.44.csv")
icr_names <- names(icr)
rm(icr)
icr <- read_csv("./data_raw/Big Data y Acceso a Info en México_August 9, 2019_14.44.csv",
                skip  =3,
                col_names = icr_names)
icr2 <- icr %>% 
  filter(!(is.na(RecipientEmail)), Progress==100) 

#cleanup folio_id
icr2 <- mutate(icr2, folio_id = ifelse(grepl("'", S1),
                                       trimws(S1),
                                       trimws(str_c("'", S1))))

#get short coder_id
icr2 <- mutate(icr2, coder_id = str_extract(RecipientEmail, "^[[:alnum:]]+"))

#omit earlier coding
icr2 <- icr2 %>%
  mutate(start_date = date(StartDate)) %>%
  filter(start_date >= date("2019-06-08") )

#get rid of dublicated by taking the one with the last start time time
icr2 <- icr2 %>% group_by(folio_id, coder_id ) %>%
  filter(StartDate==max(StartDate)) %>%
  mutate(duplicate = n()>1) %>%
  ungroup()

#check
stopifnot(sum(icr2$duplicate) ==0)

#get all the double coded things and then use majority rule for those observations

icr2 <- icr2 %>% add_count(folio_id)
#check if the same coder coded the same folio multiple times
icr2 <- icr2 %>% add_count(folio_id, coder_id, name = "dup_code_folio") 

icr2_dups <- icr2 %>% filter(n>1)
icr2_nodups <- icr2 %>% filter(n==1)

icr_vars <- quo(c(S2, S3,                   
              S4_2, S5,S6_1, S6_2, S6_3, S6_4, S7, S8, S9, S10_1, S10_2, S10_3, S10_4, S11,
              R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11)  )
icr_vars_q <- c("S2", "S3",                   
                  "S4_2", "S5","S6_1", "S6_2", "S6_3", "S6_4", "S7"," S8", "S9", "S10_1", "S10_2", "S10_3"," S10_4", "S11",
                  "R1", "R2", "R3", "R4", "R5", "R6", "R7", "R8", "R9", "R10", "R11") 
#requires dev_version of tidyr
wide_dups <- icr2 %>%
  pivot_wider(id_cols = c(coder_id, folio_id), 
               names_from  = coder_id,
               values_from = UQ(icr_vars))

#get each variable, get only the coders who code it, then but in final columns, then choose final one
all_names <- names(wide_dups)[-1]


# get_final_code_yn <- function(df) {
#   ifelse(df$total_yes/df$non_na < .5, 0,
#          ifelse(df$total_yes/df$non_na > .5, 1,
#                 ifelse(!is.na(select(df, contains("jesicatapia"))),
#                        select(df, contains("jesicatapia")),
#            ifelse(!is.na(select(df, contains("carmen"))),
#                   select(df, contains("carmen")),
#                   ifelse(!is.na(select(df, contains("andrea"))),
#                          select(df, contains("andrea")),
#                          ifelse(!is.na(select(df, contains("sandra"))),
#                                 select(df, contains("sandra")),
#                          NA))))))
# }

question <- "S2_"

#if there is a majority answer, use it. 
#if there is a tie default to the order below
S2 <- select(wide_dups, folio_id, matches("S2_*")) %>%
  mutate_at(vars(contains("S2")), funs(recode(., `No` = 0, `Si` = 1, .default = NaN))) %>%
  mutate(total_yes = rowSums(select(., contains("S2")), na.rm = TRUE)) %>%
  mutate(non_na = 6 - rowSums(is.na(select(., contains("S2")))), ratio = total_yes/non_na) %>%
 # rowwise() %>%
  mutate(S2_final_code = ifelse( ratio< .5, 0,
                               ifelse(ratio > .5, 1,
                                      ifelse(!is.na(!! sym(paste0(question, "jesicatapia"))),
                                             !! sym(paste0(question, "jesicatapia")),
                                             ifelse(!is.na(!! sym(paste0(question, "carmen"))),
                                                    !! sym(paste0(question, "carmen")),
                                                    ifelse(!is.na(!! sym(paste0(question, "andrea"))),
                                                           !! sym(paste0(question, "andrea")),
                                                           ifelse(!is.na(!! sym(paste0(question, "sandra"))),
                                                                  !! sym(paste0(question, "sandra")),
                                                                  ifelse(!is.na(!! sym(paste0(question, "edithimg"))),
                                                                         !! sym(paste0(question, "edithimg")), NA ))))))))
#if the majority chose to skip the question, skip it
#if there is one mode, choose that mode
#if there are multiple modes, defer to order
question <- "S3_"
S3 <- full_join(select(S2, S2_final_code, folio_id), select(wide_dups, folio_id, matches("S3_*")))
S3$Count <- apply(select(S3, contains("S3_")), 1, function(x) length(na.omit(unique(x))))
S3$modes <- apply(select(S3, contains("S3_")), 1,  mfv, na.rm=TRUE)

S3 <- S3 %>% rowwise %>% 
  mutate(S3_final_code = ifelse(S2_final_code == 0, NA, 
                      ifelse(Count == 1 | (length(modes) == 1 & !is.nan(modes)), modes, 
                                    ifelse(length(modes) > 1,
                                           ifelse(!is.na(!! sym(paste0(question, "jesicatapia"))),
                                                  !! sym(paste0(question, "jesicatapia")),
                                                  ifelse(!is.na(!! sym(paste0(question, "carmen"))),
                                                         !! sym(paste0(question, "carmen")),
                                                         ifelse(!is.na(!! sym(paste0(question, "andrea"))),
                                                                !! sym(paste0(question, "andrea")),
                                                                ifelse(!is.na(!! sym(paste0(question, "sandra"))),
                                                                       !! sym(paste0(question, "sandra")),
                                                                       ifelse(!is.na(!! sym(paste0(question, "edithimg"))),
                                                                              !! sym(paste0(question, "edithimg")), NA )))))))))

                    
                                        

for(i in icr_vars_q) {
 obs <- select(wide_dups, matches(paste0(i,"S2_*")))
 print(obs)
}
