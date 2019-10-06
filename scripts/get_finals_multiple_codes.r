pacman::p_load(dplyr, stringr, readr, irr, kableExtra, irrNA, lubridate, splitstackshape, statip, fastDummies, gtools)
devtools::install_github("tidyverse/tidyr")
library(tidyr)
#deal with weird Qualtrics export

rm(list=ls())
#need to do error checking here. You can't specify aboth skip_col and skip_match
agreeement_cat_fn <- function(wide_dups, col_regex, skip_df = NULL, skip_col = NULL, skip_match = NULL) {
  new_df <- select(wide_dups, folio_id, matches(col_regex))
  if(!is.null(skip_df)) {
    new_df <- full_join(new_df, select(skip_df, folio_id, !! skip_col), by = "folio_id")
  }
  
  if(!is.null(skip_match)) {
    new_df <- full_join(new_df, select(skip_df, folio_id, matches(skip_match)), by = "folio_id")
  }
  new_df$Count <- apply(select(new_df, matches(col_regex)), 1, function(x) length(na.omit(unique(x))))
  new_df$modes <- apply(select(new_df, matches(col_regex)), 1,  mfv, na.rm=TRUE)
  return(new_df)
}

icr2_dups <- read_rds("./data_clean/handcoding_dups.rds")

dummies <- sort(names(select(icr2_dups, matches(".*_dummy_.*"))))

# icr_vars <- quo(list(!!!(S2, S3,                   
#               S4_2, S5,S6_1, S6_2, S6_3, S6_4, S7, S8, S9, S10_1, S10_2, S10_3, S10_4,
#               R1, R2, R3, R5, R6, R7, R8, R9, R10, R11)  ))

icr_vars_q <-  c("S2", "S3",                   
                 "S4_1", "S4_2", "S5","S6_1", "S6_2", "S6_3", "S6_4", "S7","S8", "S9", "S10_1", "S10_2", "S10_3","S10_4", 
                "R1", "R2", "R3", "R5", "R7", "R8", "R9", "R11", dummies)
#requires dev_version of tidyr
wide_dups <- icr2_dups %>%
  pivot_wider(id_cols = c(coder_id, folio_id), 
               names_from  = coder_id,
               values_from = (!! icr_vars_q))

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
  mutate(total_yes = rowSums(select(., contains("S2")), na.rm = TRUE)) %>%
  mutate(non_na = 6 - rowSums(is.na(select(., contains("S2")))), ratio = total_yes/non_na) %>%
 # rowwise() %>%
  mutate(S2_final_code = ifelse( ratio < .5, 0,
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


final_dups <- select(wide_dups, folio_id) %>%
  left_join(select(S2, folio_id, S2= S2_final_code), by = "folio_id")

#if the majority chose to skip the question, skip it
#if there is one mode, choose that mode
#if there are multiple modes, defer to order
question <- "S3_"
S3 <- full_join(select(S2, S2_final_code, folio_id), select(wide_dups, folio_id, matches("S3_*")))
S3$Count <- apply(select(S3, contains("S3_")), 1, function(x) length(na.omit(unique(x))))
S3$modes <- apply(select(S3, contains("S3_")), 1,  mfv, na.rm=TRUE)


S3 <- S3 %>% rowwise() %>% 
  mutate(S3_final_code = ifelse(S2_final_code == 0, NA, 
                      ifelse(Count == 1 | (length(modes) == 1 & !is.nan(modes)), modes[[1]], 
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

final_dups <- final_dups %>%
  left_join(select(S3, folio_id, S3= S3_final_code), by = "folio_id")


question <- "S4_1_"
S4_1 <- select(wide_dups, folio_id, matches("S4_1_.*"))
S4_1$Count <- apply(select(S4_1, contains("S4_1")), 1, function(x) length(na.omit(unique(x))))
S4_1$modes <- apply(select(S4_1, contains("S4_1")), 1,  mfv, na.rm=TRUE)


#WTF
S4_1 <- S4_1 %>% rowwise %>%
  mutate(S4_1_final_code =
           ifelse(Count == 1 | (length(modes) == 1 & !is.nan(modes[[1]]) ), 
                  modes[[1]], 
                  ifelse(!is.na(!! sym(paste0(question, "jesicatapia"))),
                                                !! sym(paste0(question, "jesicatapia")),
                                                ifelse(!is.na(!! sym(paste0(question, "carmen"))),
                                                       !! sym(paste0(question, "carmen")),
                                                       ifelse(!is.na(!! sym(paste0(question, "andrea"))),
                                                              !! sym(paste0(question, "andrea")),
                                                              ifelse(!is.na(!! sym(paste0(question, "sandra"))),
                                                                     !! sym(paste0(question, "sandra")),
                                                                     ifelse(!is.na(!! sym(paste0(question, "edithimg"))),
                                                                            !! sym(paste0(question, "edithimg")), NA)) ) ) ) ) )
        

question <- "S4_2_"
# S4_2 <- select(wide_dups, folio_id, matches("S4_2_.*"))
# S4_2$Count <- apply(select(S4_1, contains("S4_2")), 1, function(x) length(na.omit(unique(x))))
# S4_2$modes <- apply(select(S4_1, contains("S4_2")), 1,  mfv, na.rm=TRUE)

S4_2 <- agreeement_cat_fn(wide_dups, "S4_2", skip_df = S4_1, skip_col = quo(S4_1_final_code))

#WTF
S4_2 <- S4_2 %>% rowwise %>%
  mutate(S4_2_final_code =
           ifelse(Count == 1 | (length(modes) == 1 & !is.nan(modes[[1]]) ), 
                  modes[[1]], 
                  ifelse(!is.na(!! sym(paste0(question, "jesicatapia"))),
                         !! sym(paste0(question, "jesicatapia")),
                         ifelse(!is.na(!! sym(paste0(question, "carmen"))),
                                !! sym(paste0(question, "carmen")),
                                ifelse(!is.na(!! sym(paste0(question, "andrea"))),
                                       !! sym(paste0(question, "andrea")),
                                       ifelse(!is.na(!! sym(paste0(question, "sandra"))),
                                              !! sym(paste0(question, "sandra")),
                                              ifelse(!is.na(!! sym(paste0(question, "edithimg"))),
                                                     !! sym(paste0(question, "edithimg")), NA)) ) ) ) ) )

#"S4_1"                  "S4_2" are 10 point scales


final_dups <- final_dups %>%
  left_join(select(S4_1, folio_id, S4_1= S4_1_final_code), by = "folio_id") %>%
  left_join(select(S4_2, folio_id, S4_2= S4_2_final_code), by = "folio_id")


#S5 is contingent on S4
question <- "S5_"
S5 <- full_join(select(S4_1, S4_1_final_code, folio_id), select(wide_dups, folio_id, matches("S5_*")))
S5$Count <- apply(select(S5, contains("S5")), 1, function(x) length(na.omit(unique(x))))
S5$modes <- apply(select(S5, contains("S5")), 1,  mfv, na.rm=TRUE)

S5 <- S5 %>% rowwise() %>% 
  mutate(S5_final_code = ifelse(S4_1_final_code == 1, NA, 
                                ifelse(Count == 1 | (length(modes) == 1 & !is.nan(modes)), modes[[1]], 
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


final_dups <- final_dups %>%
  left_join(select(S5, folio_id, S5= S5_final_code), by = "folio_id") 

question <- "S9_"
S9 <- select(wide_dups, folio_id, matches("S9_.*"))
S9$Count <- apply(select(S9, contains("S9")), 1, function(x) length(na.omit(unique(x))))
S9$modes <- apply(select(S9, contains("S9")), 1,  mfv, na.rm=TRUE)

S9 <- S9 %>% rowwise() %>%
  mutate(S9_final_code =
           ifelse(Count == 1 | (length(modes) == 1 & !is.nan(modes[[1]]) ), 
                  modes[[1]], 
                  ifelse(!is.na(!! sym(paste0(question, "jesicatapia"))),
                         !! sym(paste0(question, "jesicatapia")),
                         ifelse(!is.na(!! sym(paste0(question, "carmen"))),
                                !! sym(paste0(question, "carmen")),
                                ifelse(!is.na(!! sym(paste0(question, "andrea"))),
                                       !! sym(paste0(question, "andrea")),
                                       ifelse(!is.na(!! sym(paste0(question, "sandra"))),
                                              !! sym(paste0(question, "sandra")),
                                              ifelse(!is.na(!! sym(paste0(question, "edithimg"))),
                                                     !! sym(paste0(question, "edithimg")), NA)) ) ) ) ) )


final_dups <- final_dups %>%
  left_join(select(S9, folio_id, S9= S9_final_code), by = "folio_id")


#S6_1-4 are binary and all yes no
s_binary_cols <- names(icr2_dups)[grepl("S[61]0?_[0-9]", names(icr2_dups))]
S_bin_list <- list()
for(i in s_binary_cols) {
  question <- i
  pattern <- paste0(question, "_*")

element <- select(wide_dups, folio_id, matches(pattern)) %>%
  mutate(total_yes = rowSums(select(., contains(question)), na.rm = TRUE)) %>%
  mutate(non_na = 6 - rowSums(is.na(select(., contains(question)))), ratio = total_yes/non_na) %>%
  # rowwise() %>%
  mutate(final_code = ifelse( ratio < .5, 0,
                                 ifelse(ratio > .5, 1,
                                        ifelse(!is.na(!! sym(paste0(question, "_jesicatapia"))),
                                               !! sym(paste0(question, "_jesicatapia")),
                                               ifelse(!is.na(!! sym(paste0(question, "_carmen"))),
                                                      !! sym(paste0(question, "_carmen")),
                                                      ifelse(!is.na(!! sym(paste0(question, "_andrea"))),
                                                             !! sym(paste0(question, "_andrea")),
                                                             ifelse(!is.na(!! sym(paste0(question, "_sandra"))),
                                                                    !! sym(paste0(question, "_sandra")),
                                                                    ifelse(!is.na(!! sym(paste0(question, "_edithimg"))),
                                                                           !! sym(paste0(question, "_edithimg")), NA ))))))))
  
S_bin_list[[question]] <- element 
}

for(i in 1:length(S_bin_list)) {
  final_dups[ , names(S_bin_list[i]) ] <- S_bin_list[[i]]$final_code
}



#THESE ARE ALL THE MULTIPLE RESPONSE ALLOWED OPTIONS
#S7,8,11 all dummy variables
s_dummy_cols <- names(icr2_dups)[grepl("S[781]1?_dummy_.*", names(icr2_dups))]
S_bin_list_dummy2 <- list()
for(i in s_dummy_cols) {
  question <- i
  pattern <- paste0(question, "_*")
  element <- select(wide_dups, folio_id, matches(pattern)) %>%
    mutate(total_yes = rowSums(select(., contains(question)), na.rm = TRUE)) %>%
    mutate(non_na = 6 - rowSums(is.na(select(., contains(question)))), ratio = total_yes/non_na) %>%
    # rowwise() %>%
    mutate(final_code = ifelse( ratio < .5, 0,
                                ifelse(ratio > .5, 1,
                                       ifelse(!is.na(!! sym(paste0(question, "_jesicatapia"))),
                                              !! sym(paste0(question, "_jesicatapia")),
                                              ifelse(!is.na(!! sym(paste0(question, "_carmen"))),
                                                     !! sym(paste0(question, "_carmen")),
                                                     ifelse(!is.na(!! sym(paste0(question, "_andrea"))),
                                                            !! sym(paste0(question, "_andrea")),
                                                            ifelse(!is.na(!! sym(paste0(question, "_sandra"))),
                                                                   !! sym(paste0(question, "_sandra")),
                                                                   ifelse(!is.na(!! sym(paste0(question, "_edithimg"))),
                                                                          !! sym(paste0(question, "_edithimg")), NA ))))))))
  
  S_bin_list_dummy2[[question]] <- element 
}

for(i in 1:length(S_bin_list_dummy2)) {
  final_dups[ , names(S_bin_list_dummy2[i]) ] <- S_bin_list_dummy2[[i]]$final_code
}
######RESPONSES

question <- "R1_"

R1 <- agreeement_cat_fn(wide_dups, "R1_.*")

# candidate_rank_fn <- function(question, df){
#   mutate(df, ifelse(!is.na(!! sym(paste0(question, "jesicatapia"))),
#   !! sym(paste0(question, "jesicatapia")),
#   ifelse(!is.na(!! sym(paste0(question, "carmen"))),
#          !! sym(paste0(question, "carmen")),
#          ifelse(!is.na(!! sym(paste0(question, "andrea"))),
#                 !! sym(paste0(question, "andrea")),
#                 ifelse(!is.na(!! sym(paste0(question, "sandra"))),
#                        !! sym(paste0(question, "sandra")),
#                        ifelse(!is.na(!! sym(paste0(question, "edithimg"))),
#                               !! sym(paste0(question, "edithimg"))
# }

R1 <- R1 %>% rowwise()  %>%
  mutate(R1_final_code =
           ifelse(Count == 1 | (length(modes) == 1 & !is.nan(modes[[1]]) ), 
                  modes[[1]], 
                  ifelse(!is.na(!! sym(paste0(question, "jesicatapia"))),
                         !! sym(paste0(question, "jesicatapia")),
                         ifelse(!is.na(!! sym(paste0(question, "carmen"))),
                                !! sym(paste0(question, "carmen")),
                                ifelse(!is.na(!! sym(paste0(question, "andrea"))),
                                       !! sym(paste0(question, "andrea")),
                                       ifelse(!is.na(!! sym(paste0(question, "sandra"))),
                                              !! sym(paste0(question, "sandra")),
                                              ifelse(!is.na(!! sym(paste0(question, "edithimg"))),
                                                     !! sym(paste0(question, "edithimg")), NA_character_)) ) ) ) ) )

final_dups <- final_dups %>%
  left_join(select(R1, folio_id, R1= R1_final_code), by = "folio_id")

#WTF
question <- "R2_"
R2 <- agreeement_cat_fn(wide_dups, "R2_.*", skip_df = R1, skip_col = quo(R1_final_code))

# candidate_rank_fn <- function(question, df){
#   mutate(df, ifelse(!is.na(!! sym(paste0(question, "jesicatapia"))),
#   !! sym(paste0(question, "jesicatapia")),
#   ifelse(!is.na(!! sym(paste0(question, "carmen"))),
#          !! sym(paste0(question, "carmen")),
#          ifelse(!is.na(!! sym(paste0(question, "andrea"))),
#                 !! sym(paste0(question, "andrea")),
#                 ifelse(!is.na(!! sym(paste0(question, "sandra"))),
#                        !! sym(paste0(question, "sandra")),
#                        ifelse(!is.na(!! sym(paste0(question, "edithimg"))),
#                               !! sym(paste0(question, "edithimg"))
# }

R2 <- R2 %>% rowwise() %>% 
  mutate(R2_final_code = ifelse(R1_final_code %in% c("No", "Yes, but cannot open link", "End"),
                                NA, 
                                ifelse(Count == 1 | (length(modes) == 1 & !is.nan(modes)),
                                       modes[[1]], 
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


final_dups <- final_dups %>%
  left_join(select(R2, folio_id, R2= R2_final_code), by = "folio_id")


question <- "R3_"
R3 <- agreeement_cat_fn(wide_dups, "R3_.*", skip_df = R1, skip_col = quo(R1_final_code))


R3 <- R3 %>% rowwise() %>% 
  mutate(R3_final_code = ifelse(R1_final_code %in% c("No", "Yes, but cannot open link", "End"),
                                NA, 
                                ifelse(Count == 1 | (length(modes) == 1 & !is.nan(modes)),
                                       modes[[1]], 
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

final_dups <- final_dups %>%
  left_join(select(R3, folio_id, R3 = R3_final_code), by = "folio_id")



#get all of the dummmy variables that we will group the output by
dummy_r_cols <- names(icr2_dups)[grepl(pattern = "R[46].*dummy_.*", x = names(icr2_dups))]

#iterate through all of the dummy R columns. 
#I am assuming the skip pattern works for the missing data
#R4, R6, R10
R_bin_list_dummy <- list()
for(i in dummy_r_cols) {
  question <- i
  pattern <- paste0(question, "_.*")
  element <- select(wide_dups, folio_id, matches(pattern)) %>%
    full_join(select(R1, folio_id, R1_final_code)) %>% #make sure folks werent sent to the end
    mutate(total_yes = rowSums(select(., contains(question)), na.rm = TRUE)) %>%
    mutate(non_na = 6 - rowSums(is.na(select(., contains(question)))), ratio = total_yes/non_na) %>%
    rowwise() %>%
    mutate(final_code = ifelse(R1_final_code == "End", NA,
                               ifelse(ratio < .5, 0,
                                ifelse(ratio > .5, 1,
                                       ifelse(!is.na(!! sym(paste0(question, "_jesicatapia"))),
                                              !! sym(paste0(question, "_jesicatapia")),
                                              ifelse(!is.na(!! sym(paste0(question, "_carmen"))),
                                                     !! sym(paste0(question, "_carmen")),
                                                     ifelse(!is.na(!! sym(paste0(question, "_andrea"))),
                                                            !! sym(paste0(question, "_andrea")),
                                                            ifelse(!is.na(!! sym(paste0(question, "_sandra"))),
                                                                   !! sym(paste0(question, "_sandra")),
                                                                   ifelse(!is.na(!! sym(paste0(question, "_edithimg"))),
                                                                          !! sym(paste0(question, "_edithimg")), NA )))))))))
  
  R_bin_list_dummy[[question]] <- element 
}

for(i in 1:length(R_bin_list_dummy)) {
  final_dups[ , names(R_bin_list_dummy[i]) ] <- R_bin_list_dummy[[i]]$final_code
}


#R5-- this depends on both S4_1 and S4_2
question <- "R5_"
R5 <- agreeement_cat_fn(wide_dups, "R5_.*", skip_df = S4_2, skip_match = "S4.*final.*")

R5 <- R5 %>% rowwise() %>% 
  mutate(R5_final_code = ifelse(S4_1_final_code == 1 | S4_2_final_code == 1 , NA, 
                                ifelse(Count == 1 | (length(modes) == 1 & !is.nan(modes)), modes[[1]], 
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
                                                                                 !! sym(paste0(question, "edithimg"))))))), NA))))


final_dups <- final_dups %>%
  left_join(select(R5, folio_id, R5 = R5_final_code), by = "folio_id")



#R7 You need to have answered R6  if you sayed tehre was no information then you went to the end
question <- "R7_"
R7 <- agreeement_cat_fn(wide_dups, "R7_.*", skip_df = R_bin_list_dummy$R6_dummy_NoInfo, skip_col  = quo(final_code))
R7 <- full_join(R7, select(R1, folio_id, R1_final_code))
R7 <- R7 %>% rowwise() %>% 
  mutate(R7_final_code = ifelse( (final_code == 1 | R1_final_code == "End"),
                                 NA, 
                                 ifelse(Count == 1 | (length(modes) == 1 & !is.nan(modes) ),
                                       modes[[1]], 
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
                                                                                 !! sym(paste0(question, "edithimg"))))))), NA))))


final_dups <- final_dups %>%
  left_join(select(R7, folio_id, R7 = R7_final_code), by = "folio_id")



#R8 - ONLY For those who got a link!
question <- "R8_"
R8 <- agreeement_cat_fn(wide_dups, "R8_.*", skip_df = R_bin_list_dummy$R6_dummy_Links, skip_col  = quo(final_code))
R8 <- full_join(R8, select(R1, folio_id, R1_final_code))
R8 <- R8 %>% rowwise() %>% 
  mutate(R8_final_code = ifelse((final_code != 1 | R1_final_code == "End"),
                                 NA, 
                                 ifelse(Count == 1 | (length(modes) == 1 & !is.nan(modes) ),
                                        modes[[1]], 
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
                                                                                  !! sym(paste0(question, "edithimg"))))))), NA))))


final_dups <- final_dups %>%
  left_join(select(R8, folio_id, R8 = R8_final_code), by = "folio_id")



#R9
question <- "R9_"
R9 <- agreeement_cat_fn(wide_dups, "R9_.*", skip_df = R_bin_list_dummy$R6_dummy_InLetter, skip_col  = quo(final_code))
R9 <- full_join(R9, select(R_bin_list_dummy$R6_dummy_Attached, folio_id, R6_attachment_final_code = final_code))
R9 <- full_join(R9, select(R1, folio_id, R1_final_code))
R9 <- R9 %>% rowwise() %>% 
  mutate(R9_final_code = ifelse( ( (final_code != 1 & R6_attachment_final_code != 1) | R1_final_code == "End" ),
                                NA, 
                                ifelse(Count == 1 | (length(modes) == 1 & !is.nan(modes) ),
                                       modes[[1]], 
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
                                                                                 !! sym(paste0(question, "edithimg"))))))), NA))))



final_dups <- final_dups %>%
  left_join(select(R9, folio_id, R9 = R9_final_code), by = "folio_id")



dummy_r10_cols <- names(icr2_dups)[grepl(pattern = "R10.*dummy_.*", x = names(icr2_dups))]
R_bin_list_dummy2 <- list()
for(i in dummy_r10_cols) {
  question <- i
  pattern <- paste0(question, "_.*")
  element <- select(wide_dups, folio_id, matches(pattern)) %>%
    full_join(select(R1, folio_id, R1_final_code)) %>% #make sure folks werent sent to the end
    full_join(select(R_bin_list_dummy$R6_dummy_NoInfo, folio_id, R6_no_info_final_code = final_code)) %>% #make sure folks got information
    mutate(total_yes = rowSums(select(., contains(question)), na.rm = TRUE)) %>%
    mutate(non_na = 6 - rowSums(is.na(select(., contains(question)))), ratio = total_yes/non_na) %>%
    rowwise() %>%
    mutate(final_code = ifelse(R1_final_code == "End", NA,
                               ifelse(ratio < .5, 0,
                                      ifelse(ratio > .5, 1,
                                             ifelse(!is.na(!! sym(paste0(question, "_jesicatapia"))),
                                                    !! sym(paste0(question, "_jesicatapia")),
                                                    ifelse(!is.na(!! sym(paste0(question, "_carmen"))),
                                                           !! sym(paste0(question, "_carmen")),
                                                           ifelse(!is.na(!! sym(paste0(question, "_andrea"))),
                                                                  !! sym(paste0(question, "_andrea")),
                                                                  ifelse(!is.na(!! sym(paste0(question, "_sandra"))),
                                                                         !! sym(paste0(question, "_sandra")),
                                                                         ifelse(!is.na(!! sym(paste0(question, "_edithimg"))),
                                                                                !! sym(paste0(question, "_edithimg")), NA )))))))))
  
  R_bin_list_dummy2[[question]] <- element 
}

for(i in 1:length(R_bin_list_dummy2)) {
  final_dups[ , names(R_bin_list_dummy2[i]) ] <- R_bin_list_dummy2[[i]]$final_code
}



#R11
question <- "R11_"
R11 <- select(wide_dups, folio_id, matches("R11_.*")) %>% 
  mutate(total_yes = rowSums(select(., matches("R11_.*")), na.rm = TRUE)) %>%
  mutate(non_na = 6 - rowSums(is.na(select(., matches("R11_.*")))), ratio = total_yes/non_na) %>%
  rowwise() %>%
  mutate(R11_final_code = ifelse( ratio < .5, 0,
                                 ifelse(ratio > .5, 1,
                                        ifelse(!is.na(!! sym(paste0(question, "jesicatapia"))),
                                               !! sym(paste0(question, "jesicatapia")),
                                               ifelse(!is.na(!! sym(paste0(question, "carmen"))),
                                                      !! sym(paste0(question, "carmen")),
                                                      ifelse(!is.na(!! sym(paste0(question, "andrea"))),
                                                             !! sym(paste0(question, "andrea")),
                                                             ifelse(!is.na(!! sym(paste0(question, "sandra"))),
                                                                    !! sym(paste0(question, "sandra")),
                                                                    ifelse(!is.na(!! sym(paste0(question, "edithimg"))), NA))))))))

final_dups <- final_dups %>%
  left_join(select(R11, folio_id, R11 = R11_final_code), by = "folio_id")  

final_dups <- final_dups[, mixedsort(names(final_dups))]

write_csv(final_dups, "./data_clean/unique_dups_only.csv")

###BRING DUPLICATES AND NON DUPLICATES BACK TOGETHER
final_nodups <- read_rds("./data_clean/handcoding_once_only.rds")

common_cols <- intersect(colnames(final_nodups), colnames(final_dups)) %>% mixedsort() #put everythin in order

all_handcoding <- bind_rows(select(final_nodups, !!!common_cols),
          select(final_dups, !!!common_cols))

filter(all_handcoding, R10_dummy_NoInfo == 1 && (R10_dummy_MachineEdit == 1 | R10_dummy_MachineRead | R10_dummy_NotMachineRead == 1)) %>% select(matches("R10"))

#ENFORCE idiosyncratic selection patters and remove NA dummy_cols (maybe remove NA_dummy cols earlier)
all_handcoding <- all_handcoding %>% select(-matches("_NA$")) %>%
  mutate_at(vars("R4_dummy_Nonexistent",
                 "R4_dummy_NotProcessed",
                 "R4_dummy_NotResponsable",
                 "R4_dummy_OutsideLaw",
                 "R4_dummy_PublicAvailable",
                 "R4_dummy_Confidential",
                 "R4_dummy_DeliveryElectronic",
                 "R4_dummy_DeliveryPerson",
                 "R4_dummy_NeedMoreInfo"), funs(ifelse(R4_dummy_None == 1, 0, .))) %>%
  mutate_at(vars("R6_dummy_InLetter",
                 "R6_dummy_Links",
                 "R6_dummy_Attached"), funs(ifelse(R6_dummy_NoInfo == 1, 0, .))) %>%
  mutate_at(vars("R10_dummy_MachineEdit",
                 "R10_dummy_MachineRead",
                 "R10_dummy_NotMachineRead"), funs(ifelse(R10_dummy_NoInfo == 1, 0, .)))


#nor rowse so looks good
filter(all_handcoding, R10_dummy_NoInfo == 1 && (R10_dummy_MachineEdit == 1 | R10_dummy_MachineRead | R10_dummy_NotMachineRead == 1)) %>% select(matches("R10"))
write_rds(all_handcoding, "./data_clean/handcoding_forML.rds")
write_csv(all_handcoding, "./data_clean/handcoding_forML.csv")
