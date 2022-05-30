pacman::p_load(
  tidyverse,
  stringr,
  readr,
  irr,
  kableExtra,
  irrNA,
  lubridate,
  splitstackshape,
  statip,
  fastDummies,
  gtools,
  forcats
)
#devtools::install_github("tidyverse/tidyr") #dev version required
#deal with weird Qualtrics export

rm(list = ls())
#need to do error checking here. You can't specify a both skip_col and skip_match

ORD_DIFF_CLASS <- c("Very easy", "Normal", "Very difficult")
ORD_NUM_REQ <-
  c("Little or none",
    "Less than half",
    "Approximately half",
    "The majority",
    "All")

agreeement_cat_fn <-
  function(wide_dups,
           col_regex,
           skip_df = NULL,
           skip_col = NULL,
           skip_match = NULL) {
    new_df <-
      select(wide_dups, folio_id, matches(col_regex)) #get only the questions for this
    if (!is.null(skip_df)) {
      new_df <-
        full_join(new_df, select(skip_df, folio_id,!!skip_col), by = "folio_id") #deal with skip pattern
    }
    
    if (!is.null(skip_match)) {
      new_df <-
        full_join(new_df, select(skip_df, folio_id, matches(skip_match)), by = "folio_id")
    }
    new_df$Count <-
      apply(select(new_df, matches(col_regex)), 1, function(x)
        length(na.omit(unique(x))))
    new_df$modes <-
      apply(select(new_df, matches(col_regex)), 1,  mfv, na.rm = TRUE)
    return(new_df)
  }

icr2_dups <- read_rds("./data_clean/handcoding_dups.rds")

dummies <- sort(names(select(icr2_dups, matches(".*_dummy_.*"))))

icr_vars_q <-
  c(
    "S2_has_attachment",
    "S3_num_attachment_pages",
    "S4_num_info_queries",
    "S4_num_areas",
    "S5_requests_related",
    "S6_is_formal",
    "S6_is_legal",
    "S6_is_technical",
    "S6_is_aggressive",
    "S9_likely_use",
    "S10_is_clear",
    "S10_is_competency",
    "S10_is_public",
    "S10_is_existant",
    "R1_has_letter_addressed",
    "R2_num_pages_letter",
    "R3_readability",
    "R5_num_referenced_requests",
    "R7_proportion_answered",
    "R8_is_link_correct",
    "R9_diff_find_info",
    "R11_is_req_interesting",
    dummies
  )

#puts every variable in a wide data frame
wide_dups <- icr2_dups %>%
  pivot_wider(
    id_cols = folio_id,
    names_from  = coder_id,
    values_from = (all_of(icr_vars_q))
  )

#get each variable, get only the coders who code it, then but in final columns, then choose final one
all_names <- names(wide_dups)[-1]

stats_df <-
  tibble(
    stat = NA_character_,
    question = NA_character_,
    value = NA,
    .rows = 0
  )
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

question <- "S2_has_attachment_"

#if there is a majority answer, use it.
#if there is a tie default to the order below
S2 <- select(wide_dups, folio_id, matches("S2_*")) %>%
  mutate(total_yes = rowSums(select(., contains("S2")), na.rm = TRUE)) %>%
  mutate(non_na = 6 - rowSums(is.na(select(., contains(
    "S2"
  )))), ratio = total_yes / non_na) %>%
  # rowwise() %>%
  mutate(S2_final_code = ifelse(ratio < .5, 0,
                                ifelse(
                                  ratio > .5, 1,
                                  ifelse(
                                    !is.na(!!sym(paste0(
                                      question, "jesicatapia"
                                    ))),!!sym(paste0(question, "jesicatapia")),
                                    ifelse(
                                      !is.na(!!sym(paste0(question, "carmen"))),!!sym(paste0(question, "carmen")),
                                      ifelse(
                                        !is.na(!!sym(paste0(question, "andrea"))),!!sym(paste0(question, "andrea")),
                                        ifelse(!is.na(!!sym(
                                          paste0(question, "sandra")
                                        )),!!sym(paste0(question, "sandra")),
                                        ifelse(!is.na(!!sym(
                                          paste0(question, "edithimg")
                                        )),!!sym(
                                          paste0(question, "edithimg")
                                        ), NA))
                                      )
                                    )
                                  )
                                )))


#rewrite function to determine which stats are printed....

get_icr_stats <-
  function(matrix,
           var_name,
           kendall = TRUE,
           ICC = TRUE,
           kripp = TRUE,
           kripp.method = "nominal") {
    if (ICC) {
      icc <- iccNA(matrix)$ICCs[4, 1] #ICC(A,k)
    } else {
      icc <- NA
    }
    print(icc)
    if (kendall) {
      kendall <- kendallNA(matrix)$`Kendall's W`
    } else {
      kendall <- NA
    }
    print(kendall)
    if (kripp) {
      kripp_a <- kripp.alpha(t(matrix), method = kripp.method)
    } else {
      kripp_a <- NA
    }
    tibble(
      question = rep(var_name, 3),
      stat = c("icc", "kendall", "kripp.alpha"),
      value = c(icc, kendall, kripp_a$value)
    )
  }

to_stat <-
  select(S2,-folio_id,-total_yes,-non_na,-ratio,-S2_final_code)

stats_df <-
  bind_rows(stats_df, get_icr_stats(to_stat, "S2_has_attachment"))

#aggregate all the final data into final_dups
final_dups <- select(wide_dups, folio_id) %>%
  left_join(select(S2, folio_id, S2_has_attachment = S2_final_code), by = "folio_id")

#if the majority chose to skip the question, skip it
#if there is one mode, choose that mode
#if there are multiple modes, defer to order
question <- "S3_num_attachment_pages"
S3 <-
  full_join(select(S2, S2_final_code, folio_id),
            select(wide_dups, folio_id, matches("S3_*")))
S3$Count <-
  apply(select(S3, contains("S3_")), 1, function(x)
    length(na.omit(unique(x))))
S3$modes <- apply(select(S3, contains("S3_")), 1,  mfv, na_rm = TRUE)


#There is a skip pattern remove for now
S3 <- S3 %>% rowwise() %>%
  mutate(S3_final_code = ifelse(S2_final_code == 0, NA,
                                ifelse(
                                  Count == 1 | (length(modes) == 1 & !is.nan(modes)),
                                  modes[[1]],
                                  ifelse(length(modes) > 1,
                                         ifelse(
                                           !is.na(!!sym(paste0(
                                             question, "jesicatapia"
                                           ))),!!sym(paste0(question, "jesicatapia")),
                                           ifelse(
                                             !is.na(!!sym(paste0(question, "carmen"))),!!sym(paste0(question, "carmen")),
                                             ifelse(
                                               !is.na(!!sym(paste0(question, "andrea"))),!!sym(paste0(question, "andrea")),
                                               ifelse(!is.na(!!sym(
                                                 paste0(question, "sandra")
                                               )),!!sym(paste0(question, "sandra")),
                                               ifelse(!is.na(!!sym(
                                                 paste0(question, "edithimg")
                                               )),!!sym(
                                                 paste0(question, "edithimg")
                                               ), NA))
                                             )
                                           )
                                         ))
                                )))

final_dups <- final_dups %>%
  left_join(select(S3, folio_id, S3_num_attachment_pages = S3_final_code),
            by = "folio_id")


question <- "S4_num_info_queries_"
S4_1 <- select(wide_dups, folio_id, matches("S4_num_info_queries*"))
S4_1$Count <-
  apply(select(S4_1, contains("S4_num_info_queries")), 1, function(x)
    length(na.omit(unique(x))))
S4_1$modes <-
  apply(select(S4_1, contains("S4_num_info_queries")), 1,  mfv, na_rm = TRUE)

#WTF
S4_1 <- S4_1 %>% rowwise %>%
  mutate(S4_1_final_code =
           ifelse(
             Count == 1 | (length(modes) == 1 & !is.nan(modes[[1]])),
             modes[[1]],
             ifelse(
               !is.na(!!sym(paste0(
                 question, "jesicatapia"
               ))),!!sym(paste0(question, "jesicatapia")),
               ifelse(
                 !is.na(!!sym(paste0(question, "carmen"))),!!sym(paste0(question, "carmen")),
                 ifelse(
                   !is.na(!!sym(paste0(question, "andrea"))),!!sym(paste0(question, "andrea")),
                   ifelse(!is.na(!!sym(
                     paste0(question, "sandra")
                   )),!!sym(paste0(question, "sandra")),
                   ifelse(!is.na(!!sym(
                     paste0(question, "edithimg")
                   )),!!sym(
                     paste0(question, "edithimg")
                   ), NA))
                 )
               )
             )
           ))


question <- "S4_num_areas_"
# S4_2 <- select(wide_dups, folio_id, matches("S4_2_.*"))
# S4_2$Count <- apply(select(S4_1, contains("S4_2")), 1, function(x) length(na.omit(unique(x))))
# S4_2$modes <- apply(select(S4_1, contains("S4_2")), 1,  mfv, na.rm=TRUE)

S4_2 <-
  agreeement_cat_fn(
    wide_dups,
    "S4_num_areas",
    skip_df = S4_1,
    skip_col = quo(S4_1_final_code)
  )

#WTF
S4_2 <- S4_2 %>% rowwise %>%
  mutate(S4_2_final_code =
           ifelse(
             Count == 1 | (length(modes) == 1 & !is.nan(modes[[1]])),
             modes[[1]],
             ifelse(
               !is.na(!!sym(paste0(
                 question, "jesicatapia"
               ))),!!sym(paste0(question, "jesicatapia")),
               ifelse(
                 !is.na(!!sym(paste0(question, "carmen"))),!!sym(paste0(question, "carmen")),
                 ifelse(
                   !is.na(!!sym(paste0(question, "andrea"))),!!sym(paste0(question, "andrea")),
                   ifelse(!is.na(!!sym(
                     paste0(question, "sandra")
                   )),!!sym(paste0(question, "sandra")),
                   ifelse(!is.na(!!sym(
                     paste0(question, "edithimg")
                   )),!!sym(
                     paste0(question, "edithimg")
                   ), NA))
                 )
               )
             )
           ))

#"S4_1"                  "S4_2" are 10 point scales

to_stat_s_41 <- dplyr::select(S4_1, starts_with("S4_num_info"))
to_stat_s_42 <- dplyr::select(S4_2, starts_with("S4_num_areas"))

stats_df <-
  bind_rows(stats_df,
            get_icr_stats(to_stat_s_41, "S4_num_info",  kripp.method = "interval"))
stats_df <-
  bind_rows(stats_df,
            get_icr_stats(to_stat_s_42, "S4_num_areas", kripp.method = "interval"))


final_dups <- final_dups %>%
  left_join(select(S4_1, folio_id, S4_num_info_queries = S4_1_final_code),
            by = "folio_id") %>%
  left_join(select(S4_2, folio_id, S4_num_areas = S4_2_final_code), by = "folio_id")


#S5 is contingent on S4
#Won't count it in ICR
question <- "S5_requests_related_"
S5 <-
  full_join(select(S4_1, S4_1_final_code, folio_id),
            select(wide_dups, folio_id, matches("S5_*")))
S5$Count <-
  apply(select(S5, contains("S5")), 1, function(x)
    length(na.omit(unique(x))))
S5$modes <- apply(select(S5, contains("S5")), 1,  mfv, na_rm = TRUE)

S5 <- S5 %>% rowwise() %>%
  mutate(S5_final_code = ifelse(S4_1_final_code == 1, NA,
                                ifelse(
                                  Count == 1 | (length(modes) == 1 & !is.nan(modes)),
                                  modes[[1]],
                                  ifelse(length(modes) > 1,
                                         ifelse(
                                           !is.na(!!sym(paste0(
                                             question, "jesicatapia"
                                           ))),!!sym(paste0(question, "jesicatapia")),
                                           ifelse(
                                             !is.na(!!sym(paste0(question, "carmen"))),!!sym(paste0(question, "carmen")),
                                             ifelse(
                                               !is.na(!!sym(paste0(question, "andrea"))),!!sym(paste0(question, "andrea")),
                                               ifelse(!is.na(!!sym(
                                                 paste0(question, "sandra")
                                               )),!!sym(paste0(question, "sandra")),
                                               ifelse(!is.na(!!sym(
                                                 paste0(question, "edithimg")
                                               )),!!sym(
                                                 paste0(question, "edithimg")
                                               ), NA))
                                             )
                                           )
                                         ))
                                )))


final_dups <- final_dups %>%
  left_join(select(S5, folio_id, S5_requests_related = S5_final_code), by = "folio_id")

question <- "S9_likely_use_"
S9 <- select(wide_dups, folio_id, matches("S9_.*"))
S9$Count <-
  apply(select(S9, contains("S9")), 1, function(x)
    length(na.omit(unique(x))))
S9$modes <- apply(select(S9, contains("S9")), 1,  mfv, na.rm = TRUE)

S9 <- S9 %>% rowwise() %>%
  mutate(S9_final_code =
           ifelse(
             Count == 1 | (length(modes) == 1 & !is.nan(modes[[1]])),
             modes[[1]],
             ifelse(
               !is.na(!!sym(paste0(
                 question, "jesicatapia"
               ))),
               !!sym(paste0(question, "jesicatapia")),
               ifelse(
                 !is.na(!!sym(paste0(question, "carmen"))),
                 !!sym(paste0(question, "carmen")),
                 ifelse(
                   !is.na(!!sym(paste0(question, "andrea"))),
                   !!sym(paste0(question, "andrea")),
                   ifelse(!is.na(!!sym(
                     paste0(question, "sandra")
                   )), !!sym(paste0(question, "sandra")),
                   ifelse(!is.na(!!sym(
                     paste0(question, "edithimg")
                   )), !!sym(
                     paste0(question, "edithimg")
                   ), NA))
                 )
               )
             )
           ))

to_stat_s9 <- dplyr::select(S9, starts_with("S9_likely_use"))

stats_df <-
  bind_rows(
    stats_df,
    get_icr_stats(
      to_stat_s9,
      "S9_likely_use",
      ICC = FALSE,
      kendall = FALSE,
      kripp.method = "nominal"
    )
  )

final_dups <- final_dups %>%
  left_join(select(S9, folio_id, S9_likely_use = S9_final_code), by = "folio_id")


#S6_1-4 and S10 are binary and all yes no
s_binary_cols <-
  c(names(icr2_dups)[grepl("S6_*", names(icr2_dups))],
    names(icr2_dups)[grepl("S10_*", names(icr2_dups))])
S_bin_list <- list()
for (i in s_binary_cols) {
  question <- i
  pattern <- paste0(question, "_*")
  
  element <- select(wide_dups, folio_id, matches(pattern)) %>%
    mutate(total_yes = rowSums(select(., contains(question)), na.rm = TRUE)) %>%
    mutate(non_na = 6 - rowSums(is.na(select(
      ., contains(question)
    ))),
    ratio = total_yes / non_na) %>%
    # rowwise() %>%
    mutate(final_code = ifelse(ratio < .5, 0,
                               ifelse(
                                 ratio > .5, 1,
                                 ifelse(
                                   !is.na(!!sym(paste0(
                                     question, "_jesicatapia"
                                   ))),!!sym(paste0(question, "_jesicatapia")),
                                   ifelse(
                                     !is.na(!!sym(paste0(
                                       question, "_carmen"
                                     ))),!!sym(paste0(question, "_carmen")),
                                     ifelse(
                                       !is.na(!!sym(paste0(
                                         question, "_andrea"
                                       ))),!!sym(paste0(question, "_andrea")),
                                       ifelse(!is.na(!!sym(
                                         paste0(question, "_sandra")
                                       )),!!sym(paste0(
                                         question, "_sandra"
                                       )),
                                       ifelse(!is.na(!!sym(
                                         paste0(question, "_edithimg")
                                       )),!!sym(
                                         paste0(question, "_edithimg")
                                       ), NA))
                                     )
                                   )
                                 )
                               )))
  
  S_bin_list[[question]] <- element
}


for (i in 1:length(S_bin_list)) {
  to_stat <-
    dplyr::select(S_bin_list[[i]],-folio_id,-total_yes,-non_na,-ratio,-contains("final_code"))
  stats_df <-
    bind_rows(stats_df,
              get_icr_stats(to_stat, names(S_bin_list[i]), kripp.method = "nominal"))
}

for (i in 1:length(S_bin_list)) {
  final_dups[, names(S_bin_list[i])] <- S_bin_list[[i]]$final_code
}


#THESE ARE ALL THE MULTIPLE RESPONSE ALLOWED OPTIONS
#S7,8,11 all dummy variables
s_dummy_cols <-
  names(icr2_dups)[grepl("S[781]1?_dummy_.*", names(icr2_dups))]
#s_dummy_cols <- "S7_dummy_Data"
S_bin_list_dummy2 <- list()

for (i in s_dummy_cols) {
  question <- i
  pattern <- paste0(question, "_.*")
  element <- select(wide_dups, folio_id, matches(pattern)) %>%
    mutate(total_yes = rowSums(select(., contains(question)), na.rm = TRUE)) %>%
    mutate(non_na = 6 - rowSums(is.na(select(
      ., contains(question)
    ))),
    ratio = total_yes / non_na) %>%
    # rowwise() %>%
    mutate(final_code = ifelse(ratio < .5, 0,
                               ifelse(
                                 ratio > .5, 1,
                                 ifelse(
                                   !is.na(!!sym(paste0(
                                     question, "_jesicatapia"
                                   ))),
                                   !!sym(paste0(question, "_jesicatapia")),
                                   ifelse(
                                     !is.na(!!sym(paste0(
                                       question, "_carmen"
                                     ))),
                                     !!sym(paste0(question, "_carmen")),
                                     ifelse(
                                       !is.na(!!sym(paste0(
                                         question, "_andrea"
                                       ))),
                                       !!sym(paste0(question, "_andrea")),
                                       ifelse(!is.na(!!sym(
                                         paste0(question, "_sandra")
                                       )), !!sym(paste0(
                                         question, "_sandra"
                                       )),
                                       ifelse(!is.na(!!sym(
                                         paste0(question, "_edithimg")
                                       )), !!sym(
                                         paste0(question, "_edithimg")
                                       ), NA))
                                     )
                                   )
                                 )
                               )))
  
  S_bin_list_dummy2[[question]] <- element
}


#only calculate for non missing
S_bin_list_dummy2_noNA <-
  S_bin_list_dummy2[!grepl("_NA", names(S_bin_list_dummy2))]

for (i in 1:length(S_bin_list_dummy2_noNA)) {
  to_stat <-
    dplyr::select(
      S_bin_list_dummy2_noNA[[i]],-folio_id,-total_yes,-non_na,-ratio,-contains("final_code")
    )
  stats_df <-
    bind_rows(stats_df,
              get_icr_stats(to_stat, names(S_bin_list_dummy2_noNA[i]), kripp.method = "nominal"))
}

for (i in 1:length(S_bin_list_dummy2)) {
  final_dups[, names(S_bin_list_dummy2[i])] <-
    S_bin_list_dummy2[[i]]$final_code
}

select(final_dups, S7_dummy_Datum, S7_dummy_Database, S7_dummy_Data)

write_rds(stats_df, "./data_clean/requests_icr.rds")
write_excel_csv(stats_df, "./data_clean/requests_icr.csv")

###############
######RESPONSES
################
stats_df_rep <-
  tibble(
    stat = NA_character_,
    question = NA_character_,
    value = NA,
    .rows = 0
  )

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
question <- "R1_has_letter_addressed_"
R1 <- R1 %>% rowwise()  %>%
  mutate(R1_final_code =
           ifelse(
             Count == 1 | (length(modes) == 1 & !is.nan(modes[[1]])),
             modes[[1]],
             ifelse(
               !is.na(!!sym(paste0(
                 question, "jesicatapia"
               ))),!!sym(paste0(question, "jesicatapia")),
               ifelse(
                 !is.na(!!sym(paste0(question, "carmen"))),!!sym(paste0(question, "carmen")),
                 ifelse(
                   !is.na(!!sym(paste0(question, "andrea"))),!!sym(paste0(question, "andrea")),
                   ifelse(
                     !is.na(!!sym(paste0(question, "sandra"))),!!sym(paste0(question, "sandra")),
                     ifelse(!is.na(!!sym(
                       paste0(question, "edithimg")
                     )),!!sym(paste0(
                       question, "edithimg"
                     )), NA_character_)
                   )
                 )
               )
             )
           ))


to_stat_r1 <- R1 %>% select(contains("R1_has"))
#this is nominal because of the weird link answer possiblility
stats_df_rep <-
  bind_rows(
    stats_df_rep,
    get_icr_stats(
      to_stat_r1,
      "R1_has_letter_addressed",
      ICC = FALSE,
      kendall = FALSE,
      kripp.method = "nominal"
    )
  )


final_dups <- final_dups %>%
  left_join(select(R1, folio_id, R1_has_letter_addressed = R1_final_code),
            by = "folio_id")

#WTF
question <- "R2_num_pages_letter_"
R2 <-
  agreeement_cat_fn(wide_dups,
                    "R2_.*",
                    skip_df = R1,
                    skip_col = quo(R1_final_code))

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
  mutate(R2_final_code = ifelse(
    R1_final_code %in% c("No", "Yes, but cannot open link", "End"),
    NA,
    ifelse(
      Count == 1 | (length(modes) == 1 & !is.nan(modes)),
      modes[[1]],
      ifelse(length(modes) > 1,
             ifelse(
               !is.na(!!sym(paste0(
                 question, "jesicatapia"
               ))),!!sym(paste0(question, "jesicatapia")),
               ifelse(
                 !is.na(!!sym(paste0(question, "carmen"))),!!sym(paste0(question, "carmen")),
                 ifelse(
                   !is.na(!!sym(paste0(question, "andrea"))),!!sym(paste0(question, "andrea")),
                   ifelse(!is.na(!!sym(
                     paste0(question, "sandra")
                   )),!!sym(paste0(question, "sandra")),
                   ifelse(!is.na(!!sym(
                     paste0(question, "edithimg")
                   )),!!sym(
                     paste0(question, "edithimg")
                   ), NA))
                 )
               )
             ))
    )
  ))

#makefactor ordinal for conversion
to_stat_r2 <- filter(R2, R1_final_code == "Yes") %>%
  select(contains("R2_num")) %>%
  purrr::map_df( ~ as.numeric(as.factor(.)))



stats_df_rep <-
  bind_rows(
    stats_df_rep,
    get_icr_stats(to_stat_r2, "R2_num_pages_letter", kripp.method = "ordinal")
  )


final_dups <- final_dups %>%
  left_join(select(R2, folio_id, R2_num_pages_letter = R2_final_code), by = "folio_id")


question <- "R3_readability_"
R3 <-
  agreeement_cat_fn(wide_dups,
                    "R3_.*",
                    skip_df = R1,
                    skip_col = quo(R1_final_code))

# R3$R3_final_code <-NA
# R3$R3_final_code <-as.factor(R3$R3_final_code )

#WHAT IS THE PROBLEM HERE??? has to do with
R3 <- R3 %>% rowwise() %>%
  mutate(R3_final_code = ifelse(
    R1_final_code %in% c("No", "Yes, but cannot open link", "End"),
    NA_character_,
    ifelse(
      Count == 1 | (length(modes) == 1 & !is.nan(modes)),
      modes[[1]],
      ifelse(length(modes) > 1,
             ifelse(
               !is.na(!!sym(paste0(
                 question, "jesicatapia"
               ))),!!sym(paste0(question, "jesicatapia")),
               ifelse(
                 !is.na(!!sym(paste0(question, "carmen"))),!!sym(paste0(question, "carmen")),
                 ifelse(
                   !is.na(!!sym(paste0(question, "andrea"))),!!sym(paste0(question, "andrea")),
                   ifelse(
                     !is.na(!!sym(paste0(question, "sandra"))),!!sym(paste0(question, "sandra")),
                     ifelse(!is.na(!!sym(
                       paste0(question, "edithimg")
                     )),!!sym(paste0(
                       question, "edithimg"
                     )), NA_character_)
                   )
                 )
               )
             ))
    )
  ))



# icr2 <- icr2 %>%
#   mutate(R3_readability = fct_relevel(R3_readability, ord_diff_class),
#          R5_num_referenced_requests = fct_relevel(R5_num_referenced_requests, ord_num_req),
#          R7_proportion_answered = fct_relevel(R7_proportion_answered, ord_num_req))

#makefactor ordinal for conversion
to_stat_r3 <- filter(R3, R1_final_code == "Yes")  %>%
  select(contains("R3_re")) %>%
  purrr::map_df( ~ fct_relevel(., ORD_DIFF_CLASS)) %>%
  purrr::map_df( ~ as.numeric(as.factor(.)))


stats_df_rep <-
  bind_rows(stats_df_rep,
            get_icr_stats(to_stat_r3, "R3_readability", kripp.method = "ordinal"))

final_dups <- final_dups %>%
  left_join(select(R3, folio_id, R3_readability = R3_final_code), by = "folio_id") %>%
  dplyr::mutate(R3_readability = fct_relevel(R3_readability, ORD_DIFF_CLASS))



#get all of the dummmy variables that we will group the output by
dummy_r_cols <-
  names(icr2_dups)[grepl(pattern = "R[46].*dummy_.*", x = names(icr2_dups))]

#iterate through all of the dummy R columns.
#I am assuming the skip pattern works for the missing data
#R4, R6, R10
R_bin_list_dummy <- list()
for (i in dummy_r_cols) {
  question <- i
  pattern <- paste0(question, "_.*")
  element <- select(wide_dups, folio_id, matches(pattern)) %>%
    full_join(select(R1, folio_id, R1_final_code)) %>% #make sure folks werent sent to the end
    mutate(total_yes = rowSums(select(., contains(question)), na.rm = TRUE)) %>%
    mutate(non_na = 6 - rowSums(is.na(select(
      ., contains(question)
    ))),
    ratio = total_yes / non_na) %>%
    rowwise() %>%
    mutate(final_code = ifelse(R1_final_code == "End", NA,
                               ifelse(
                                 ratio < .5, 0,
                                 ifelse(ratio > .5, 1,
                                        ifelse(
                                          !is.na(!!sym(paste0(
                                            question, "_jesicatapia"
                                          ))),!!sym(paste0(question, "_jesicatapia")),
                                          ifelse(
                                            !is.na(!!sym(paste0(
                                              question, "_carmen"
                                            ))),!!sym(paste0(question, "_carmen")),
                                            ifelse(
                                              !is.na(!!sym(paste0(
                                                question, "_andrea"
                                              ))),!!sym(paste0(question, "_andrea")),
                                              ifelse(!is.na(!!sym(
                                                paste0(question, "_sandra")
                                              )),!!sym(paste0(
                                                question, "_sandra"
                                              )),
                                              ifelse(!is.na(!!sym(
                                                paste0(question, "_edithimg")
                                              )),!!sym(
                                                paste0(question, "_edithimg")
                                              ), NA))
                                            )
                                          )
                                        ))
                               )))
  
  R_bin_list_dummy[[question]] <- element
}


#only calculate for non missing
R_bin_list_dummy_noNA <-
  R_bin_list_dummy[!grepl("_NA", names(R_bin_list_dummy))] %>%
  purrr::map( ~ filter(., R1_final_code != "End"))

for (i in 1:length(R_bin_list_dummy_noNA)) {
  to_stat_Rdummy <-
    dplyr::select(
      R_bin_list_dummy_noNA[[i]],
      -folio_id,
      -total_yes,
      -non_na,
      -ratio,
      -contains("final_code")
    )
  stats_df_rep <-
    bind_rows(stats_df_rep,
              get_icr_stats(
                to_stat_Rdummy,
                names(R_bin_list_dummy_noNA[i]),
                kripp.method = "nominal"
              ))
  
}

for (i in 1:length(R_bin_list_dummy)) {
  final_dups[, names(R_bin_list_dummy[i])] <-
    R_bin_list_dummy[[i]]$final_code
}


#R5-- this depends on both S4_1 and S4_2
question <- "R5_num_referenced_requests_"
R5 <-
  agreeement_cat_fn(wide_dups, "R5_.*", skip_df = S4_2, skip_match = "S4.*final.*")

R5 <- R5 %>% rowwise() %>%
  mutate(R5_final_code = ifelse(
    S4_1_final_code == 1 | S4_2_final_code == 1 ,
    NA,
    ifelse(
      Count == 1 | (length(modes) == 1 & !is.nan(modes)),
      modes[[1]],
      ifelse(length(modes) > 1,
             ifelse(
               !is.na(!!sym(paste0(
                 question, "jesicatapia"
               ))),!!sym(paste0(question, "jesicatapia")),
               ifelse(
                 !is.na(!!sym(paste0(question, "carmen"))),!!sym(paste0(question, "carmen")),
                 ifelse(
                   !is.na(!!sym(paste0(question, "andrea"))),!!sym(paste0(question, "andrea")),
                   ifelse(!is.na(!!sym(
                     paste0(question, "sandra")
                   )),!!sym(paste0(question, "sandra")),
                   ifelse(!is.na(!!sym(
                     paste0(question, "edithimg")
                   )),!!sym(
                     paste0(question, "edithimg")
                   )))
                 )
               )
             ), NA)
    )
  ))


final_dups <- final_dups %>%
  left_join(select(R5, folio_id, R5_num_referenced_requests = R5_final_code),
            by = "folio_id") %>%
  mutate(R5_num_referenced_requests = fct_relevel(R5_num_referenced_requests, ORD_NUM_REQ))

to_stat_r5 <-
  filter(R5, S4_1_final_code == 1 | S4_2_final_code == 1) %>%
  select(contains("R5_num_referenced_requests")) %>%
  purrr::map_df( ~ fct_relevel(., ORD_NUM_REQ)) %>%
  purrr::map_df( ~ as.numeric(as.factor(.)))

stats_df_rep <-
  bind_rows(
    stats_df_rep,
    get_icr_stats(to_stat_r5, "R5_num_referenced_requests", kripp.method = "ordinal")
  )



#R7 You need to have answered R6  if you sayed tehre was no information then you went to the end
question <- "R7_proportion_answered_"
R7 <-
  agreeement_cat_fn(
    wide_dups,
    "R7_.*",
    skip_df = R_bin_list_dummy$R6_dummy_NoInfo,
    skip_col  = quo(final_code)
  )
R7 <- full_join(R7, select(R1, folio_id, R1_final_code))
R7 <- R7 %>% rowwise() %>%
  mutate(R7_final_code = ifelse((final_code == 1 |
                                   R1_final_code == "End"),
                                NA_character_,
                                ifelse(
                                  Count == 1 | (length(modes) == 1 & !is.nan(modes)),
                                  modes[[1]],
                                  ifelse(
                                    length(modes) > 1,
                                    ifelse(
                                      !is.na(!!sym(paste0(
                                        question, "jesicatapia"
                                      ))),!!sym(paste0(question, "jesicatapia")),
                                      ifelse(
                                        !is.na(!!sym(paste0(question, "carmen"))),!!sym(paste0(question, "carmen")),
                                        ifelse(
                                          !is.na(!!sym(paste0(question, "andrea"))),!!sym(paste0(question, "andrea")),
                                          ifelse(!is.na(!!sym(
                                            paste0(question, "sandra")
                                          )),!!sym(paste0(question, "sandra")),
                                          ifelse(!is.na(!!sym(
                                            paste0(question, "edithimg")
                                          )),!!sym(
                                            paste0(question, "edithimg")
                                          )))
                                        )
                                      )
                                    ),
                                    NA_character_
                                  )
                                )
  ))

#makefactor ordinal for conversion
to_stat_r7 <-
  filter(R7, final_code == 0) %>% #filter if dummy_NoInfo is true
  select(contains("R7_pro")) %>%
  purrr::map_df( ~ fct_relevel(., ORD_NUM_REQ)) %>%
  purrr::map_df( ~ as.numeric(as.factor(.)))


stats_df_rep <-
  bind_rows(
    stats_df_rep,
    get_icr_stats(to_stat_r7, "R7_proportion_answered", kripp.method = "ordinal")
  )

final_dups <- final_dups %>%
  left_join(select(R7, folio_id, R7_proportion_answered = R7_final_code),
            by = "folio_id") %>%
  mutate(R7_proportion_answered  = fct_relevel(R7_proportion_answered, ORD_NUM_REQ))


#R8 - ONLY For those who got a link! -- SKIP FOR NOW
question <- "R8_is_link_correct_"
R8 <-
  agreeement_cat_fn(
    wide_dups,
    "R8_.*",
    skip_df = R_bin_list_dummy$R6_dummy_Links,
    skip_col  = quo(final_code)
  )
R8 <- full_join(R8, select(R1, folio_id, R1_final_code))
R8 <- R8 %>% rowwise() %>%
  mutate(R8_final_code = ifelse((final_code != 1 |
                                   R1_final_code == "End"),
                                NA,
                                ifelse(
                                  Count == 1 | (length(modes) == 1 & !is.nan(modes)),
                                  modes[[1]],
                                  ifelse(length(modes) > 1,
                                         ifelse(
                                           !is.na(!!sym(paste0(
                                             question, "jesicatapia"
                                           ))),!!sym(paste0(question, "jesicatapia")),
                                           ifelse(
                                             !is.na(!!sym(paste0(question, "carmen"))),!!sym(paste0(question, "carmen")),
                                             ifelse(
                                               !is.na(!!sym(paste0(question, "andrea"))),!!sym(paste0(question, "andrea")),
                                               ifelse(!is.na(!!sym(
                                                 paste0(question, "sandra")
                                               )),!!sym(paste0(question, "sandra")),
                                               ifelse(!is.na(!!sym(
                                                 paste0(question, "edithimg")
                                               )),!!sym(
                                                 paste0(question, "edithimg")
                                               )))
                                             )
                                           )
                                         ), NA)
                                )
  ))
# Because small subset
# to_stat_r8 <- filter(R8, R1_final_code == "Yes" | is.na(R1_final_code)) %>%
#   select(contains("R7_pro")) %>%
#   purrr::map_df(~as.numeric(as.factor(.)))
#
#
# stats_df_rep <- bind_rows(stats_df_rep, get_icr_stats(to_stat_r7, "R7_proportion_answered", kripp.method = "ordinal"))


final_dups <- final_dups %>%
  left_join(select(R8, folio_id, R8_is_link_correct = R8_final_code), by = "folio_id")



#R9 --SKip for now since there is a skip pattern
question <- "R9_diff_find_info_"
R9 <-
  agreeement_cat_fn(
    wide_dups,
    "R9_.*",
    skip_df = R_bin_list_dummy$R6_dummy_InLetter,
    skip_col  = quo(final_code)
  )
R9 <-
  full_join(
    R9,
    select(
      R_bin_list_dummy$R6_dummy_Attached,
      folio_id,
      R6_attachment_final_code = final_code
    )
  )
R9 <- full_join(R9, select(R1, folio_id, R1_final_code))
R9 <- R9 %>% rowwise() %>%
  mutate(R9_final_code = ifelse(
    (
      (final_code != 1 &
         R6_attachment_final_code != 1) | R1_final_code == "End"
    ),
    NA,
    ifelse(
      Count == 1 | (length(modes) == 1 & !is.nan(modes)),
      modes[[1]],
      ifelse(length(modes) > 1,
             ifelse(
               !is.na(!!sym(paste0(
                 question, "jesicatapia"
               ))),!!sym(paste0(question, "jesicatapia")),
               ifelse(
                 !is.na(!!sym(paste0(question, "carmen"))),!!sym(paste0(question, "carmen")),
                 ifelse(
                   !is.na(!!sym(paste0(question, "andrea"))),!!sym(paste0(question, "andrea")),
                   ifelse(!is.na(!!sym(
                     paste0(question, "sandra")
                   )),!!sym(paste0(question, "sandra")),
                   ifelse(!is.na(!!sym(
                     paste0(question, "edithimg")
                   )),!!sym(
                     paste0(question, "edithimg")
                   )))
                 )
               )
             ), NA)
    )
  ))



final_dups <- final_dups %>%
  left_join(select(R9, folio_id, R9_diff_find_info = R9_final_code), by = "folio_id")



dummy_r10_cols <-
  names(icr2_dups)[grepl(pattern = "R10.*dummy_.*", x = names(icr2_dups))]
R_bin_list_dummy2 <- list()
for (i in dummy_r10_cols) {
  question <- i
  pattern <- paste0(question, "_.*")
  element <- select(wide_dups, folio_id, matches(pattern)) %>%
    full_join(select(R1, folio_id, R1_final_code)) %>% #make sure folks werent sent to the end
    full_join(
      select(
        R_bin_list_dummy$R6_dummy_NoInfo,
        folio_id,
        R6_no_info_final_code = final_code
      )
    ) %>% #make sure folks got information
    mutate(total_yes = rowSums(select(., contains(question)), na.rm = TRUE)) %>%
    mutate(non_na = 6 - rowSums(is.na(select(
      ., contains(question)
    ))),
    ratio = total_yes / non_na) %>%
    rowwise() %>%
    mutate(final_code = ifelse(R1_final_code == "End", NA,
                               ifelse(
                                 ratio < .5, 0,
                                 ifelse(ratio > .5, 1,
                                        ifelse(
                                          !is.na(!!sym(paste0(
                                            question, "_jesicatapia"
                                          ))),!!sym(paste0(question, "_jesicatapia")),
                                          ifelse(
                                            !is.na(!!sym(paste0(
                                              question, "_carmen"
                                            ))),!!sym(paste0(question, "_carmen")),
                                            ifelse(
                                              !is.na(!!sym(paste0(
                                                question, "_andrea"
                                              ))),!!sym(paste0(question, "_andrea")),
                                              ifelse(!is.na(!!sym(
                                                paste0(question, "_sandra")
                                              )),!!sym(paste0(
                                                question, "_sandra"
                                              )),
                                              ifelse(!is.na(!!sym(
                                                paste0(question, "_edithimg")
                                              )),!!sym(
                                                paste0(question, "_edithimg")
                                              ), NA))
                                            )
                                          )
                                        ))
                               )))
  
  R_bin_list_dummy2[[question]] <- element
}

R_bin_list_dummy2_noNA <-
  R_bin_list_dummy2[!grepl("_NA", names(R_bin_list_dummy2))] %>%
  purrr::map( ~ filter(., R1_final_code != "End"))

for (i in 1:length(R_bin_list_dummy2_noNA)) {
  to_stat_Rdummy <-
    dplyr::select(
      R_bin_list_dummy2_noNA[[i]],
      -folio_id,
      -total_yes,
      -non_na,
      -ratio,
      -contains("final_code")
    )
  stats_df_rep <-
    bind_rows(stats_df_rep,
              get_icr_stats(
                to_stat_Rdummy,
                names(R_bin_list_dummy2_noNA[i]),
                kripp.method = "nominal"
              ))
  
}

for (i in 1:length(R_bin_list_dummy2)) {
  final_dups[, names(R_bin_list_dummy2[i])] <-
    R_bin_list_dummy2[[i]]$final_code
}



#R11
question <- "R11_is_req_interesting_"
R11 <- select(wide_dups, folio_id, matches("R11_.*")) %>%
  mutate(total_yes = rowSums(select(., matches("R11_.*")), na.rm = TRUE)) %>%
  mutate(non_na = 6 - rowSums(is.na(select(
    ., matches("R11_.*")
  ))), ratio = total_yes / non_na) %>%
  rowwise() %>%
  mutate(R11_final_code = ifelse(ratio < .5, 0,
                                 ifelse(
                                   ratio > .5, 1,
                                   ifelse(
                                     !is.na(!!sym(paste0(
                                       question, "jesicatapia"
                                     ))),!!sym(paste0(question, "jesicatapia")),
                                     ifelse(
                                       !is.na(!!sym(paste0(question, "carmen"))),!!sym(paste0(question, "carmen")),
                                       ifelse(
                                         !is.na(!!sym(paste0(question, "andrea"))),!!sym(paste0(question, "andrea")),
                                         ifelse(!is.na(!!sym(
                                           paste0(question, "sandra")
                                         )),!!sym(paste0(question, "sandra")),
                                         ifelse(!is.na(!!sym(
                                           paste0(question, "edithimg")
                                         )), NA))
                                       )
                                     )
                                   )
                                 )))
final_dups <- final_dups %>%
  left_join(select(R11, folio_id, R11_is_req_interesting = R11_final_code),
            by = "folio_id")

final_dups <- final_dups[, mixedsort(names(final_dups))]

write_rds(stats_df_rep, "./data_clean/responses_icr.rds")
write_csv(stats_df_rep, "./data_clean/responses_icr.csv")

write_csv(final_dups, "./data_clean/unique_dups_only.csv")

###BRING DUPLICATES AND NON DUPLICATES BACK TOGETHER
final_nodups <- read_rds("./data_clean/handcoding_once_only.rds")

common_cols <-
  intersect(colnames(final_nodups), colnames(final_dups)) %>% mixedsort() #put everythin in order

all_handcoding <- bind_rows(select(final_nodups,!!!common_cols),
                            select(final_dups,!!!common_cols))

filter(
  all_handcoding,
  R10_dummy_NoInfo == 1 &&
    (
      R10_dummy_MachineEdit == 1 |
        R10_dummy_MachineRead |
        R10_dummy_NotMachineRead == 1
    )
) %>% select(matches("R10"))

#ENFORCE idiosyncratic selection patters and remove NA dummy_cols (maybe remove NA_dummy cols earlier)
all_handcoding <- all_handcoding %>% select(-matches("_NA$")) %>%
  mutate_at(
    vars(
      "R4_dummy_Nonexistent",
      "R4_dummy_NotProcessed",
      "R4_dummy_NotResponsable",
      "R4_dummy_OutsideLaw",
      "R4_dummy_PublicAvailable",
      "R4_dummy_Confidential",
      "R4_dummy_DeliveryElectronic",
      "R4_dummy_DeliveryPerson",
      "R4_dummy_NeedMoreInfo"
    ),
    funs(ifelse(R4_dummy_None == 1, 0, .))
  ) %>%
  mutate_at(vars("R6_dummy_InLetter",
                 "R6_dummy_Links",
                 "R6_dummy_Attached"),
            funs(ifelse(R6_dummy_NoInfo == 1, 0, .))) %>%
  mutate_at(
    vars(
      "R10_dummy_MachineEdit",
      "R10_dummy_MachineRead",
      "R10_dummy_NotMachineRead"
    ),
    funs(ifelse(R10_dummy_NoInfo == 1, 0, .))
  )


#nor rows so looks good
filter(
  all_handcoding,
  R10_dummy_NoInfo == 1 &&
    (
      R10_dummy_MachineEdit == 1 |
        R10_dummy_MachineRead | R10_dummy_NotMachineRead == 1
    )
) %>%
  select(matches("R10"))
write_rds(all_handcoding, "./data_clean/handcoding_forML.rds")
write_csv(all_handcoding, "./data_clean/handcoding_forML.csv")

appendix <- filter(stats_df, stat == "icc") %>%
  filter(grepl("^S6|^S8|^S10|^S11", question)) %>%
  summarise(
    mean = mean(value),
    sd = sd(value),
    min = min(value),
    max = max(value)
  )
