rm(list= ls())
pacman::p_load(dplyr, stringr, readr, irr, kableExtra, irrNA, lubridate, splitstackshape, statip, fastDummies)
#devtools::install_github("tidyverse/tidyr")
library(tidyr)
#deal with weird Qualtrics export by removing two rows
icr <- read_csv("./data_raw/Big Data y Acceso a Info en México_September 4, 2019_09.43.csv")
icr <- slice(icr, 3:n()) %>% rename(RecipientEmail = `RecipientEmail...12`)

sampled_ids <- read_rds("./data_raw/full_sample_post_p1b4.rds")

#deal with problematic cases
icr2 <- icr %>% 
  filter(!(is.na(RecipientEmail)), Progress==100) 

#cleanup folio_id
icr2 <- mutate(icr2, folio_id = ifelse(grepl("'", S1),
                                       trimws(S1),
                                       trimws(str_c("'", S1))))

#
#new_S11 <- paste0("S11_", 1:6)
#icr2s <- separate(icr2, S11,  into = new_S11, sep= "\\W,\\W", "fi")


#create  coder_id out of email
icr2 <- mutate(icr2, coder_id = str_extract(RecipientEmail, "^[[:alnum:]]+"))

#omit earlier coding
icr2 <- icr2 %>%
  mutate(start_date = date(StartDate)) %>%
  filter(start_date >= date("2019-06-08") )

#get rid of duplicated by taking the one with the last start time
icr2 <- icr2 %>% group_by(folio_id, coder_id ) %>%
  filter(StartDate==max(StartDate)) %>%
  mutate(duplicate = n()>1) %>%
  ungroup()

#check
stopifnot(sum(icr2$duplicate) ==0)

#THESES ARE VARIABLES WITH MULTIPLE POSSIBLE ANSWERS (check_alls)
##EACH ONE OF THESE will be stored as R/S[NUM]_dummy_*** in the final dataset

check_all_vars <- c("S7", "S8", "S11", "R4", "R6", "R10")

#split the string. there are not any spaces, so it's a pain and `Csplit only takes one space'`
#use a pipe to split. 
icr2 <- icr2 %>%
  mutate_at(check_all_vars,
            funs(new = (str_replace_all(. ,
                                        pattern =  "([[:alnum:],\\.\\)])(,)([[:ALPHA:]])",
                                        replace = "\\1 | \\3"))))

# icr2$S11_new <- str_replace_all(icr2$S11,  
#                             pattern= "([[:alnum:],\\.\\)])(,)([[:ALPHA:]])", 
#                             replace = "\\1 | \\3")
# 
# icr2$S7_new <- str_replace_all(icr2$S7,  
#                             pattern= "([[:alnum:],\\.\\)])(,)([[:ALPHA:]])", 
#                             replace = "\\1 | \\3")
# 
# icr2$S8_new <- str_replace_all(icr2$S8,  
#                            pattern= "([[:alnum:],\\.\\)])(,)([[:ALPHA:]])", 
#                            replace = "\\1 | \\3")
# 
# icr2$R4_new <- str_replace_all(icr2$R4,  
#                                pattern= "([[:alnum:],\\.\\)])(,)([[:ALPHA:]])", 
#                                replace = "\\1 | \\3")


#s11_dummies <- dummy_cols(icr2, select_columns = c("S11"),  split= " BLOCKW ")
#dummy_names <- names(select(icr2, contains("S11_")))


#put in different columns
icr2 <-cSplit(icr2, "S11_new", sep= "|", direction = "wide", fixed = TRUE, makeEqual = TRUE)
icr2 <-cSplit(icr2, "S8_new", sep= "|", direction = "wide", fixed = TRUE, makeEqual = TRUE)
icr2 <-cSplit(icr2, "S7_new", sep= "|", direction = "wide", fixed = TRUE, makeEqual = TRUE)
icr2 <-cSplit(icr2, "R10_new", sep= "|", direction = "wide", fixed = TRUE, makeEqual = TRUE)
icr2 <-cSplit(icr2, "R6_new", sep= "|", direction = "wide", fixed = TRUE, makeEqual = TRUE)
icr2 <-cSplit(icr2, "R4_new", sep= "|", direction = "wide", fixed = TRUE, makeEqual = TRUE)

#recode and recombine
s11_names <- names(select(icr2, contains("S11_new")))
s8_names <- names(select(icr2, contains("S8_new")))
s7_names <- names(select(icr2, contains("S7_new")))
r4_names <- names(select(icr2, contains("R4_new")))
r6_names <- names(select(icr2, contains("R6_new")))
r10_names <- names(select(icr2, contains("R10_new")))


#R5 and R7 are off by a letter on Todas
icr2 <- icr2 %>% 
  mutate(S3 = recode(S3,
                     `Más de 5` = "More than 5",
                     `No se puede abrir el anexo` = "Cannot open"),
         S5 = recode(S5, 
                     `Algo relacionadas` = "Somewhat releated",
                     `Muy relacionadas` = "Very related",
                     `Poca o nada relacionadas` = "Little or not at all related"),
         S9 = recode(S9,
                     `Comercial` = "Commercial",
                     `Imposible de definir` = "Impossible to say",
                     `Investigación académica, proyectos escolar` = "Academic/Scholarly",
                     `Monitoreo (periodismo, sociedad civil, auditoría)` = "Monitoring",
                     `Personal` = "Personal"),
         R2 = gsub(".*de.*3.*", "More than 3", x = R2),
         R3 = gsub(".*lenguaje.*com.*", "Very easy", x = R3),
         R3 = gsub(".*Muy.*dif.*", "Very difficult", x = R3),
         R5 = recode(R5, `Aproximadamente la mitad` = "Approximately half",
                    `La mayoría` = "The majority", 
                    `Menos de la mitad` = "Less than half",
                    `Poca o nada` = "Little or none",
                    `Todas` = "All",
                   .default = NA_character_),
         R7 = recode(R7, `Aproximadamente la mitad` = "Approximately half",
                     `La mayoría` = "The majority", 
                     `Menos de la mitad` = "Less than half",
                     `Poca o nada` = "Little or none",
                     `Toda` = "All",
                     .default = NA_character_),
         R1 = recode(R1, `No` = "No", 
                    `Sí` = "Yes", 
                    `Sí, pero no se puede abrir el enlace al escrito` = "Yes, but cannot open link",
                    `Terminar protocolo: (1) No se puede abrir el anexo de respuesta y la respuesta no se encuentra en la celda de "texto respuesta" O (2) Toda la información proporcionada se entregó en físico` = "End",
                    .default = NA_character_),
         R8 = recode(R8, 
                      `El enlace no funciona` = "Link broken",
                      `No` = "No",
                      `Sí` = "Yes",
                      .default = NA_character_),
         R9 = recode(R9, `Fácil` = "Easy",
         `Muy difícil (secciones ilegibles o confusas)` = "Very difficult",
         `No se proporcionó información` = "No information provided",
         `Un poco difícil` = "A little difficult"),
         R11 = recode(R11,
                      `No` = 0,
                      `Sí, ¿Por qué?` = 1,
                      .default = NA_real_)) %>%
  mutate_at(vars(contains("S2")), funs(recode(., `No` = 0, `Si` = 1, .default = NaN))) %>%  #careful S2 has no accent
  mutate_at(vars(matches("S[61]0?_[0-9]")), funs(recode(., `No` = 0, `Sí` = 1, .default = NaN))) %>% # these have an accent!
  mutate_at(s11_names,
            funs(recode(.,
                        `Un documento específico por nombre o número`= "Document", 
                        `Un lugar específico (una referencia más específica que el nombre de un estado, por ejemplo, un municipio, colonia, etc.)`="Place",
                        `Una fecha específica, cuando la información fue generada o cuando ocurrió un evento (una referencia temporal más específica que un año)` = "Date",
                        `Una institución pública específica (una referencia más específica que el nombre de la dependencia, por ejemplo, una subsecretaría o área interna)` = "Institution",
                        `Una organización no gubernamental específica (por ejemplo, una empresa u ONG)` = "Organization",
                        `Una persona específica por nombre o por título` = "Person",
                        `AnswerDisplayOrder` = NA_character_,
                        .default = NA_character_))) %>%
  mutate_at(s7_names,
            funs(recode(.,
                        `Datos agregados (base de datos)`= "Database", 
                        `Múltiples datos`="Data",
                        `Múltiples documentos` = "MultipleDocuments",
                        `Un dato` = "Datum",
                        `Un documento` = "Document",
                        `AnswerDisplayOrder` = NA_character_,
                        .default = NA_character_))) %>%
  mutate_at(s8_names,
            funs(recode(.,
                        `Estructura de la institución, personal, recursos humanos` = "InstStruc",
                        `Regulatorio, permisos`= "Regulatory", 
                        `Presupuesto, gasto`="Budget",
                        `Evaluaciones, estadísticas y resultados` = "Evaluation",
                        `Actividades de la institución` = "Activities",
                        `Contrataciones con externos` = "ExternalContracts",
                        `Otro, ¿cuál?` = "Other",
                        .default = NA_character_))) %>%
  mutate_at(r4_names,
            funs(recode(.,
                        `Entrega de información en medio electrónico` = "DeliveryElectronic",
                        `Entrega o consulta en físico y/o pago requerido para parte o toda la información`= "DeliveryPerson", 
                        `Inexistencia de la información`="Nonexistent",
                        `La información está disponible públicamente` = "PublicAvailable",
                        `La solicitud no corresponde al marco de la ley` = "OutsideLaw",
                        `Negativa por ser reservada o confidencial (incluye parcialmente reservada o confidencial)` = "Confidential",
                        `Ninguna` = "None",
                        `No es de competencia de la Unidad de Enlace` = "NotResponsable",
                        `No se dará trámite a la solicitud` = "NotProcessed",
                        `Requerimiento de información adicional` = "NeedMoreInfo",
                        .default = NA_character_))) %>%
  mutate_at(r6_names,
            funs(recode(.,
                        `Dato(s) en escrito al solicitante` = "InLetter",
                        `Documento(s) anexo(s)`= "Attached", 
                        `Enlace(s) a página web`="Links",
                        `No se proporcionó información` = "NoInfo",
                        .default = NA_character_))) %>%
  mutate_at(r10_names,
            funs(recode(.,
                        `Documento manipulable (por ejemplo: Excel, Word)` = "MachineEdit",
                        `Documento no reutilizable (por ejemplo: PDF solo imagen)`= "NotMachineRead", 
                        `Documento reutilizable (por ejemplo: PDF que permite copiar y pegar)`="MachineRead",
                        `No se proporcionó información` = "NoInfo",
                        .default = NA_character_))) %>%
  unite("S7_dummy", !!!s7_names) %>%
  unite("S8_dummy", !!!s8_names) %>%
  unite("S11_dummy", !!!s11_names) %>%
  unite("R4_dummy", !!!r4_names) %>%
  unite("R6_dummy", !!!r6_names) %>%
  unite("R10_dummy", !!!r10_names)


#now make into dummy columns
icr2 <- dummy_cols(icr2, select_columns = c("S11_dummy"), ignore_na = TRUE, split = "_")
icr2 <- dummy_cols(icr2, select_columns = c("S8_dummy"), ignore_na = TRUE, split = "_")
icr2 <- dummy_cols(icr2, select_columns = c("S7_dummy"), ignore_na = TRUE, split = "_")
icr2 <- dummy_cols(icr2, select_columns = c("R4_dummy"), ignore_na = TRUE, split = "_")
icr2 <- dummy_cols(icr2, select_columns = c("R6_dummy"), ignore_na = TRUE, split = "_")
icr2 <- dummy_cols(icr2, select_columns = c("R10_dummy"), ignore_na = TRUE, split = "_")

icr2 <- icr2 %>%
  dplyr::rename(
        Duration = `Duration (in seconds)`,
        S2_has_attachment = S2,
        S3_num_attachment_pages = S3,
        S4_num_info_queries = S4_1,
        S4_num_areas = S4_2,
        S5_requests_related = S5,
        S6_is_formal = S6_1,
        S6_is_legal = S6_2,
        S6_is_technical = S6_3,
        S6_is_aggressive = S6_4,
        S9_likely_use = S9,
        S10_is_clear = S10_1,
        S10_is_competency = S10_2,
        S10_is_public = S10_3,
        S10_is_existant = S10_4,
        R1_has_letter_addressed = R1,
        R2_num_pages_letter = R2,
        R3_readability = R3,
        R5_num_referenced_requests = R5,
        R7_proportion_answered = R7,
        R8_is_link_correct  = R8,
        R9_diff_find_info = R9,
        R11_is_req_interesting = R11)

# ord_diff_class <- c("Very easy", "Normal", "Very difficult")
# ord_num_req <- c("Little or none", "Less than half", "Approximately half", "The majority", "All")
# icr2 <- icr2 %>% 
#   mutate(R3_readability = fct_relevel(R3_readability, ord_diff_class),
#          R5_num_referenced_requests = fct_relevel(R5_num_referenced_requests, ord_num_req),
#          R7_proportion_answered = fct_relevel(R7_proportion_answered, ord_num_req))

#get all the double coded things and then use majority rule for those observations
icr2 <- icr2 %>% add_count(folio_id)
#check if the same coder coded the same folio multiple times
icr2 <- icr2 %>% add_count(folio_id, coder_id, name = "dup_code_folio") 

# icr2 %>% 
#   dplyr::select(StartDate, EndDate, Status, IPAddress, Progress, Duration, Finished,RecordedDate, ResponseId,
# RecipientLastName, RecipientFirstName, RecipientEmail, ExternalReference, LocationLatitude, LocationLongitude,
# DistributionChannel, UserLanguage, 
#   matches("S[1-9].*"),
#   matches("R[1-9].*"))

#could put in correct order

write_csv(x = icr2, "./data_clean/handcoding_all.csv")
write_rds(x = icr2, "./data_clean/handcoding_all.rds")


