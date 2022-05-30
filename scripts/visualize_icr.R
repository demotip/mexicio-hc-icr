rm(list = ls())
pacman::p_load(tidyverse, ggplot2, forcats)
req_icr <- read_rds(here::here("./data_clean/requests_icr.rds"))
rep_icr <- read_rds(here::here("./data_clean/responses_icr.rds"))

means_icr_req <- req_icr %>% group_by(question) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>%
  mutate(question = fct_reorder(question, value)) %>%
  arrange(value)

means_icr_rep <- rep_icr %>% group_by(question) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>%
  mutate(question = fct_reorder(question, value)) %>%
  arrange(value)

icr_viz_req <- ggplot(req_icr, aes(x = question, y = value)) +
  geom_point(data =  means_icr_req, size = 2, shape  = 1) +
  geom_point(aes(
    group = question,
    shape = stat,
    colour = stat
  ), alpha = .6) +
  geom_hline(yintercept = .6,
             colour = "grey8",
             linetype = "dashed") +
  coord_flip() +
  theme_bw()

icr_viz_rep <- ggplot(rep_icr, aes(x = question, y = value)) +
  geom_point(data =  means_icr_rep, size = 2, shape  = 1) +
  geom_point(aes(
    group = question,
    shape = stat,
    colour = stat
  ), alpha = .6) +
  geom_hline(yintercept = .6,
             colour = "grey8",
             linetype = "dashed") +
  coord_flip() +
  theme_bw()


ggsave("./figs/icr_requests.pdf", icr_viz_req, width = 9,units = "in")
ggsave("./figs/icr_responses.pdf", icr_viz_rep, width = 9,units = "in")

