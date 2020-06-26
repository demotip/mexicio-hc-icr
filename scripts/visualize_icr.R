rm(list=ls())
pacman::p_load(tidyverse, ggplot2,forcats)
req_icr <- read_rds(here::here("./data_clean/requests_icr.rds"))

means_icr <- req_icr %>% group_by(question) %>%
  summarise(value = mean(value)) %>%
  mutate(question = fct_reorder(question, value)) %>%
  arrange(value)



icr_viz <- ggplot(req_icr, aes(x = question, y = value)) + 
  geom_point(data =  means_icr, size = 2) +
  geom_point(aes(group = question, shape = stat, colour =stat), alpha = .6) +
  geom_hline(yintercept = .6, colour = "grey8", linetype = "dashed") +
  coord_flip() +
  theme_bw()

ggsave("./figs/icr_requests.pdf", icr_viz)
