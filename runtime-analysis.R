library(tidyverse)
library(gt)

theme_set(theme_bw())

all_ert <- read_csv("all_ert.csv") %>%
  select(-"...1") %>% 
  mutate(algorithm = factor(algorithm, levels = c("VBS", "GRS", "GRS-LOPO", "64L", "32L", "16L", "8L", "4L", "2L", "default", "L-BFGS-B", "Powell", "SLSQP")))

vbs_tbl <- all_ert %>% 
  mutate(ert = pmin(ert, 10**7 * dim)) %>% 
  filter(!(algorithm %in% c("GRS", "GRS-LOPO", "VBS"))) %>% 
  group_by(fid, dim, target) %>% 
  summarize(vbs = min(ert)) %>% 
  ungroup()

impute_all_ert <- all_ert %>% 
  left_join(vbs_tbl, by = c("fid", "dim", "target")) %>% 
  mutate(ert = pmin(ert, 10**7 * dim))

impute_all_ert %>% 
  group_by(dim, algorithm) %>% 
  summarize(mean_ert = mean(ert), gmean_ert = exp(mean(log(ert))), mean_logert = mean(log10(ert)), mean_relert = mean(ert / vbs)) %>%
  arrange(dim, mean_ert) %>% View() # %>%  filter(algorithm == "GRS-LOPO") %>% summarize(mean(mean_logert), mean(mean_relert)) %>% View()

impute_all_ert %>% 
  filter(dim == 10, algorithm != "GRS-LOPO") %>% 
  ggplot(aes(fid, target, fill = ert / vbs)) +
  geom_tile() +
  facet_wrap(vars(algorithm)) +
  scale_fill_gradientn(colors = viridisLite::viridis(500), transform = "log10") +
  scale_x_continuous(breaks = c(1,5,10,15,20,24), name = "FID") +
  scale_y_continuous(breaks = c(2,0,-2,-4,-6,-8), name = "log10(target)") +
  labs(fill = "relERT") +
  theme(legend.position = "bottom")

ggsave("heatmap-d10-relERT.pdf", width = 6, height = 6)


impute_all_ert %>% 
  filter(algorithm %in% c("VBS", "GRS", "GRS-LOPO")) %>% 
  ggplot(aes(fid, target, fill = ert)) +
  geom_tile() +
  facet_grid(vars(dim), vars(algorithm)) +
  scale_fill_gradientn(colors = viridisLite::viridis(500), transform = "log10") +
  scale_x_continuous(breaks = c(1,5,10,15,20,24), name = "FID") +
  scale_y_continuous(breaks = c(2,0,-2,-4,-6,-8), name = "log10(target)") +
  labs(fill = "ERT") +
  theme(legend.position = "bottom")

ggsave("heatmap-lopo.pdf", width = 6, height = 6)
