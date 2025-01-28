library(tidyverse)

theme_set(theme_bw())

generate_runtimes <- function(fes, succ, maxruns) {
  cum_unsolved <- c(1, cumprod(rep(1 - succ, length.out = maxruns)))
  cum_runtime <- c(0, cumsum(rep(fes, length.out = maxruns)))
  
  data.frame(
    fevals = cum_runtime,
    solved = 1 - cum_unsolved
  )
}

maxruns <- 1000

all_runtimes <- rbind(
  cbind(generate_runtimes(10, 0.2, maxruns), Algorithm = "A1", Problem = "P1"),
  cbind(generate_runtimes(10, 0.05, maxruns), Algorithm = "A1", Problem = "P2"),
  cbind(generate_runtimes(10, 0.05, maxruns), Algorithm = "A2", Problem = "P1"),
  cbind(generate_runtimes(10, 0.2, maxruns), Algorithm = "A2", Problem = "P2"),
  cbind(generate_runtimes(10, c(0.2, 0.05), maxruns), Algorithm = "GRS", Problem = "P1"),
  cbind(generate_runtimes(10, c(0.05, 0.2), maxruns), Algorithm = "GRS", Problem = "P2")
  # cbind(generate_runtimes(10, c(0.125), maxruns), Algorithm = "Random", Problem = "P1"),
  # cbind(generate_runtimes(10, c(0.125), maxruns), Algorithm = "Random", Problem = "P2")
)

all_runtimes <- all_runtimes %>% 
  mutate(Algorithm = factor(Algorithm, levels = c("A1", "GRS", "A2")))

# mean_runtimes <- all_runtimes %>% 
#   filter(Algorithm %in% c("A1", "A2")) %>% 
#   group_by(Problem, fevals) %>% 
#   summarize(solved = mean(solved)) %>% 
#   mutate(Algorithm = "Random")
# 
# all_runtimes <- rbind(all_runtimes, mean_runtimes)

# Summarize across Problems
all_runtimes %>% 
  group_by(Algorithm, fevals) %>% 
  summarize(solved = mean(solved)) %>% 
  ggplot(aes(fevals, solved, color = Algorithm)) +
  geom_step() +
  scale_color_viridis_d() +
  scale_x_log10()

ggplot(all_runtimes %>% filter(Problem == "P1"), aes(fevals, solved, color = Algorithm)) +
  geom_step() +
  scale_color_viridis_d() +
  scale_x_log10()

ggplot(all_runtimes %>% filter(Problem == "P2"), aes(fevals, solved, color = Algorithm)) +
  geom_step() +
  scale_color_viridis_d() +
  scale_x_log10()

solved <- all_runtimes %>% filter(Algorithm == "Random", Problem == "P1") %>% pull(solved)
fevals <- all_runtimes %>% filter(Algorithm == "Random", Problem == "P1") %>% pull(fevals)

(diff(solved) * fevals[2:length(solved)]) %>% sum()
