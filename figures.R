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

maxruns <- 100

all_runtimes <- rbind(
  cbind(generate_runtimes(10, 0.2, maxruns), Algorithm = "A1", Problem = "P1"),
  cbind(generate_runtimes(10, 0.05, maxruns), Algorithm = "A1", Problem = "P2"),
  cbind(generate_runtimes(10, 0.05, maxruns), Algorithm = "A2", Problem = "P1"),
  cbind(generate_runtimes(10, 0.2, maxruns), Algorithm = "A2", Problem = "P2"),
  cbind(generate_runtimes(10, c(0.2, 0.05), maxruns), Algorithm = "Schedule", Problem = "P1"),
  cbind(generate_runtimes(10, c(0.05, 0.2), maxruns), Algorithm = "Schedule", Problem = "P2")
  # cbind(generate_runtimes(10, c(0.125), maxruns), Algorithm = "Random", Problem = "P1"),
  # cbind(generate_runtimes(10, c(0.125), maxruns), Algorithm = "Random", Problem = "P2")
)

all_runtimes <- all_runtimes %>% 
  mutate(Algorithm = factor(Algorithm, levels = c("A1", "Schedule", "A2")))

# mean_runtimes <- all_runtimes %>% 
#   filter(Algorithm %in% c("A1", "A2")) %>% 
#   group_by(Problem, fevals) %>% 
#   summarize(solved = mean(solved)) %>% 
#   mutate(Algorithm = "Random")
# 
# all_runtimes <- rbind(all_runtimes, mean_runtimes)

# Summarize across Problems
all_ecdf <- all_runtimes %>% 
  group_by(Algorithm, fevals) %>% 
  summarize(solved = mean(solved)) %>%
  mutate(Problem = "All")

rbind(all_runtimes, all_ecdf) %>% mutate(Problem = factor(Problem, levels = c("P1", "P2", "All"))) %>% 
  ggplot(aes(fevals, solved, color = Algorithm, linetype = Algorithm)) +
  geom_step() +
  facet_wrap(~Problem) +
  scale_color_viridis_d() +
  scale_linetype_manual(values = c("dotted", "solid", "dashed")) +
  theme(legend.position = "bottom") +
  scale_x_log10() +
  labs(x = "Function Evaluations", y = "Proportion of Problems Solved")

ggsave("ecdf-miniex.pdf", width = 10, height = 3)

solved <- all_runtimes %>% filter(Algorithm == "Random", Problem == "P1") %>% pull(solved)
fevals <- all_runtimes %>% filter(Algorithm == "Random", Problem == "P1") %>% pull(fevals)

(diff(solved) * fevals[2:length(solved)]) %>% sum()



evals_schedule <- rbind(
  read_csv("expected_evals_schedule_2D.csv"),
  read_csv("expected_evals_schedule_3D.csv"),
  read_csv("expected_evals_schedule_5D.csv"),
  read_csv("expected_evals_schedule_10D.csv")
) %>% mutate(id = row_number())

evals_schedule <- evals_schedule %>%
  mutate(algorithm = factor(algorithm, levels = c("64L", "32L", "16L", "8L", "4L", "2L", "default", "L-BFGS-B", "Powell", "SLSQP")))

evals_schedule %>% 
  ggplot(aes(x = expected_evals, y = factor(dim), group = -id, fill = algorithm)) +
  geom_bar(stat = "identity", orientation = "y") +
  labs(x = "Expected Evaluations", y = "Dimension", fill = "Algorithm") +
  facet_wrap(vars(dim), nrow = 4, scales = "free") +
  scale_fill_viridis_d() +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    legend.position = "bottom"
  )

ggsave("schedule_1k_illustration.pdf", width = 5, height = 5)
 
