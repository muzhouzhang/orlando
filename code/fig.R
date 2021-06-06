setwd("/Users/mz/Box/myBox/orlando")

library(tidyverse)
library(cowplot)

load("data/output.RData")

# fig 1----
fig1_dat %>%
  mutate(
    sample = c(
      "Full sample",
      "Narrow time window",
      "Proximity",
      "Reasonable duration",
      "Intersection of\nNarrow time window and Reasonable duration",
      "Intersection of\nProximity and Reasonable duration"
    ),
    sample = factor(sample, levels = rev(unique(sample)))
  ) %>%
  ggplot(aes(fd_fig1, sample)) +
  geom_pointrange(aes(xmin = cilow95, xmax = cihigh95), size = 1.5) +
  geom_linerange(aes(xmin = cilow90, xmax = cihigh90)) +
  geom_errorbarh(
    aes(xmin = cilow95, xmax = cihigh95),
    height = 0.20, size = 1.5
  ) +
  geom_errorbarh(
    aes(xmin = cilow90, xmax = cihigh90),
    height = 0.40, size = 1.5
  ) +
  geom_vline(xintercept = 0, linetype = 2, size = 1.5) +
  labs(x = "", y = "") +
  theme_minimal_vgrid() +
  theme(
    text = element_text(family = "Times"),
    panel.grid.major = element_line(linetype = 2, size = 1.5),
    axis.line.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 25)
  )
ggsave("fig/fig1.pdf", height = 10, width = 15)

# fig 2 ----
fig2_dat %>%
  mutate(
    sample = c(
      "Full sample",
      "Narrow time window",
      "Proximity",
      "Reasonable duration",
      "Intersection of\nNarrow time window and Reasonable duration",
      "Intersection of\nProximity and Reasonable duration"
    ),
    sample = factor(sample, levels = rev(unique(sample)))
  ) %>%
  ggplot(aes(fd_fig2, sample)) +
  geom_pointrange(aes(xmin = cilow95, xmax = cihigh95), size = 1.5) +
  geom_linerange(aes(xmin = cilow90, xmax = cihigh90)) +
  geom_errorbarh(aes(xmin = cilow95, xmax = cihigh95), height = 0.20, size = 1.5) +
  geom_errorbarh(aes(xmin = cilow90, xmax = cihigh90), height = 0.40, size = 1.5) +
  geom_vline(xintercept = 0, linetype = 2, size = 1.5) +
  # scale_y_discrete(limits = rev(levels(sample))) +
  labs(x = "", y = "") +
  theme_minimal_vgrid() +
  theme(
    text = element_text(family = "Times"),
    panel.grid.major = element_line(linetype = 2, size = 1.5),
    axis.line.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 25)
  )
ggsave("fig/fig2.pdf", height = 10, width = 15)
