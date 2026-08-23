# Code for Statistical analysis and Plots from 
# Hauser et al., 2026

# Load Packages -----------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(lme4)
library(car)
library(emmeans)
library(ggpubr)

# Load Data ---------------------------------------------------------------
sfoae <- read_csv("sfoae_data.csv")
dpoae <- read_csv("dpoae_data.csv")
oae <- inner_join(sfoae, dpoae, by = c("Subject", "Sedated", "freq", "Sex"))

oneChin <- read_csv("./singleChin.csv")

info <- read_csv("./chin_info.csv")
info <- info %>% rename(Subject = subject) %>% select(Subject, est_age_months)

oae <- left_join(oae, info, by = "Subject")

# Set up data structure ---------------------------------------------------

oae$freqFactor <- as.ordered(as.factor(oae$freq))
oae$Sedated <- as.factor(oae$Sedated)
oae <- rename(oae, amp_sf = amp.x, amp_dp = amp.y)
oae$Sex <- as.factor(oae$Sex)
oae$Subject <- as.factor(oae$Subject)

# Stats -------------------------------------------------------------------

# Create linear models for each outcome variable
mod_dp <- lmer(amp_dp ~ freqFactor * Sedated + (1|Subject), data = oae)
mod_sf <- lmer(amp_sf ~ freqFactor * Sedated + (1|Subject), data = oae)
mod_qerb <- lmer(qerb ~ freqFactor * Sedated + (1|Subject), data = oae)

mod_dp2   <- lmer(amp_dp ~ freqFactor * Sedated + (1 + Sedated | Subject), data = oae)
mod_sf2   <- lmer(amp_sf ~ freqFactor * Sedated + (1 + Sedated | Subject), data = oae)
mod_qerb2   <- lmer(qerb ~ freqFactor * Sedated + (1 + Sedated | Subject), data = oae)

# ANVOAs for each 
Anova(mod_dp, test.statistic = 'F')
Anova(mod_sf, test.statistic = 'F')
Anova(mod_qerb, test.statistic = 'F')

# Frequency by Frequency comparisons
em_dp <- emmeans(mod_dp, ~  Sedated | freqFactor)
em_sf <- emmeans(mod_sf, ~  Sedated | freqFactor)
em_qerb <- emmeans(mod_qerb, ~  Sedated | freqFactor)

# Save stats into dataframe
stats_dp <- as.data.frame(pairs(em_dp, adjust = "none"))
stats_sf <- as.data.frame(pairs(em_sf, adjust = "none"))
stats_qerb <- as.data.frame(pairs(em_qerb, adjust = "none"))


# Conservative Models
mod_dp2   <- lmer(amp_dp ~ freqFactor * Sedated + (1 + Sedated | Subject), data = oae)

Anova(mod_dp2, test.statistic = 'F')
em_dp2 <- emmeans(mod_dp2, ~  Sedated | freqFactor)
stats_dp2 <- as.data.frame(pairs(em_dp2, adjust = "none"))

# Build significance labels -----------------------------------------------
make_pval_labels <- function(stats_df) {
  p <- stats_df$p.value
  stats_df %>%
    mutate(
      significance = case_when(
        p < 0.001 ~ "***",
        p < 0.01  ~ "**",
        p < 0.05  ~ "*",
        TRUE      ~ ""
      ),
      freq = as.numeric(as.character(freqFactor))
    )
}

pval_dp   <- make_pval_labels(stats_dp)
pval_sf   <- make_pval_labels(stats_sf)
pval_qerb <- make_pval_labels(stats_qerb)


# Plot Settings -----------------------------------------------------------

# plot settings
lw <- 1
col_sed <- '#4393c3'
col_awk <- '#d6604d'

# Figure 1: Single Chin ---------------------------------------------------

## Fig. 1A — DPOAE
oneChinDP <-
  ggplot(data = oneChin) +
  geom_line(aes(x = freq, y = A_dpOAE), linewidth = lw, color = col_awk) +
  geom_line(aes(x = freq, y = S_dpOAE), linewidth = lw, color = col_sed, linetype = '2121') +
  geom_line(aes(x = freq, y = A_dpNF),  linewidth = lw/2, color = col_awk, linetype = 'solid') +
  geom_line(aes(x = freq, y = S_dpNF),  linewidth = lw/2, color = col_sed, linetype = '2121') +
  xlab("Frequency (kHz)") +
  ylab("Amplitude (dB EPL)") +
  ylim(-40, 50) +
  ggtitle("DPOAE") +
  scale_x_continuous(trans = 'log10',
                     breaks = c(.5, 1, 2, 4, 8, 12),
                     labels = c(".5", "1", "2", "4", "8", "12")) +
  theme_bw() +
  theme(text = element_text(size = 11, family = "sans"),
        legend.position = "top", legend.title = element_blank())

## Fig. 1B — SFOAE
oneChinSF <-
  ggplot(data = oneChin) +
  geom_line(aes(x = freq, y = A_sfOAE), linewidth = lw, color = col_awk) +
  geom_line(aes(x = freq, y = S_sfOAE), linewidth = lw, color = col_sed, linetype = '2121') +
  geom_line(aes(x = freq, y = A_sfNF),  linewidth = lw/2, color = col_awk, linetype = 'solid') +
  geom_line(aes(x = freq, y = S_sfNF),  linewidth = lw/2, color = col_sed, linetype = '2121') +
  xlab("Frequency (kHz)") +
  ylab("Amplitude (dB EPL)") +
  ylim(-40, 50) +
  ggtitle("SFOAE") +
  scale_x_continuous(trans = 'log10',
                     breaks = c(.5, 1, 2, 4, 8, 12),
                     labels = c(".5", "1", "2", "4", "8", "12")) +
  annotate("text", x = 14, y = 47, label = "Awake",   size = 4, hjust = "right",
           color = col_awk, family = "sans") +
  annotate("text", x = 14, y = 37, label = "Sedated", size = 4, hjust = "right",
           color = col_sed, family = "sans") +
  theme_bw() +
  theme(text = element_text(size = 11, family = "sans"),
        legend.position = "top")

ggarrange(oneChinDP, oneChinSF + rremove("ylab"),
          labels = c("A", "B"),
          ncol = 2, nrow = 1,
          align = 'v',
          common.legend = TRUE)

## Print Figure
ggsave("./figs/fig1.tiff",
       plot = last_plot(),
       width = 170,
       height = 60,
       units = "mm",
       dpi = 600)

# Figure 2: DPOAE Awake vs Sedated -------------------------------------------------

## Summarize by condition and frequency
dp_sum <- oae %>%
  group_by(Sedated, freq) %>%
  summarise(avg = mean(amp_dp, na.rm = TRUE),
            std = sd(amp_dp,   na.rm = TRUE),
            n   = n(),
            sem = std / sqrt(n))

## Join significance labels
dp_sum <- left_join(dp_sum, pval_dp %>% select(freq, significance), by = "freq")

## Fig. 2A — raw amplitudes awake and sedated
Fig2a_DP <-
  ggplot(data = oae) +
  geom_line(linewidth = .5, alpha = .2,
            aes(x = freq, y = amp_dp, color = Sedated,
                group = interaction(Subject, Sedated), linetype = Sedated)) +
  geom_line(linewidth = .75, data = dp_sum,
            aes(x = freq, y = avg, color = Sedated, linetype = Sedated)) +
  geom_errorbar(data = dp_sum,
                aes(x = freq, ymax = avg + sem, ymin = avg - sem, color = Sedated),
                width = .03, linewidth = .75) +
  geom_text(data = dp_sum %>% filter(Sedated == "Sedated"),
            aes(x = freq, y = avg + sem + 2, label = significance),
            size = 4) +
  xlab("Frequency (kHz)") +
  ylab("Amplitude (dB EPL)") +
  scale_x_continuous(trans = 'log10',
                     breaks = c(.5, 1, 2, 4, 8, 12),
                     labels = c(".5", "1", "2", "4", "8", "12")) +
  scale_color_manual(name = "Status",
                     values = c("Awake" = col_awk, "Sedated" = col_sed),
                     labels = c("Awake", "Sedated")) +
  scale_linetype_manual(name = "Status",
                        values = c("Awake" = "solid", "Sedated" = "3121"),
                        labels = c("Awake", "Sedated")) +
  theme_bw() +
  theme(text = element_text(size = 11, family = "sans"),
        legend.position = "top", legend.title = element_blank())

## Compute sedation difference per subject
dp_diff <- oae %>%
  group_by(Subject, freq) %>%
  summarize(Difference = amp_dp[Sedated == "Sedated"] - amp_dp[Sedated == "Awake"])

dp_diff_sum <- dp_diff %>%
  group_by(freq) %>%
  summarise(avg = mean(Difference, na.rm = TRUE),
            std = sd(Difference,   na.rm = TRUE),
            n   = n(),
            sem = std / sqrt(n))

dp_diff_sum <- left_join(dp_diff_sum, pval_dp %>% select(freq, significance), by = "freq")

## Fig. 2B — sedation difference w/in subject
Fig2b_DP <-
  ggplot(data = dp_diff) +
  geom_hline(yintercept = 0, linetype = '1212', color = "#808080") +
  geom_line(linewidth = .5, alpha = .1,
            aes(x = freq, y = Difference, group = Subject)) +
  geom_line(linewidth = .75, data = dp_diff_sum, aes(x = freq, y = avg)) +
  geom_errorbar(data = dp_diff_sum,
                aes(x = freq, ymax = avg + sem, ymin = avg - sem),
                width = .03, linewidth = .75) +
  geom_text(data = dp_diff_sum,
            aes(x = freq, y = avg + sem + 2, label = significance),
            size = 4) +
  xlab("Frequency (kHz)") +
  ylab("\u0394 Amplitude (dB)") +
  scale_x_continuous(trans = 'log10',
                     breaks = c(.5, 1, 2, 4, 8, 12),
                     labels = c(".5", "1", "2", "4", "8", "12")) +
  annotate("text", x = .7, y =  21, label = "Higher Sedated", size = 4, hjust = "left",
           color = "#808080", family = "sans") +
  annotate("text", x = .7, y = -13, label = "Higher Awake",   size = 4, hjust = "left",
           color = "#808080", family = "sans") +
  theme_bw() +
  theme(text = element_text(size = 11, family = "sans"),
        legend.position = "top", legend.title = element_blank())

ggarrange(Fig2a_DP + rremove("xlab"), Fig2b_DP,
          labels = c("A", "B"),
          ncol = 1, nrow = 2,
          align = 'v',
          font.label = list(family = "sans"))

## Save Figure
ggsave("./figs/fig2.tiff",
       plot = last_plot(),
       width = 84,
       height = 100,
       units = "mm",
       dpi = 600)

# Figure 3: SFOAE Awake vs Sedated --------------------------------------------------------
sf_sum <- oae %>%
  group_by(Sedated, freq) %>%
  summarise(avg = mean(amp_sf, na.rm = TRUE),
            std = sd(amp_sf,   na.rm = TRUE),
            n   = n(),
            sem = std / sqrt(n))

sf_sum <- left_join(sf_sum, pval_sf %>% select(freq, significance), by = "freq")

## Fig. 3A — raw amplitudes awake and sedated
Fig2a_SF <-
  ggplot(data = oae) +
  geom_line(linewidth = .5, alpha = .2,
            aes(x = freq, y = amp_sf, color = Sedated,
                group = interaction(Subject, Sedated), linetype = Sedated)) +
  geom_line(linewidth = .75, data = sf_sum,
            aes(x = freq, y = avg, color = Sedated, linetype = Sedated)) +
  geom_errorbar(data = sf_sum,
                aes(x = freq, ymax = avg + sem, ymin = avg - sem, color = Sedated),
                width = .03, linewidth = .75) +
  geom_text(data = sf_sum %>% filter(Sedated == "Awake"),
            aes(x = freq, y = avg + sem + 2, label = significance),
            size = 4) +
  xlab("Frequency (kHz)") +
  ylab("Amplitude (dB EPL)") +
  scale_x_continuous(trans = 'log10',
                     breaks = c(.5, 1, 2, 4, 8, 12),
                     labels = c(".5", "1", "2", "4", "8", "12")) +
  scale_color_manual(name = "Status",
                     values = c("Awake" = col_awk, "Sedated" = col_sed),
                     labels = c("Awake", "Sedated")) +
  scale_linetype_manual(name = "Status",
                        values = c("Awake" = "solid", "Sedated" = "3121"),
                        labels = c("Awake", "Sedated")) +
  theme_bw() +
  theme(text = element_text(size = 11, family = "sans"),
        legend.position = "top", legend.title = element_blank())

## Compute sedation difference per subject
sf_diff <- oae %>%
  group_by(Subject, freq) %>%
  summarize(Difference = amp_sf[Sedated == "Sedated"] - amp_sf[Sedated == "Awake"])

sf_diff_sum <- sf_diff %>%
  group_by(freq) %>%
  summarise(avg = mean(Difference, na.rm = TRUE),
            std = sd(Difference,   na.rm = TRUE),
            n   = n(),
            sem = std / sqrt(n))

sf_diff_sum <- left_join(sf_diff_sum, pval_sf %>% select(freq, significance), by = "freq")

## Fig. 3B — sedation difference w/in subject
Fig2b_SF <-
  ggplot(data = sf_diff) +
  geom_hline(yintercept = 0, linetype = '1212', color = "#808080") +
  geom_line(linewidth = .5, alpha = .1,
            aes(x = freq, y = Difference, group = Subject)) +
  geom_line(linewidth = .75, data = sf_diff_sum, aes(x = freq, y = avg)) +
  geom_errorbar(data = sf_diff_sum,
                aes(x = freq, ymax = avg + sem, ymin = avg - sem),
                width = .03, linewidth = .75) +
  geom_text(data = sf_diff_sum,
            aes(x = freq, y = avg + sem + 1, label = significance),
            size = 4) +
  xlab("Frequency (kHz)") +
  ylab("\u0394 Amplitude (dB)") +
  scale_x_continuous(trans = 'log10',
                     breaks = c(.5, 1, 2, 4, 8, 12),
                     labels = c(".5", "1", "2", "4", "8", "12")) +
  annotate("text", x = 12, y =  16, label = "Higher Sedated", size = 4, hjust = "right",
           color = "#808080", family = "sans") +
  annotate("text", x = .7, y = -21, label = "Higher Awake",   size = 4, hjust = "left",
           color = "#808080", family = "sans") +
  theme_bw() +
  theme(text = element_text(size = 11, family = "sans"),
        legend.position = "top", legend.title = element_blank())

ggarrange(Fig2a_SF + rremove("xlab"), Fig2b_SF,
          labels = c("A", "B"),
          ncol = 1, nrow = 2,
          align = 'v',
          font.label = list(family = "sans"))

ggsave("./figs/fig3.tiff",
       plot = last_plot(),
       width = 84,
       height = 100,
       units = "mm",
       dpi = 600)

# Figure 4: Qerb --------------------------------------------------------------
qerb_sum <- oae %>%
  group_by(Sedated, freq) %>%
  summarise(avg = mean(qerb, na.rm = TRUE),
            std = sd(qerb,   na.rm = TRUE),
            n   = n(),
            sem = std / sqrt(n))

qerb_sum <- left_join(qerb_sum, pval_qerb %>% select(freq, significance), by = "freq")

## Fig. 4A — raw Qerb awake and sedated
Fig3a_Qerb <-
  ggplot(data = oae) +
  geom_line(linewidth = .5, alpha = .2,
            aes(x = freq, y = qerb, color = Sedated,
                group = interaction(Subject, Sedated), linetype = Sedated)) +
  geom_line(linewidth = .75, data = qerb_sum,
            aes(x = freq, y = avg, color = Sedated, linetype = Sedated)) +
  geom_errorbar(data = qerb_sum,
                aes(x = freq, ymax = avg + sem, ymin = avg - sem, color = Sedated),
                width = .03, linewidth = .75) +
  geom_text(data = qerb_sum %>% filter(Sedated == "Sedated"),
            aes(x = freq, y = avg + sem + .5, label = significance),
            size = 4) +
  xlab("Frequency (kHz)") +
  ylab("Qerb Estimate") +
  scale_x_continuous(trans = 'log10',
                     breaks = c(.5, 1, 2, 4, 8, 12),
                     labels = c(".5", "1", "2", "4", "8", "12")) +
  scale_y_continuous(trans = 'log10',
                     breaks = c(.5, 1, 2, 3, 5, 10, 20)) +
  coord_cartesian(ylim = c(.5, 20)) +
  scale_color_manual(name = "Status",
                     values = c("Awake" = col_awk, "Sedated" = col_sed),
                     labels = c("Awake", "Sedated")) +
  scale_linetype_manual(name = "Status",
                        values = c("Awake" = "solid", "Sedated" = "3121"),
                        labels = c("Awake", "Sedated")) +
  theme_bw() +
  theme(text = element_text(size = 11, family = "sans"),
        legend.position = "top", legend.title = element_blank())

## Compute sedation difference per subject
qerb_diff <- oae %>%
  group_by(Subject, freq) %>%
  summarize(Difference = qerb[Sedated == "Sedated"] - qerb[Sedated == "Awake"])

qerb_diff_sum <- qerb_diff %>%
  group_by(freq) %>%
  summarise(avg = mean(Difference, na.rm = TRUE),
            std = sd(Difference,   na.rm = TRUE),
            n   = n(),
            sem = std / sqrt(n))

qerb_diff_sum <- left_join(qerb_diff_sum, pval_qerb %>% select(freq, significance), by = "freq")

## Fig. 4B — sedation difference
Fig3b_Qerb <-
  ggplot(data = qerb_diff) +
  geom_hline(yintercept = 0, linetype = '1212', color = "#808080") +
  geom_line(linewidth = .5, alpha = .1,
            aes(x = freq, y = Difference, group = Subject)) +
  geom_line(linewidth = .75, data = qerb_diff_sum, aes(x = freq, y = avg)) +
  geom_errorbar(data = qerb_diff_sum,
                aes(x = freq, ymax = avg + sem, ymin = avg - sem),
                width = .03, linewidth = .75) +
  geom_text(data = qerb_diff_sum,
            aes(x = freq, y = avg + sem + .5, label = significance),
            size = 4) +
  xlab("Frequency (kHz)") +
  ylab("\u0394 Qerb") +
  scale_x_continuous(trans = 'log10',
                     breaks = c(.5, 1, 2, 4, 8, 12),
                     labels = c(".5", "1", "2", "4", "8", "12")) +
  theme_bw() +
  theme(text = element_text(size = 11, family = "sans"),
        legend.position = "top", legend.title = element_blank())

ggarrange(Fig3a_Qerb + rremove("xlab"), Fig3b_Qerb,
          labels = c("A", "B"),
          ncol = 1, nrow = 2,
          align = 'v',
          font.label = list(family = "sans"))

ggsave("./figs/fig4.tiff",
       plot = last_plot(),
       width = 84,
       height = 100,
       units = "mm",
       dpi = 600)

# Figure 5: Effect of Sex -----------------------------------------------------------
mod_dpS <- lmer(amp_dp ~ freqFactor * Sedated +  Sedated:Sex + Sex +  (1|Subject), data = oae)
mod_sfS<- lmer(amp_sf ~ freqFactor * Sedated  +  Sedated:Sex +Sex + (1|Subject), data = oae)
mod_qerbS <- lmer(qerb ~ freqFactor * Sedated  +  Sedated:Sex + Sex + (1|Subject), data = oae)

Anova(mod_dpS, test.statistic = 'F')
Anova(mod_sfS, test.statistic = 'F')
Anova(mod_qerbS, test.statistic = 'F')
## DPOAE sex difference
dp_sex_diff <- oae %>%
  group_by(Subject, freq, Sex) %>%
  summarize(Difference = amp_dp[Sedated == "Sedated"] - amp_dp[Sedated == "Awake"])
dp_sex_diff$freq <- as.numeric(as.character(dp_sex_diff$freq))

dp_sex_diff_sum <- dp_sex_diff %>%
  group_by(freq, Sex) %>%
  summarise(avg = mean(Difference, na.rm = TRUE),
            std = sd(Difference,   na.rm = TRUE),
            n   = n(),
            sem = std / sqrt(n))

Fig5a <-
  ggplot(data = dp_sex_diff) +
  geom_hline(yintercept = 0, linetype = '1212', color = "#808080") +
  geom_line(linewidth = .5, alpha = .2,
            aes(x = freq, y = Difference, group = Subject, color = Sex, linetype = Sex)) +
  geom_line(linewidth = .75, data = dp_sex_diff_sum,
            aes(x = freq, y = avg, color = Sex, linetype = Sex)) +
  geom_errorbar(data = dp_sex_diff_sum,
                aes(x = freq, ymax = avg + sem, ymin = avg - sem, color = Sex),
                width = .03, linewidth = .75) +
  scale_linetype_manual(values = c("M" = "solid", "F" = '3121')) +
  scale_x_continuous(trans = 'log10',
                     breaks = c(.5, 1, 2, 4, 8, 12),
                     labels = c(".5", "1", "2", "4", "8", "12")) +
  xlab("") +
  ylab("\u0394 DPOAE Amp.") +
  annotate("text", x = .7, y =  24, label = "Higher Sedated", size = 3, hjust = "left",
           color = "#808080", family = "sans") +
  annotate("text", x = .7, y = -21, label = "Higher Awake",   size = 3, hjust = "left",
           color = "#808080", family = "sans") +
  theme_bw() +
  theme(text = element_text(size = 11, family = "sans"),
        legend.position = "top", legend.title = element_blank())

## SFOAE sex difference
sf_sex_diff <- oae %>%
  group_by(Subject, freq, Sex) %>%
  summarize(Difference = amp_sf[Sedated == "Sedated"] - amp_sf[Sedated == "Awake"])
sf_sex_diff$freq <- as.numeric(as.character(sf_sex_diff$freq))

sf_sex_diff_sum <- sf_sex_diff %>%
  group_by(freq, Sex) %>%
  summarise(avg = mean(Difference, na.rm = TRUE),
            std = sd(Difference,   na.rm = TRUE),
            n   = n(),
            sem = std / sqrt(n))

Fig5b <-
  ggplot(data = sf_sex_diff) +
  geom_hline(yintercept = 0, linetype = '1212', color = "#808080") +
  geom_line(linewidth = .5, alpha = .2,
            aes(x = freq, y = Difference, group = Subject, color = Sex, linetype = Sex)) +
  geom_line(linewidth = .75, data = sf_sex_diff_sum,
            aes(x = freq, y = avg, color = Sex, linetype = Sex)) +
  geom_errorbar(data = sf_sex_diff_sum,
                aes(x = freq, ymax = avg + sem, ymin = avg - sem, color = Sex),
                width = .03, linewidth = .75) +
  scale_linetype_manual(values = c("M" = "solid", "F" = '3121')) +
  scale_x_continuous(trans = 'log10',
                     breaks = c(.5, 1, 2, 4, 8, 12),
                     labels = c(".5", "1", "2", "4", "8", "12")) +
  xlab("") +
  ylab("\u0394 SFOAE Amp.") +
  annotate("text", x = .7, y =  24, label = "Higher Sedated", size = 3, hjust = "left",
           color = "#808080", family = "sans") +
  annotate("text", x = .7, y = -26, label = "Higher Awake",   size = 3, hjust = "left",
           color = "#808080", family = "sans") +
  theme_bw() +
  theme(text = element_text(size = 11, family = "sans"),
        legend.position = "top", legend.title = element_blank())

## Qerb sex difference
qerb_sex_diff <- oae %>%
  group_by(Subject, freq, Sex) %>%
  summarize(Difference = qerb[Sedated == "Sedated"] - qerb[Sedated == "Awake"])
qerb_sex_diff$freq <- as.numeric(as.character(qerb_sex_diff$freq))

qerb_sex_diff_sum <- qerb_sex_diff %>%
  group_by(freq, Sex) %>%
  summarise(avg = mean(Difference, na.rm = TRUE),
            std = sd(Difference,   na.rm = TRUE),
            n   = n(),
            sem = std / sqrt(n))

Fig5c <-
  ggplot(data = qerb_sex_diff) +
  geom_hline(yintercept = 0, linetype = '1212', color = "#808080") +
  geom_line(linewidth = .5, alpha = .2,
            aes(x = freq, y = Difference, group = Subject, color = Sex, linetype = Sex)) +
  geom_line(linewidth = .75, data = qerb_sex_diff_sum,
            aes(x = freq, y = avg, color = Sex, linetype = Sex)) +
  geom_errorbar(data = qerb_sex_diff_sum,
                aes(x = freq, ymax = avg + sem, ymin = avg - sem, color = Sex),
                width = .03, linewidth = .75) +
  scale_linetype_manual(values = c("M" = "solid", "F" = '3121')) +
  scale_x_continuous(trans = 'log10',
                     breaks = c(.5, 1, 2, 4, 8, 12),
                     labels = c(".5", "1", "2", "4", "8", "12")) +
  xlab("Frequency (kHz)") +
  ylab("\u0394 Qerb") +
  annotate("text", x = .7, y =  14,   label = "Higher Sedated", size = 3, hjust = "left",
           color = "#808080", family = "sans") +
  annotate("text", x = .7, y = -10.5, label = "Higher Awake",   size = 3, hjust = "left",
           color = "#808080", family = "sans") +
  theme_bw() +
  theme(text = element_text(size = 11, family = "sans"),
        legend.position = "top", legend.title = element_blank())

ggarrange(Fig5a, Fig5b, Fig5c,
          labels = c("A", "B", "C"),
          vjust = 0,
          ncol = 1, nrow = 3,
          common.legend = TRUE,
          legend = "top",
          font.label = list(family = "sans"))


ggsave("./figs/fig5.tiff",
       plot = last_plot(),
       width = 84,
       height = 150,
       units = "mm",
       dpi = 600)



# Figure 6: Time Under Sedation -------------------------------------------
Q443 <- read_csv("./Q443_TimePoints.csv")
Q443_Q <- read_csv("./Q443_Qerb_TimePoints.csv")

lw <- 1
col_sed_time <- c('#d7191c', '#fdae61', '#abdda4', '#2b83ba')
lt <- 'dashed'
sz <- 3

DP443 <-ggplot(data = Q443) + 
  geom_line(aes(x = freq, y = Awake_DP), linewidth = lw) + 
  geom_line(aes(x = freq, y = Sed_DP_11.36), linewidth = lw, color = col_sed_time[1]) + 
  geom_line(aes(x = freq, y = Sed_DP_12.15), linewidth = lw, color = col_sed_time[2]) +
  geom_line(aes(x = freq, y = Sed_DP_12.53), linewidth = lw, color = col_sed_time[3]) +
  geom_line(aes(x = freq, y = Sed_DP_13.13), linewidth = lw, color = col_sed_time[4]) +
  geom_line(aes(x = freq, y = Awake_NFd), linewidth = lw, linetype = lt) + 
  geom_line(aes(x = freq, y = Sed_NF_11.36), linewidth = lw, color = col_sed_time[1], linetype = lt) +
  geom_line(aes(x = freq, y = Sed_NF_12.15), linewidth = lw, color = col_sed_time[2], linetype = lt) +
  geom_line(aes(x = freq, y = Sed_NF_12.53), linewidth = lw, color = col_sed_time[3], linetype = lt) +
  geom_line(aes(x = freq, y = Sed_NF_13.13), linewidth = lw, color = col_sed_time[4], linetype = lt) +
  
  annotate("text", x = 5, y = 24, label = "Awake", color = "black", hjust = "center", family = "sans", size = sz) + 
  annotate("text", x = 5, y = 15, label = "25 min.", color = col_sed_time[1], hjust = "center", family = "sans", size = sz) + 
  annotate("text", x = 5, y = 6, label = "65 min.", color = col_sed_time[2], hjust = "center", family = "sans", size = sz) + 
  annotate("text", x = 5, y = -3, label = "105 min.", color = col_sed_time[3], hjust = "center", family = "sans", size = sz) + 
  annotate("text", x = 5, y = -12, label = "125 min.", color = col_sed_time[4], hjust = "center", family = "sans", size = sz) + 
  
  xlab("Frequency (kHz)") + 
  ylab("DP Amp. dB EPL") +
  ylim(-40, 45) +
  scale_x_continuous(trans = 'log10', 
                     breaks = c( .5, 1, 2, 4, 8, 16),  
                     labels = c(".5", "1", "2", "4", "8", "16")) +  
  theme_bw() + 
  theme(text = element_text(size = 11, family = "sans"), 
        legend.position = "top", legend.title = element_blank()) 


SF443 <- ggplot(data = Q443) + 
  geom_line(aes(x = freq, y = Awake_SF), linewidth = lw) + 
  geom_line(aes(x = freq, y = Sed_SF_11.43), linewidth = lw, color = col_sed_time[1]) + 
  geom_line(aes(x = freq, y = Sed_SF_12.23), linewidth = lw, color = col_sed_time[2]) +
  geom_line(aes(x = freq, y = Sed_SF_13.02), linewidth = lw, color = col_sed_time[3]) +
  geom_line(aes(x = freq, y = Sed_SF_13.39), linewidth = lw, color = col_sed_time[4]) +
  geom_line(aes(x = freq, y = Awake_NFd), linewidth = lw, linetype = lt) + 
  geom_line(aes(x = freq, y = Sed_NF_11.43), linewidth = lw, color = col_sed_time[1], linetype = lt) +
  geom_line(aes(x = freq, y = Sed_NF_12.23), linewidth = lw, color = col_sed_time[2], linetype = lt) +
  geom_line(aes(x = freq, y = Sed_NF_13.02), linewidth = lw, color = col_sed_time[3], linetype = lt) +
  geom_line(aes(x = freq, y = Sed_NF_13.39), linewidth = lw, color = col_sed_time[4], linetype = lt) +
  
  annotate("text", x = 7.75, y = 44, label = "Awake", color = "black", hjust = "center", family = "sans", size = sz) + 
  annotate("text", x = 7.75, y = 35, label = "30 min.", color = col_sed_time[1], hjust = "center", family = "sans", size = sz) + 
  annotate("text", x = 7.75, y = 26, label = "70 min.", color = col_sed_time[2], hjust = "center", family = "sans", size = sz) + 
  annotate("text", x = 7.75, y = 17, label = "110 min.", color = col_sed_time[3], hjust = "center", family = "sans", size = sz) + 
  annotate("text", x = 7.75, y = 8, label = "150 min.", color = col_sed_time[4], hjust = "center", family = "sans", size = sz) + 
  
  xlab("Frequency (kHz)") + 
  ylab("SF Amp. dB EPL") +
  ylim(-40, 45) +
  scale_x_continuous(trans = 'log10', 
                     breaks = c( .5, 1, 2, 4, 8, 16),  
                     labels = c(".5", "1", "2", "4", "8", "16")) +  
  theme_bw() + 
  theme(text = element_text(size = 11, family = "sans"), 
        legend.position = "top", legend.title = element_blank()) 

ggarrange(DP443, SF443, 
                     ncol = 1, nrow = 2, 
                     labels = c("A", "B"),
                     font.label = list(family = "sans"))

ggsave("./figs/fig6.tiff",
       plot = last_plot(),
       width = 84,
       height = 100,
       units = "mm",
       dpi = 600)


phase443 <- ggplot(data = Q443) + 
  geom_line(aes(x = freq, y = Awake_phase), linewidth = lw) + 
  geom_line(aes(x = freq, y = Sed_phase_11.43), linewidth = lw, color = col_sed_time[1]) + 
  geom_line(aes(x = freq, y = Sed_phase_12.23), linewidth = lw, color = col_sed_time[2]) +
  geom_line(aes(x = freq, y = Sed_phase_13.02), linewidth = lw, color = col_sed_time[3]) +
  geom_line(aes(x = freq, y = Sed_phase_13.39), linewidth = lw, color = col_sed_time[4]) +
  xlab("Frequency (kHz)") + 
  ylab("Phase (cycles)") +
  coord_cartesian( ylim = c(-16, 0) )+
  scale_x_continuous(trans = 'log10', 
                     breaks = c( .5, 1, 2, 4, 8, 16),  
                     labels = c(".5", "1", "2", "4", "8", "16")) +  
  theme_bw() + 
  theme(text = element_text(size = 11, family = "sans"), 
        legend.position = "top", legend.title = element_blank()) 

Qfig443 <- ggplot(data = Q443_Q) + 
  geom_line(aes(x = freq, y = Awake_Q), linewidth = lw) + 
  geom_line(aes(x = freq, y = Sed_Q_11.43), linewidth = lw, color = col_sed_time[1]) + 
  geom_line(aes(x = freq, y = Sed_Q_12.23), linewidth = lw, color = col_sed_time[2]) +
  geom_line(aes(x = freq, y = Sed_Q_13.02), linewidth = lw, color = col_sed_time[3]) +
  geom_line(aes(x = freq, y = Sed_Q_13.39), linewidth = lw, color = col_sed_time[4]) +
  #geom_point(aes(x = freq, y = Awake_Q),  shape = 4, size = 2) + 
  #geom_point(aes(x = freq, y = Sed_Q_11.43), color = col_sed[1], shape = 16, size = 2) + 
  #geom_point(aes(x = freq, y = Sed_Q_12.23), color = col_sed[2], shape = 17, size = 2) +
  #geom_point(aes(x = freq, y = Sed_Q_13.02),  color = col_sed[3], shape = 18, size = 2) +
  #geom_point(aes(x = freq, y = Sed_Q_13.39), color = col_sed[4], shape = 15, size = 2) +
  xlab("Frequency (kHz)") + 
  ylab("Qerb") +
  ylim(0, 20) +
  coord_cartesian(xlim = c(.5,16)) +
  scale_x_continuous(trans = 'log10', 
                     breaks = c( .5, 1, 2, 4, 8, 16),  
                     labels = c(".5", "1", "2", "4", "8", "16")) +  
  annotate("text", x = .75, y = 18, label = "Awake", color = "black", hjust = "center", family = "sans", size = sz) + 
  annotate("text", x = .75, y = 16, label = "30 min.", color = col_sed_time[1], hjust = "center", family = "sans", size = sz) + 
  annotate("text", x = .75, y = 14, label = "70 min.", color = col_sed_time[2], hjust = "center", family = "sans", size = sz) + 
  annotate("text", x = .75, y = 12, label = "110 min.", color = col_sed_time[3], hjust = "center", family = "sans", size = sz) + 
  annotate("text", x = .75, y = 10, label = "150 min.", color = col_sed_time[4], hjust = "center", family = "sans", size = sz) + 
  theme_bw() + 
  theme(text = element_text(size = 11, family = "sans"), 
        legend.position = "top", legend.title = element_blank()) 


ggarrange(phase443, Qfig443, 
          labels = c("A", "B"),
          ncol = 1, 
          nrow = 2, 
          align = "hv", 
          font.label = list(family = "sans"))

ggsave("./figs/ESM3.tiff",
       plot = last_plot(),
       width = 84,
       height = 100,
       units = "mm",
       dpi = 600)






