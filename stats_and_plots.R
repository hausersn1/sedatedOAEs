
# Set working directory ---------------------------------------------------
setwd("D:/THESIS/Document/ch-3-diag-precision")

# Load Packages -----------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(lme4)
library(car)
library(emmeans)
library(ggpubr)
library(Cairo)


# Load Data ---------------------------------------------------------------
# Get general chinchilla informaation
info <- read_csv("D:/THESIS/Pitch_Diagnostics_Data/compiledData/Chin/Chin_Info.csv")
chin_info <- info %>% select(Subject, Sex)  %>% unique()

# Load and organize SFOAE 
sfoae <- read_csv("D:/THESIS/Pitch_Diagnostics_Data/compiledData/Chin/Data_SFOAE.csv")
sfoae$Group <- factor(sfoae$Group)
sfoae$Subject <- factor(sfoae$Subject)
sfoae$Status <- factor(sfoae$PrePost, levels = c(0, 1, 2, 3), labels = c("Pre", "1day", "Post", "Last"))
sfoae$Sedated <- factor(sfoae$Sedated, levels = c(0,1), labels = c("Awake", "Sedated"))
sfoae <- sfoae %>% filter(Status == "Pre")

Q443 <- sfoae %>% filter(Subject == "Q443")
sfoae <- sfoae %>% filter(Group != 'Sed')

subjects_in_both_conditions <- sfoae %>%
  group_by(Subject) %>%
  summarize(n_conditions = n_distinct(Sedated)) %>%
  filter(n_conditions == 2) %>%
  pull(Subject)

sfoae <- sfoae %>%
  filter(Subject %in% subjects_in_both_conditions)

sfoae <- sfoae %>%
  pivot_longer(cols = 5:22, names_to = c("meas", "freq"), names_sep = "_", values_to = "amp") %>%
  group_by(Subject, Group, Status, Sedated, freq, meas) %>%
  pivot_wider(names_from = "meas", values_from = "amp")
sfoae <- rename(sfoae, amp = SF)
sfoae <- rename(sfoae, qerb = Q)

sfoae <- sfoae %>%
  mutate(freq = dplyr::recode(freq,"707" = .7,
                              "1000" = 1,
                              "1414" = 1.4,
                              "2000" = 2,
                              "2828" = 2.8,
                              "4000" = 4,
                              "5656" = 5.6,
                              "8000" = 8, 
                              "11314" = 11.3))


# Load and organize DPOAE data
dpoae <- read_csv("./awake-sed-dpoae.csv")

dpoae$Sedated <- factor(dpoae$Group, levels = c(0,1), labels = c("Awake", "Sedated"))
dpoae$Subject <- factor(dpoae$Subject)

#dpoae <- dpoae %>% filter(!Subject %in% c('Q423', 'Q425', 'Q427'))

# Set up for by-frequency analysis
dpoae <- dpoae %>% pivot_longer(cols = 3:11, names_to = "freq", values_to = "amp" ) 
dpoae <- dpoae %>%
  mutate(freq = dplyr::recode(freq,"dp707" = .7,
                              "dp1" = 1,
                              "dp1.4" = 1.4,
                              "dp2" = 2,
                              "dp2.8" = 2.8,
                              "dp4" = 4,
                              "dp5.6" = 5.6,
                              "dp8" = 8, 
                              "dp11.3" = 11.3))


# dpoae$oaetype <- as.factor(dpoae$oaetype)
# dpoae$Sex <- as.factor(dpoae$Sex)
# dpoae$Subject <- as.factor(dpoae$Subject)
sfoae <- sfoae %>%
  filter(Subject %in% dpoae$Subject)
dpoae <- dpoae %>%
  filter(Subject %in% sfoae$Subject)

# Stats -------------------------------------------------------------------
chin_info$Subject <- as.factor(chin_info$Subject)
chin_info$Sex <- as.factor(chin_info$Sex)
sfoae2 <- left_join(sfoae, chin_info, by="Subject")
dpoae2 <- left_join(dpoae, chin_info, by="Subject")

sfoae2$freq <- as.ordered(as.factor(sfoae2$freq))
dpoae2$freq <- as.ordered(as.factor(dpoae2$freq))

mod_dp <- lmer(amp ~ freq * Sedated + (1|Subject), data = dpoae2)
mod_sf <- lmer(amp ~ freq * Sedated + (1|Subject), data = sfoae2)
mod_qerb <- lmer(qerb ~ freq * Sedated + (1|Subject), data = sfoae2)

Anova(mod_dp, test.statistic = 'F')
Anova(mod_sf, test.statistic = 'F')
Anova(mod_qerb, test.statistic = 'F')

em_dp <- emmeans(mod_dp, ~  Sedated | freq)
stats_dp <- as.data.frame(pairs(em_dp))

em_sf <- emmeans(mod_sf, ~  Sedated | freq)
stats_sf <- as.data.frame(pairs(em_sf))

em_qerb <- emmeans(mod_qerb, ~  Sedated | freq)
stats_q <- as.data.frame(pairs(em_qerb))

# Plot Figures ------------------------------------------------------------


# Figure 1: Single Chin -------------------------------------------------------------

oneChin <- read_csv("./singleChin.csv")
lw <- 1
col_sed <- '#43a2ca'
lt <- 'dashed'

oneChinDP <- 
  ggplot(data = oneChin) + 
  geom_line(aes(x = freq, y = A_dpOAE), linewidth = lw) + 
  geom_line(aes(x = freq, y = S_dpOAE), linewidth = lw, color = col_sed) + 
  geom_line(aes(x = freq, y = A_dpNF), linewidth = lw, linetype = lt) + 
  geom_line(aes(x = freq, y = S_dpNF), linewidth = lw, color = col_sed, linetype = lt) +
  xlab("Frequency (kHz)") + 
  ylab("Amplitude (dB EPL)") +
  ylim(-40, 50) +
  ggtitle("DPOAE") +
  scale_x_continuous(trans = 'log10', 
                     breaks = c( .5, 1, 2, 4, 8, 12),  
                     labels = c(".5", "1", "2", "4", "8", "12")) +  
  theme_bw() + 
  theme(text = element_text(size = 11, family = "serif"), 
        legend.position = "top", legend.title = element_blank()) 

oneChinDP

oneChinSF <- 
  ggplot(data = oneChin) + 
  geom_line(aes(x = freq, y = A_sfOAE), linewidth = lw) + 
  geom_line(aes(x = freq, y = S_sfOAE), linewidth = lw, color = col_sed) + 
  geom_line(aes(x = freq, y = A_sfNF), linewidth = lw, linetype = lt) + 
  geom_line(aes(x = freq, y = S_sfNF), linewidth = lw, color = col_sed, linetype = lt) +
  xlab("Frequency (kHz)") + 
  ylab("Amplitude (dB EPL)") +
  ylim(-40, 50) +
  ggtitle("SFOAE") +
  scale_x_continuous(trans = 'log10', 
                     breaks = c( .5, 1, 2, 4, 8, 12),  
                     labels = c(".5", "1", "2", "4", "8", "12")) +  
  theme_bw() + 
  theme(text = element_text(size = 11, family = "serif"), 
        legend.position = "top", legend.title = element_blank()) 


oneChinSF

ggarrange(oneChinDP , oneChinSF+ rremove("ylab"), 
          labels = c("A", "B"),
          ncol = 2, nrow = 1, 
          align = 'v', 
          common.legend = TRUE, 
          font.label = list(family = 'serif'))


ggsave("./figs/fig-4-1_SingleChin_AwkVsSed.pdf",
       plot = last_plot(),
       width = 5,
       height = 2.5,
       units = "in",
       dpi = 600)

# Figure 2: DPOAE Awake vs Sedated -------------------------------------------------

# Get averaged response
dpoae_sum <- dpoae %>% group_by(Sedated, freq) %>% 
  summarise(avg = mean(amp, na.rm=T), 
            std = sd(amp, na.rm=T), 
            n = n(), 
            sem = std/sqrt(n))

# Calculate p-values for each frequency (example using t-test)
p_values <- stats_dp %>% 
  mutate(
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ "" # not significant
    )
  )


Fig1a_DP <- 
  ggplot(
    data = dpoae) +
  geom_line(size = .5, alpha = .1, 
            aes(x = freq, y=amp, color = Sedated,
                group = interaction(Subject, Sedated) )) +
  xlab("Frequency (kHz)") + 
  ylab("Amplitude (dB EPL)") +
  geom_line(size = 1, data = dpoae_sum, aes(x = freq, y = avg, color = Sedated)) +
  geom_errorbar(data = dpoae_sum, 
                aes(x = freq, ymax=avg+sem, ymin=avg-sem, color = Sedated), 
                width = .03, size = 1 )+
  scale_x_continuous(trans = 'log10', 
                     breaks = c( .5, 1, 2, 4, 8, 12),  
                     labels = c(".5", "1", "2", "4", "8", "12")) +  
  scale_color_manual(values = c("Awake" = "black", "Sedated" = col_sed),
                     labels = c("Awake", "Sedated"), 
                     "Status") + 
  theme_bw() + 
  theme(text = element_text(size = 11, family = "serif"), 
        legend.position = "top", legend.title = element_blank()) +
  geom_text(data = (dpoae_sum %>% filter(Sedated == "Sedated")), 
            aes(x = freq, y = avg + sem + 2,
                label = p_values$significance), 
            size = 8) 

Fig1a_DP
print(stats_dp)


# Now look at difference data for Fig 1B
dpoae_diff <- dpoae %>% group_by(Subject, freq) %>% 
  summarize(Difference = amp[Sedated == "Sedated"] - amp[Sedated == "Awake"])

# Get averaged response
dpoae_diff_sum <- dpoae_diff %>% group_by(freq) %>% 
  summarise(avg = mean(Difference, na.rm=T), 
            std = sd(Difference, na.rm=T), 
            n = n(), 
            sem = std/sqrt(n))

# Calculate p-values for each frequency (example using t-test)
p_values <- dpoae_diff %>%
  group_by(freq) %>%
  summarize(p_value = t.test(x = Difference)$p.value) %>%
  mutate(
    significance = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      TRUE ~ "" # not significant
    )
  )

# Add p-values to the summary data table. 
dpoae_diff_sum <- 
  left_join(dpoae_diff_sum, p_values, by = join_by(freq))

Fig1b_DP <- 
  ggplot(
    data = dpoae_diff) +
  geom_hline(yintercept = 0, linetype = 'dashed')+
  geom_line(size = .5, alpha = .1, 
            aes(x = freq, y=Difference,
                group = Subject )) +
  xlab("Frequency (kHz)") + 
  ylab("\u0394 Amplitude (dB EPL)") +
  geom_line(size = 1, data = dpoae_diff_sum, aes(x = freq, y = avg)) +
  geom_errorbar(data = dpoae_diff_sum, 
                aes(x = freq, ymax=avg+sem, ymin=avg-sem), 
                width = .03, size = 1 )+
  scale_x_continuous(trans = 'log10', 
                     breaks = c( .5, 1, 2, 4, 8, 12),  
                     labels = c(".5", "1", "2", "4", "8", "12")) +  
  annotate("text", x = .7, y = 21, label = "Higher Sedated", size = 4, hjust = "left", 
           color = "#808080", family = "serif") + 
  annotate("text", x = .7, y = -13, label = "Higher Awake", size = 4, hjust = "left", 
           color = "#808080", family = "serif") + 
  geom_text(data = dpoae_diff_sum , 
            aes(x = freq, y = avg + sem + 2,
                label = significance), 
            size = 8) + 
  theme_bw() + 
  theme(text = element_text(size = 11, family = "serif"), 
        legend.position = "top", legend.title = element_blank()) 


Fig1b_DP


ggarrange(Fig1a_DP + rremove("xlab"), Fig1b_DP, 
          labels = c("A", "B"),
          ncol = 1, nrow = 2, 
          align = 'v', 
          font.label = list(family = "serif"))


ggsave("./figs/fig-4-2_DPOAE_AwkVsSed.pdf",
       plot = last_plot(),
       width = 5,
       height = 4.5,
       units = "in",
       dpi = 600, 
       device = cairo_pdf)

# Figure 3: SFOAE Awake vs Sedated --------------------------------------------------------

# Get averaged response
sfoae_sum <- sfoae %>% group_by(Sedated, freq) %>% 
  summarise(avg = mean(amp, na.rm=T), 
            std = sd(amp, na.rm=T), 
            n = n(), 
            sem = std/sqrt(n))

# Calculate p-values for each frequency (example using t-test)
p_values <- stats_sf %>% 
  mutate(
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ "" # not significant
    )
  )


Fig2a_SF <- 
  ggplot(
    data = sfoae) +
  geom_line(size = .5, alpha = .1, 
            aes(x = freq, y=amp, color = Sedated,
                group = interaction(Subject, Sedated) )) +
  xlab("Frequency (kHz)") + 
  ylab("Amplitude (dB EPL)") +
  geom_line(size = 1, data = sfoae_sum, aes(x = freq, y = avg, color = Sedated)) +
  geom_errorbar(data = sfoae_sum, 
                aes(x = freq, ymax=avg+sem, ymin=avg-sem, color = Sedated), 
                width = .03, size = 1 )+
  scale_x_continuous(trans = 'log10', 
                     breaks = c( .5, 1, 2, 4, 8, 12),  
                     labels = c(".5", "1", "2", "4", "8", "12")) +  
  scale_color_manual(values = c("Awake" = "black", "Sedated" = col_sed),
                     labels = c("Awake", "Sedated"), 
                     "Status") + 
  geom_text(data = (sfoae_sum %>% filter(Sedated == "Awake")), 
            aes(x = freq, y = avg + sem + 2,
                label = p_values$significance), 
            size = 8) + 
  theme_bw() + 
  theme(text = element_text(size = 11, family = "serif"), 
        legend.position = "top", legend.title = element_blank()) # Adjust size as needed

Fig2a_SF

# Now look at difference data for Fig 1B
sfoae_diff <- sfoae %>% group_by(Subject, freq) %>% 
  summarize(Difference = amp[Sedated == "Sedated"] - amp[Sedated == "Awake"])

# Get averaged response
sfoae_diff_sum <- sfoae_diff %>% group_by(freq) %>% 
  summarise(avg = mean(Difference, na.rm=T), 
            std = sd(Difference, na.rm=T), 
            n = n(), 
            sem = std/sqrt(n))

# Calculate p-values for each frequency (example using t-test)
p_values2 <- sfoae_diff %>%
  group_by(freq) %>%
  summarize(p_value = t.test(x = Difference)$p.value) %>%
  mutate(
    significance = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      TRUE ~ "" # not significant
    )
  )
# Add p-values to the summary data table. 
sfoae_diff_sum <- 
  left_join(sfoae_diff_sum, p_values2, by = join_by(freq))

Fig2b_SF <- 
  ggplot(
    data = sfoae_diff) +
  geom_hline(yintercept = 0, linetype = 'dashed')+
  geom_line(size = .5, alpha = .1, 
            aes(x = freq, y=Difference,
                group = Subject )) +
  xlab("Frequency (kHz)") + 
  ylab("\u0394 Amplitude (dB EPL)") +
  geom_line(size = 1, data = sfoae_diff_sum, aes(x = freq, y = avg)) +
  geom_errorbar(data = sfoae_diff_sum, 
                aes(x = freq, ymax=avg+sem, ymin=avg-sem), 
                width = .03, size = 1 )+
  scale_x_continuous(trans = 'log10', 
                     breaks = c( .5, 1, 2, 4, 8, 12),  
                     labels = c(".5", "1", "2", "4", "8", "12")) +  
  annotate("text", x = 12, y = 16, label = "Higher Sedated", size = 4, hjust = "right", 
           color = "#808080", family = "serif") + 
  annotate("text", x = .7, y = -21, label = "Higher Awake", size = 4, hjust = "left", 
           color = "#808080", family = "serif") + 
  geom_text(data = sfoae_diff_sum , 
            aes(x = freq, y = avg + sem + 1,
                label = significance), 
            size = 8) + 
  theme_bw() + 
  theme(text = element_text(size = 11, family = "serif"), 
        legend.position = "top", legend.title = element_blank()) 

Fig2b_SF

ggarrange(Fig2a_SF + rremove("xlab"), Fig2b_SF, 
          labels = c("A", "B"),
          ncol = 1, nrow = 2, 
          align = 'v', 
          font.label = list(family = "serif"))


ggsave("./figs/fig-4-3_SFOAE_AwkVsSed.pdf",
       plot = last_plot(),
       width = 5,
       height = 4.5,
       units = "in",
       dpi = 600, 
       device = cairo_pdf)


# Figure 4: Qerb --------------------------------------------------------------


# Get averaged response
sfoae_sum <- sfoae %>% group_by(Sedated, freq) %>% 
  summarise(avg = mean(qerb, na.rm=T), 
            std = sd(qerb, na.rm=T), 
            n = n(), 
            sem = std/sqrt(n))

# Calculate p-values for each frequency (example using t-test)
p_values <- stats_q %>% 
  mutate(
    significance = case_when(
      p.value/2 < 0.001 ~ "***",
      p.value/2 < 0.01 ~ "**",
      p.value/2 < 0.05 ~ "*",
      TRUE ~ "" # not significant
    )
  )


Fig3a_Qerb <- 
  ggplot(
    data = sfoae) +
  geom_line(size = .5, alpha = .1, 
            aes(x = freq, y=qerb, color = Sedated,
                group = interaction(Subject, Sedated) )) +
  xlab("Frequency (kHz)") + 
  ylab("Qerb Estimate") +
  geom_line(size = 1, data = sfoae_sum, aes(x = freq, y = avg, color = Sedated)) +
  geom_errorbar(data = sfoae_sum, 
                aes(x = freq, ymax=avg+sem, ymin=avg-sem, color = Sedated), 
                width = .03, size = 1 )+
  scale_x_continuous(trans = 'log10', 
                     breaks = c( .5, 1, 2, 4, 8, 12),  
                     labels = c(".5", "1", "2", "4", "8", "12")) +  
  scale_y_continuous(trans = 'log10', 
                     breaks = c(.5, 1, 2, 3, 5, 10, 20)) +
  coord_cartesian(ylim = c(.5,20))+
  scale_color_manual(values = c("Awake" = "black", "Sedated" = col_sed),
                     labels = c("Awake", "Sedated"), 
                     "Status") + 
  geom_text(data = (sfoae_sum %>% filter(Sedated == "Sedated")), 
            aes(x = freq, y = avg + sem + .5,
                label = p_values$significance), 
            size = 8) + # Adjust size as needed
  theme_bw() + 
  theme(text = element_text(size = 11, family = "serif"), 
        legend.position = "top", legend.title = element_blank()) 

Fig3a_Qerb

# Now look at difference data for Fig 1B
sfoae_diff <- sfoae %>% group_by(Subject, freq) %>% 
  summarize(Difference = qerb[Sedated == "Sedated"] - qerb[Sedated == "Awake"])

# Get averaged response
sfoae_diff_sum2 <- sfoae_diff %>% group_by( freq) %>% 
  summarise(avg = mean(Difference, na.rm=T), 
            std = sd(Difference, na.rm=T), 
            n = n(), 
            sem = std/sqrt(n))

# Plot Fig 3B
Fig3b_Qerb <- 
  ggplot(
    data = sfoae_diff) +
  geom_hline(yintercept = 0, linetype = 'dashed')+
  geom_line(size = .5, alpha = .1, 
            aes(x = freq, y=Difference,
                group = Subject )) +
  xlab("Frequency (kHz)") + 
  ylab("\u0394 Qerb") +
  geom_line(size = 1, data = sfoae_diff_sum2, aes(x = freq, y = avg)) +
  geom_errorbar(data = sfoae_diff_sum2, 
                aes(x = freq, ymax=avg+sem, ymin=avg-sem), 
                width = .03, size = 1 )+
  geom_text(data = (sfoae_diff_sum2), 
            aes(x = freq, y = avg + sem + .5,
                label = p_values$significance), 
            size = 8) + # Adjust size as needed
  scale_x_continuous(trans = 'log10', 
                     breaks = c( .5, 1, 2, 4, 8, 12),  
                     labels = c(".5", "1", "2", "4", "8", "12")) +  
  theme_bw() + 
  theme(text = element_text(size = 11, family = "serif"), 
        legend.position = "top", legend.title = element_blank()) 

Fig3b_Qerb

# Combine into one figure
ggarrange(Fig3a_Qerb + rremove("xlab"), Fig3b_Qerb, 
          labels = c("A", "B"),
          ncol = 1, nrow = 2, 
          align = 'v', 
          font.label = list(family = "serif"))

ggsave("./figs/fig-4-4_Qerb_AwkVsSed.pdf",
       plot = last_plot(),
       width = 5,
       height = 4.5,
       units = "in",
       dpi = 600, 
       device = cairo_pdf)



# Figure 5: Effect of Sex -----------------------------------------------------------

# Now look at difference data for Fig 1B
sfoae_diff <- dpoae2 %>% group_by(Subject, freq, Sex) %>% 
  summarize(Difference = amp[Sedated == "Sedated"] - amp[Sedated == "Awake"])
sfoae_diff$freq <- as.numeric(as.character(sfoae_diff$freq))

# Get averaged response
sfoae_diff_sum <- sfoae_diff %>% group_by(freq, Sex) %>% 
  summarise(avg = mean(Difference, na.rm=T), 
            std = sd(Difference, na.rm=T), 
            n = n(), 
            sem = std/sqrt(n))

Fig5a <- 
  ggplot(
    data = sfoae_diff) +
  geom_hline(yintercept = 0, linetype = 'dashed')+
  geom_line(size = .5, alpha = .2, 
            aes(x = freq, y=Difference,
                group = Subject, color = Sex)) +
  xlab("") + 
  ylab("\u0394 DPOAE Amp.") +
  geom_line(size = 1, data = sfoae_diff_sum, aes(x = freq, y = avg, color = Sex)) +
  geom_errorbar(data = sfoae_diff_sum, 
                aes(x = freq, ymax=avg+sem, ymin=avg-sem, color = Sex), 
                width = .03, size = 1 )+
  scale_x_continuous(trans = 'log10', 
                     breaks = c( .5, 1, 2, 4, 8, 12),  
                     labels = c(".5", "1", "2", "4", "8", "12")) +  
  annotate("text", x = .7, y = 24, label = "Higher Sedated", size = 3, hjust = "left", 
           color = "#808080", family = "serif") + 
  annotate("text", x = .7, y = -21, label = "Higher Awake", size =3, hjust = "left", 
           color = "#808080", family = "serif") + 
  theme_bw() + 
  theme(text = element_text(size = 11, family = "serif"), 
        legend.position = "top", legend.title = element_blank()) 
Fig5a

# Now look at difference data for Fig 1B
sfoae_diff <- sfoae2 %>% group_by(Subject, freq, Sex) %>% 
  summarize(Difference = amp[Sedated == "Sedated"] - amp[Sedated == "Awake"])
sfoae_diff$freq <- as.numeric(as.character(sfoae_diff$freq))

# Get averaged response
sfoae_diff_sum <- sfoae_diff %>% group_by(freq, Sex) %>% 
  summarise(avg = mean(Difference, na.rm=T), 
            std = sd(Difference, na.rm=T), 
            n = n(), 
            sem = std/sqrt(n))

Fig5b <- 
  ggplot(
    data = sfoae_diff) +
  geom_hline(yintercept = 0, linetype = 'dashed')+
  geom_line(size = .5, alpha = .2, 
            aes(x = freq, y=Difference,
                group = Subject, color = Sex)) +
  xlab("") + 
  ylab("\u0394 SFOAE Amp.") +
  geom_line(size = 1, data = sfoae_diff_sum, aes(x = freq, y = avg, color = Sex)) +
  geom_errorbar(data = sfoae_diff_sum, 
                aes(x = freq, ymax=avg+sem, ymin=avg-sem, color = Sex), 
                width = .03, size = 1 )+
  scale_x_continuous(trans = 'log10', 
                     breaks = c( .5, 1, 2, 4, 8, 12),  
                     labels = c(".5", "1", "2", "4", "8", "12")) +  
  annotate("text", x = .7, y = 24, label = "Higher Sedated", size = 3, hjust = "left", 
           color = "#808080", family = "serif") + 
  annotate("text", x = .7, y = -26, label = "Higher Awake", size = 3, hjust = "left", 
           color = "#808080", family = "serif") + 
  theme_bw() + 
  theme(text = element_text(size = 11, family = "serif"), 
        legend.position = "top", legend.title = element_blank()) 
Fig5b


# Now look at difference data for Fig 1B
sfoae_diff <- sfoae2 %>% group_by(Subject, freq, Sex) %>% 
  summarize(Difference = qerb[Sedated == "Sedated"] - qerb[Sedated == "Awake"])
sfoae_diff$freq <- as.numeric(as.character(sfoae_diff$freq))

# Get averaged response
sfoae_diff_sum <- sfoae_diff %>% group_by(freq, Sex) %>% 
  summarise(avg = mean(Difference, na.rm=T), 
            std = sd(Difference, na.rm=T), 
            n = n(), 
            sem = std/sqrt(n))

Fig5c <- 
  ggplot(
    data = sfoae_diff) +
  geom_hline(yintercept = 0, linetype = 'dashed')+
  geom_line(size = .5, alpha = .2, 
            aes(x = freq, y=Difference,
                group = Subject, color = Sex)) +
  xlab("Frequency (kHz)") + 
  ylab("\u0394 Qerb") +
  geom_line(size = 1, data = sfoae_diff_sum, aes(x = freq, y = avg, color = Sex)) +
  geom_errorbar(data = sfoae_diff_sum, 
                aes(x = freq, ymax=avg+sem, ymin=avg-sem, color = Sex), 
                width = .03, size = 1 )+
  scale_x_continuous(trans = 'log10', 
                     breaks = c( .5, 1, 2, 4, 8, 12),  
                     labels = c(".5", "1", "2", "4", "8", "12")) +  
  annotate("text", x = .7, y = 14, label = "Higher Sedated", size = 3, hjust = "left", 
           color = "#808080", family = "serif") + 
  annotate("text", x = .7, y = -10.5, label = "Higher Awake", size = 3, hjust = "left", 
           color = "#808080", family = "serif") + 
  theme_bw() + 
  theme(text = element_text(size = 11, family = "serif"), 
        legend.position = "top", legend.title = element_blank()) 
Fig5c


ggarrange(Fig5a, Fig5b, Fig5c, 
          labels = c("A", "B", "C"),
          vjust = 0,
          ncol = 1, 
          nrow = 3, 
          common.legend = TRUE, 
          legend = "top", 
          font.label = list(family = "serif"))


ggsave("./figs/fig-4-5_DifferenceBySex.pdf",
       plot = last_plot(),
       width = 5,
       height = 5.5,
       units = "in",
       dpi = 600, 
       device = cairo_pdf)

mod_dpS <- lmer(amp ~ freq * Sedated +  Sedated:Sex + Sex +  (1|Subject), data = dpoae2)
mod_sfS<- lmer(amp ~ freq * Sedated  +  Sedated:Sex +Sex + (1|Subject), data = sfoae2)
mod_qerbS <- lmer(qerb ~ freq * Sedated  +  Sedated:Sex + Sex + (1|Subject), data = sfoae2)

Anova(mod_dpS, test.statistic = 'F')
Anova(mod_sfS, test.statistic = 'F')
Anova(mod_qerbS, test.statistic = 'F')



# Figure 6: Time Under Sedation -------------------------------------------
Q443 <- read_csv("./Q443_TimePoints.csv")

Q443_Q <- read_csv("./Q443_Qerb_TimePoints.csv")

lw <- 1
col_sed <- c('#d7191c', '#fdae61', '#abdda4', '#2b83ba')
lt <- 'dashed'

DP443 <-ggplot(data = Q443) + 
  geom_line(aes(x = freq, y = Awake_DP), linewidth = lw) + 
  geom_line(aes(x = freq, y = Sed_DP_11.36), linewidth = lw, color = col_sed[1]) + 
  geom_line(aes(x = freq, y = Sed_DP_12.15), linewidth = lw, color = col_sed[2]) +
  geom_line(aes(x = freq, y = Sed_DP_12.53), linewidth = lw, color = col_sed[3]) +
  geom_line(aes(x = freq, y = Sed_DP_13.13), linewidth = lw, color = col_sed[4]) +
  geom_line(aes(x = freq, y = Awake_NFd), linewidth = lw, linetype = lt) + 
  geom_line(aes(x = freq, y = Sed_NF_11.36), linewidth = lw, color = col_sed[1], linetype = lt) +
  geom_line(aes(x = freq, y = Sed_NF_12.15), linewidth = lw, color = col_sed[2], linetype = lt) +
  geom_line(aes(x = freq, y = Sed_NF_12.53), linewidth = lw, color = col_sed[3], linetype = lt) +
  geom_line(aes(x = freq, y = Sed_NF_13.13), linewidth = lw, color = col_sed[4], linetype = lt) +
  xlab("Frequency (kHz)") + 
  ylab("DP Amp. (dB EPL)") +
  ylim(-40, 45) +
  scale_x_continuous(trans = 'log10', 
                     breaks = c( .5, 1, 2, 4, 8, 16),  
                     labels = c(".5", "1", "2", "4", "8", "16")) +  
  theme_bw() + 
  theme(text = element_text(size = 11, family = "serif"), 
        legend.position = "top", legend.title = element_blank()) 


SF443 <- ggplot(data = Q443) + 
  geom_line(aes(x = freq, y = Awake_SF), linewidth = lw) + 
  geom_line(aes(x = freq, y = Sed_SF_11.43), linewidth = lw, color = col_sed[1]) + 
  geom_line(aes(x = freq, y = Sed_SF_12.23), linewidth = lw, color = col_sed[2]) +
  geom_line(aes(x = freq, y = Sed_SF_13.02), linewidth = lw, color = col_sed[3]) +
  geom_line(aes(x = freq, y = Sed_SF_13.39), linewidth = lw, color = col_sed[4]) +
  geom_line(aes(x = freq, y = Awake_NFd), linewidth = lw, linetype = lt) + 
  geom_line(aes(x = freq, y = Sed_NF_11.43), linewidth = lw, color = col_sed[1], linetype = lt) +
  geom_line(aes(x = freq, y = Sed_NF_12.23), linewidth = lw, color = col_sed[2], linetype = lt) +
  geom_line(aes(x = freq, y = Sed_NF_13.02), linewidth = lw, color = col_sed[3], linetype = lt) +
  geom_line(aes(x = freq, y = Sed_NF_13.39), linewidth = lw, color = col_sed[4], linetype = lt) +
  
  annotate("text", x = 7.75, y = 44, label = "Awake", color = "black", hjust = "center", family = "serif") + 
  annotate("text", x = 7.75, y = 35, label = "20 min.", color = col_sed[1], hjust = "center", family = "serif") + 
  annotate("text", x = 7.75, y = 26, label = "60 min.", color = col_sed[2], hjust = "center", family = "serif") + 
  annotate("text", x = 7.75, y = 17, label = "100 min.", color = col_sed[3], hjust = "center", family = "serif") + 
  annotate("text", x = 7.75, y = 8, label = "120 min.", color = col_sed[4], hjust = "center", family = "serif") + 
  
  xlab("Frequency (kHz)") + 
  ylab("SF Amp. (dB EPL)") +
  ylim(-40, 45) +
  scale_x_continuous(trans = 'log10', 
                     breaks = c( .5, 1, 2, 4, 8, 16),  
                     labels = c(".5", "1", "2", "4", "8", "16")) +  
  theme_bw() + 
  theme(text = element_text(size = 11, family = "serif"), 
        legend.position = "top", legend.title = element_blank()) 


phase443 <- ggplot(data = Q443) + 
  geom_line(aes(x = freq, y = Awake_phase), linewidth = lw) + 
  geom_line(aes(x = freq, y = Sed_phase_11.43), linewidth = lw, color = col_sed[1]) + 
  geom_line(aes(x = freq, y = Sed_phase_12.23), linewidth = lw, color = col_sed[2]) +
  geom_line(aes(x = freq, y = Sed_phase_13.02), linewidth = lw, color = col_sed[3]) +
  geom_line(aes(x = freq, y = Sed_phase_13.39), linewidth = lw, color = col_sed[4]) +
  xlab("Frequency (kHz)") + 
  ylab("Phase (cycles)") +
  coord_cartesian( ylim = c(-16, 0) )+
  scale_x_continuous(trans = 'log10', 
                     breaks = c( .5, 1, 2, 4, 8, 16),  
                     labels = c(".5", "1", "2", "4", "8", "16")) +  
  theme_bw() + 
  theme(text = element_text(size = 11, family = "serif"), 
        legend.position = "top", legend.title = element_blank()) 

phase443

Qfig443 <- ggplot(data = Q443_Q) + 
  geom_line(aes(x = freq, y = Awake_Q), linewidth = lw) + 
  geom_line(aes(x = freq, y = Sed_Q_11.43), linewidth = lw, color = col_sed[1]) + 
  geom_line(aes(x = freq, y = Sed_Q_12.23), linewidth = lw, color = col_sed[2]) +
  geom_line(aes(x = freq, y = Sed_Q_13.02), linewidth = lw, color = col_sed[3]) +
  geom_line(aes(x = freq, y = Sed_Q_13.39), linewidth = lw, color = col_sed[4]) +
  xlab("Frequency (kHz)") + 
  ylab("Qerb") +
  ylim(0, 20) +
  coord_cartesian(xlim = c(.5,16)) +
  scale_x_continuous(trans = 'log10', 
                     breaks = c( .5, 1, 2, 4, 8, 16),  
                     labels = c(".5", "1", "2", "4", "8", "16")) +  
  annotate("text", x = .75, y = 18, label = "Awake", color = "black", hjust = "center", family = "serif") + 
  annotate("text", x = .75, y = 16, label = "20 min.", color = col_sed[1], hjust = "center", family = "serif") + 
  annotate("text", x = .75, y = 14, label = "60 min.", color = col_sed[2], hjust = "center", family = "serif") + 
  annotate("text", x = .75, y = 12, label = "100 min.", color = col_sed[3], hjust = "center", family = "serif") + 
  annotate("text", x = .75, y = 10, label = "120 min.", color = col_sed[4], hjust = "center", family = "serif") + 
  theme_bw() + 
  theme(text = element_text(size = 11, family = "serif"), 
        legend.position = "top", legend.title = element_blank()) 

Qfig443

top_row <- ggarrange(DP443, SF443, 
                     ncol = 1, nrow = 2, 
                     labels = c("A", "B"),
                     font.label = list(family = "serif"))
top_row
ggsave("./figs/fig-4-6_ampRespOverTime.pdf",
       plot = last_plot(),
       width = 5,
       height = 4,
       units = "in",
       dpi = 600)

ggarrange(phase443, Qfig443, 
          labels = c("A", "B"),
          ncol = 1, 
          nrow = 2, 
          align = "hv", 
          font.label = list(family = "serif"))

ggsave("./figs/fig-4-7_phaseRespOverTime.pdf",
       plot = last_plot(),
       width = 5,
       height = 4,
       units = "in",
       dpi = 600)

# Figure 8: Joint Profiles ----------------------------------------------------------
joint <- inner_join(dpoae2, sfoae2, by = c("Subject","Sedated", "freq"))

jointProfile <- joint %>% select(Subject,Sedated, freq, amp.x, amp.y) %>% 
  rename(DP = amp.x, 
         SF = amp.y)

# Compute centroids
centroids <- jointProfile %>%
  group_by(freq, Sedated) %>%
  summarise(DP = mean(DP, na.rm = T), SF = mean(SF, na.rm = T), .groups = "drop")

# Reshape centroids for arrow plotting
centroids_wide <- centroids %>%
  pivot_wider(names_from = Sedated, values_from = c(DP, SF), names_sep = "_") %>%
  rowwise() %>%
  mutate(
    dx = DP_Sedated - DP_Awake,
    dy = SF_Sedated - SF_Awake,
    mag = sqrt(dx^2 + dy^2),
    shorten = 0.07,  # proportion to shorten from both ends (5%)
    x_start = DP_Awake + shorten * dx,
    y_start = SF_Awake + shorten * dy,
    x_end   = DP_Sedated - shorten * dx,
    y_end   = SF_Sedated - shorten * dy
  )

ggplot() +
  # Raw data points
  geom_point(data = jointProfile,
             aes(x = DP, y = SF, color = as.factor(freq), shape = Sedated),
             size = 1, stroke = 1, alpha = .3) +
  
  # Centroid points
  geom_point(data = centroids, 
             aes(x = DP, y = SF, shape = Sedated, color = as.factor(freq)), 
             size = 1, stroke = 1) +
  
  # Arrows between awake and sedated centroids
  geom_segment(data = centroids_wide,
               aes(x = x_start, y = y_start,
                   xend = x_end, yend = y_end,
                   color = as.factor(freq)),
               arrow = arrow(length = unit(0.07, "inches")),
               size = .5) +
  scale_color_discrete() +
  scale_shape_manual(values = c("Awake" = 17, "Sedated" = 16)) +
  labs(x = "DP Amplitude", y = "SF Amplitude", 
       color = "Frequency", shape = "Condition") +
  theme_bw() + 
  theme(strip.text = element_text(size = 12), text = element_text(size = 12), legend.position = "right")


ggsave("./figs/fig-4-8_jointProfiles.pdf",
       plot = last_plot(),
       width = 5.5,
       height = 4,
       units = "in",
       dpi = 600)

# Other and Phase ---------------------------------------------------------


sfoae <- sfoae %>% mutate(
  phase = - (qerb * as.numeric(freq) / 1.25), 
  tau =  phase * as.numeric(freq)/1000)



# Get averaged response
sfoae_sum <- sfoae %>% group_by(Sedated, freq) %>% 
  summarise(avg = mean(tau, na.rm=T), 
            std = sd(tau, na.rm=T), 
            n = n(), 
            sem = std/sqrt(n))

# Calculate p-values for each frequency (exqerble using t-test)
p_values <- sfoae %>%
  group_by(freq) %>%
  summarize(p_value = t.test(x = tau[Sedated == "Sedated"], y = tau[Sedated == "Awake"], paired = TRUE)$p.value) %>%
  mutate(
    significance = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      TRUE ~ "" # not significant
    )
  )

# Add p-values to the summary data table. 
sfoae_sum <- 
  left_join(sfoae_sum, p_values, by = join_by(freq))

Fig3a_Qerb <- 
  ggplot(
    data = sfoae) +
  geom_line(size = .5, alpha = .1, 
            aes(x = as.numeric(freq), y=tau, color = Sedated,
                group = interaction(Subject, Sedated) )) +
  xlab("Frequency (kHz)") + 
  ylab("Amplitude (dB EPL)") +
  geom_line(size = 1, data = sfoae_sum, aes(x = as.numeric(freq), y = avg, color = Sedated)) +
  geom_errorbar(data = sfoae_sum, 
                aes(x = as.numeric(freq), ymax=avg+sem, ymin=avg-sem, color = Sedated), 
                width = .03, size = 1 )+
  scale_x_continuous(trans = 'log10', 
                     breaks = c( .5, 1, 2, 4, 8, 12),  
                     labels = c(".5", "1", "2", "4", "8", "12")) +  
  scale_color_manual(values = c("Awake" = "black", "Sedated" = col_sed),
                     labels = c("Awake", "Sedated"), 
                     "Status") + 
  theme_bw() + 
  theme(strip.text = element_text(size = 12), text = element_text(size = 12), 
        legend.position = "top", legend.title = element_blank())+
  geom_text(data = (sfoae_sum %>% filter(Sedated == "Sedated")), 
            aes(x = as.numeric(freq), y = avg + sem + 2,
                label = significance), 
            size = 8) # Adjust size as needed

Fig3a_Qerb


















# Now look at difference data for Fig 1B
dpoae_diff <- dpoae2 %>% group_by(Subject, freq, Sex) %>% 
  summarize(Difference = amp[Sedated == "Sedated"] - amp[Sedated == "Awake"])

# Get averaged response
dpoae_diff_sum <- dpoae_diff %>% group_by(freq, Sex) %>% 
  summarise(avg = mean(Difference, na.rm=T), 
            std = sd(Difference, na.rm=T), 
            n = n(), 
            sem = std/sqrt(n))

# Calculate p-values for each frequency (example using t-test)
p_values <- dpoae_diff %>%
  group_by(freq) %>%
  summarize(p_value = t.test(x = Difference[Sex == "M"], y = Difference[Sex == "F"])$p.value) %>%
  mutate(
    significance = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      TRUE ~ "" # not significant
    )
  )

# Add p-values to the summary data table. 
dpoae_diff_sum <- 
  left_join(dpoae_diff_sum, p_values, by = join_by(freq))

Fig4a_DPsex <- 
  ggplot(
    data = dpoae_diff) +
  geom_hline(yintercept = 0, linetype = 'dashed')+
  geom_line(size = .5, alpha = .1, 
            aes(x = freq, y=Difference,
                group = interaction(Subject, Sex), color = Sex )) +
  xlab("Frequency (kHz)") + 
  ylab("\u0394 Amplitude (dB EPL)") +
  geom_line(size = 1, data = dpoae_diff_sum, aes(x = freq, y = avg, group = Sex, color = Sex)) +
  geom_errorbar(data = dpoae_diff_sum, 
                aes(x = freq, ymax=avg+sem, ymin=avg-sem, color = Sex), 
                width = .03, size = 1 )+
  scale_x_continuous(trans = 'log10', 
                     breaks = c( .5, 1, 2, 4, 8, 12),  
                     labels = c(".5", "1", "2", "4", "8", "12")) +  
  scale_color_brewer(palette = "Set1") +
  theme_bw() + 
  theme(strip.text = element_text(size = 12), text = element_text(size = 12), legend.position = "top")+
  geom_text(data = dpoae_diff_sum , 
            aes(x = freq, y = avg + sem + 2,
                label = significance), 
            size = 8) # Adjust size as needed

Fig4a_DPsex

# Now look at difference data for Fig 1B
sfoae_diff <- sfoae2 %>% group_by(Subject, freq, Sex) %>% 
  summarize(Difference = amp[Sedated == "Sedated"] - amp[Sedated == "Awake"])

# Get averaged response
sfoae_diff_sum <- sfoae_diff %>% group_by(freq, Sex) %>% 
  summarise(avg = mean(Difference, na.rm=T), 
            std = sd(Difference, na.rm=T), 
            n = n(), 
            sem = std/sqrt(n))

# Calculate p-values for each frequency (example using t-test)
p_values <- sfoae_diff %>%
  group_by(freq) %>%
  summarize(p_value = t.test(x = Difference[Sex == "M"], y = Difference[Sex == "F"])$p.value) %>%
  mutate(
    significance = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      TRUE ~ "" # not significant
    )
  )

# Add p-values to the summary data table. 
sfoae_diff_sum <- 
  left_join(sfoae_diff_sum, p_values, by = join_by(freq))

Fig4b_SF <- 
  ggplot(
    data = sfoae_diff) +
  geom_hline(yintercept = 0, linetype = 'dashed')+
  geom_line(size = .5, alpha = .1, 
            aes(x = freq, y=Difference,
                group = interaction(Subject, Sex), color = Sex )) +
  xlab("Frequency (kHz)") + 
  ylab("\u0394 Amplitude (dB EPL)") +
  geom_line(size = 1, data = sfoae_diff_sum, aes(x = freq, y = avg, group = Sex, color = Sex)) +
  geom_errorbar(data = sfoae_diff_sum, 
                aes(x = freq, ymax=avg+sem, ymin=avg-sem, color = Sex), 
                width = .03, size = 1 )+
  scale_x_continuous(trans = 'log10', 
                     breaks = c( .5, 1, 2, 4, 8, 12),  
                     labels = c(".5", "1", "2", "4", "8", "12")) +  
  scale_color_brewer(palette = "Set1") +
  theme_bw() + 
  theme(strip.text = element_text(size = 12), text = element_text(size = 12), legend.position = "top")+
  geom_text(data = sfoae_diff_sum , 
            aes(x = freq, y = avg + sem + 2,
                label = significance), 
            size = 8) # Adjust size as needed

Fig4b_SF

mod <- lmer(data = sfoae2, amp ~ freq + Sex*Sedated + (1| Subject))
Anova(mod, test.statistic = 'F')




ggarrange(Fig4a_DPsex + rremove("xlab"), Fig4b_SF,
          labels = c("A", "B"),
          ncol = 1, nrow = 2, 
          align = 'v')


ggsave("./figs/fig-4-1_DPOAE_AwkVsSed.pdf",
       plot = last_plot(),
       width = 6,
       height = 6,
       units = "in",
       dpi = 600)