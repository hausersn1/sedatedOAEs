# Code for Statistical analysis and Plots from 
# Hauser et al., 2025

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

# Set up data structure ---------------------------------------------------

oae$freqFactor <- as.ordered(as.factor(oae$freq))
oae$Sedated <- as.factor(oae$Sedated)
oae <- rename(oae, amp_sf = amp.x, amp_dp = amp.y)
oae$Sex <- as.factor(oae$Sex)
oae$Subject <- as.factor(oae$Subject)

# Stats -------------------------------------------------------------------

mod_dp <- lmer(amp_dp ~ freqFactor * Sedated + (1|Subject), data = oae)
mod_sf <- lmer(amp_sf ~ freqFactor * Sedated + (1|Subject), data = oae)
mod_qerb <- lmer(qerb ~ freqFactor * Sedated + (1|Subject), data = oae)

Anova(mod_dp, test.statistic = 'F')
Anova(mod_sf, test.statistic = 'F')
Anova(mod_qerb, test.statistic = 'F')

em_dp <- emmeans(mod_dp, ~  Sedated | freqFactor)
stats_dp <- as.data.frame(pairs(em_dp))

em_sf <- emmeans(mod_sf, ~  Sedated | freqFactor)
stats_sf <- as.data.frame(pairs(em_sf))

em_qerb <- emmeans(mod_qerb, ~  Sedated | freqFactor)
stats_q <- as.data.frame(pairs(em_qerb))

# Figure 1: Single Chin ---------------------------------------------------

lw <- 1
col_sed <- '#43a2ca'
lt <- 'dashed'

## Make Fig. 1A
oneChinDP <- 
  ggplot(data = oneChin) + 
  geom_line(aes(x = freq, y = A_dpOAE), linewidth = lw/1.5) + 
  geom_line(aes(x = freq, y = S_dpOAE), linewidth = lw, color = col_sed) + 
  geom_line(aes(x = freq, y = A_dpNF), linewidth = lw/2, linetype = lt) + 
  geom_line(aes(x = freq, y = S_dpNF), linewidth = lw/2, color = col_sed, linetype = lt) +
  xlab("Frequency (kHz)") + 
  ylab("Amplitude (dB EPL)") +
  ylim(-40, 50) +
  ggtitle("DPOAE") +
  scale_x_continuous(trans = 'log10', 
                     breaks = c( .5, 1, 2, 4, 8, 12),  
                     labels = c(".5", "1", "2", "4", "8", "12")) +  
  theme_bw() + 
  theme(text = element_text(size = 11, family = "sans"), 
        legend.position = "top", legend.title = element_blank()) 

## Make Fig 1B
oneChinSF <- 
  ggplot(data = oneChin) + 
  geom_line(aes(x = freq, y = A_sfOAE), linewidth = lw/1.5) + 
  geom_line(aes(x = freq, y = S_sfOAE), linewidth = lw, color = col_sed) + 
  geom_line(aes(x = freq, y = A_sfNF), linewidth = lw/2, linetype = lt) + 
  geom_line(aes(x = freq, y = S_sfNF), linewidth = lw/2, color = col_sed, linetype = lt) +
  xlab("Frequency (kHz)") + 
  ylab("Amplitude (dB EPL)") +
  ylim(-40, 50) +
  ggtitle("SFOAE") +
  scale_x_continuous(trans = 'log10', 
                     breaks = c( .5, 1, 2, 4, 8, 12),  
                     labels = c(".5", "1", "2", "4", "8", "12")) +  
  annotate("text", x = 14, y = 47, label = "Awake", size = 4, hjust = "right", 
           color = "#000000", family = "sans") +   
  annotate("text", x = 14, y = 37, label = "Sedated", size = 4, hjust = "right", 
           color = col_sed, family = "sans") + 
  theme_bw() + 
  theme(text = element_text(size = 11,  family = "sans"), 
        legend.position = "top") 

ggarrange(oneChinDP , oneChinSF+ rremove("ylab"), 
          labels = c("A", "B"),
          ncol = 2, nrow = 1, 
          align = 'v', 
          common.legend = TRUE )

## Print Figure
ggsave("./figs/fig1.tiff",
       plot = last_plot(),
       width = 170,
       height = 60,
       units = "mm",
       dpi = 600)

# Figure 2: DPOAE Awake vs Sedated -------------------------------------------------

## Get averaged response
dpoae_sum <- oae %>% group_by(Sedated, freq) %>% 
  summarise(avg = mean(amp_dp, na.rm=T), 
            std = sd(amp_dp, na.rm=T), 
            n = n(), 
            sem = std/sqrt(n))

## Get stars for p-values
p_values <- stats_dp %>% 
  mutate(
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ "" # not significant
    )
  )

## Plot Fig. 2A
Fig2a_DP <- 
  ggplot(
    data = oae) +
  geom_line(size = .5, alpha = .1, 
            aes(x = freq, y=amp_dp, color = Sedated,
                group = interaction(Subject, Sedated) )) +
  xlab("Frequency (kHz)") + 
  ylab("Amplitude (dB EPL)") +
  geom_line(size = .75, data = dpoae_sum, aes(x = freq, y = avg, color = Sedated)) +
  geom_errorbar(data = dpoae_sum, 
                aes(x = freq, ymax=avg+sem, ymin=avg-sem, color = Sedated), 
                width = .03, size = .75 )+
  scale_x_continuous(trans = 'log10', 
                     breaks = c( .5, 1, 2, 4, 8, 12),  
                     labels = c(".5", "1", "2", "4", "8", "12")) +  
  scale_color_manual(values = c("Awake" = "black", "Sedated" = col_sed),
                     labels = c("Awake", "Sedated"), 
                     "Status") + 
  theme_bw() + 
  theme(text = element_text(size = 11, family = "sans"), 
        legend.position = "top", legend.title = element_blank()) +
  geom_text(data = (dpoae_sum %>% filter(Sedated == "Sedated")), 
            aes(x = freq, y = avg + sem + 2,
                label = p_values$significance), 
            size = 4) 

## Now look at difference data for Fig 2B
dpoae_diff <- oae %>% 
  group_by(Subject, freq) %>% 
  summarize(Difference = amp_dp[Sedated == "Sedated"] - amp_dp[Sedated == "Awake"])

dpoae_diff_sum <- dpoae_diff %>% group_by(freq) %>% 
  summarise(avg = mean(Difference, na.rm=T), 
            std = sd(Difference, na.rm=T), 
            n = n(), 
            sem = std/sqrt(n))

## Get stars 
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

## Add p-values to the summary data table. 
dpoae_diff_sum <- 
  left_join(dpoae_diff_sum, p_values, by = join_by(freq))

Fig2b_DP <- 
  ggplot(
    data = dpoae_diff) +
  geom_hline(yintercept = 0, linetype = 'dashed', color = "#808080")+
  geom_line(size = .5, alpha = .1, 
            aes(x = freq, y=Difference,
                group = Subject )) +
  xlab("Frequency (kHz)") + 
  ylab("\u0394 Amplitude (dB)") +
  geom_line(size = .75, data = dpoae_diff_sum, aes(x = freq, y = avg)) +
  geom_errorbar(data = dpoae_diff_sum, 
                aes(x = freq, ymax=avg+sem, ymin=avg-sem), 
                width = .03, size = .75 )+
  scale_x_continuous(trans = 'log10', 
                     breaks = c( .5, 1, 2, 4, 8, 12),  
                     labels = c(".5", "1", "2", "4", "8", "12")) +  
  annotate("text", x = .7, y = 21, label = "Higher Sedated", size = 4, hjust = "left", 
           color = "#808080", family = "sans") + 
  annotate("text", x = .7, y = -13, label = "Higher Awake", size = 4, hjust = "left", 
           color = "#808080", family = "sans") + 
  geom_text(data = dpoae_diff_sum , 
            aes(x = freq, y = avg + sem + 2,
                label = significance), 
            size = 4) + 
  theme_bw() + 
  theme(text = element_text(size = 11, family = "sans"), 
        legend.position = "top", legend.title = element_blank()) 

## Combine Figures
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

# Get averaged response
sfoae_sum <- oae %>% group_by(Sedated, freq) %>% 
  summarise(avg = mean(amp_sf, na.rm=T), 
            std = sd(amp_sf, na.rm=T), 
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
    data = oae) +
  geom_line(size = .5, alpha = .1, 
            aes(x = freq, y=amp_sf, color = Sedated,
                group = interaction(Subject, Sedated) )) +
  xlab("Frequency (kHz)") + 
  ylab("Amplitude (dB EPL)") +
  geom_line(size = .75, data = sfoae_sum, aes(x = freq, y = avg, color = Sedated)) +
  geom_errorbar(data = sfoae_sum, 
                aes(x = freq, ymax=avg+sem, ymin=avg-sem, color = Sedated), 
                width = .03, size = .75 )+
  scale_x_continuous(trans = 'log10', 
                     breaks = c( .5, 1, 2, 4, 8, 12),  
                     labels = c(".5", "1", "2", "4", "8", "12")) +  
  scale_color_manual(values = c("Awake" = "black", "Sedated" = col_sed),
                     labels = c("Awake", "Sedated"), 
                     "Status") + 
  geom_text(data = (sfoae_sum %>% filter(Sedated == "Awake")), 
            aes(x = freq, y = avg + sem + 2,
                label = p_values$significance), 
            size = 4) + 
  theme_bw() + 
  theme(text = element_text(size = 11, family = "sans"), 
        legend.position = "top", legend.title = element_blank()) 


Fig2a_SF

# Now look at difference data for Fig 1B
sfoae_diff <- oae %>% group_by(Subject, freq) %>% 
  summarize(Difference = amp_sf[Sedated == "Sedated"] - amp_sf[Sedated == "Awake"])

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
  ylab("\u0394 Amplitude (dB)") +
  geom_line(size = .75, data = sfoae_diff_sum, aes(x = freq, y = avg)) +
  geom_errorbar(data = sfoae_diff_sum, 
                aes(x = freq, ymax=avg+sem, ymin=avg-sem), 
                width = .03, size = .75 )+
  scale_x_continuous(trans = 'log10', 
                     breaks = c( .5, 1, 2, 4, 8, 12),  
                     labels = c(".5", "1", "2", "4", "8", "12")) +  
  annotate("text", x = 12, y = 16, label = "Higher Sedated", size = 4, hjust = "right", 
           color = "#808080", family = "sans") + 
  annotate("text", x = .7, y = -21, label = "Higher Awake", size = 4, hjust = "left", 
           color = "#808080", family = "sans") + 
  geom_text(data = sfoae_diff_sum , 
            aes(x = freq, y = avg + sem + 1,
                label = significance), 
            size = 4) + 
  theme_bw() + 
  theme(text = element_text(size = 11, family = "sans"), 
        legend.position = "top", legend.title = element_blank()) 

Fig2b_SF

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


# Get averaged response
sfoae_sum <- oae %>% group_by(Sedated, freq) %>% 
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
    data = oae) +
  geom_line(size = .5, alpha = .1, 
            aes(x = freq, y=qerb, color = Sedated,
                group = interaction(Subject, Sedated) )) +
  xlab("Frequency (kHz)") + 
  ylab("Qerb Estimate") +
  geom_line(size = .75, data = sfoae_sum, aes(x = freq, y = avg, color = Sedated)) +
  geom_errorbar(data = sfoae_sum, 
                aes(x = freq, ymax=avg+sem, ymin=avg-sem, color = Sedated), 
                width = .03, size = .75 )+
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
            size = 4) + # Adjust size as needed
  theme_bw() + 
  theme(text = element_text(size = 11, family = "sans"), 
        legend.position = "top", legend.title = element_blank()) 

Fig3a_Qerb

# Now look at difference data for Fig 1B
sfoae_diff <- oae %>% group_by(Subject, freq) %>% 
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
  geom_line(size = .75, data = sfoae_diff_sum2, aes(x = freq, y = avg)) +
  geom_errorbar(data = sfoae_diff_sum2, 
                aes(x = freq, ymax=avg+sem, ymin=avg-sem), 
                width = .03, size = .75 )+
  geom_text(data = (sfoae_diff_sum2), 
            aes(x = freq, y = avg + sem + .5,
                label = p_values$significance), 
            size = 4) + # Adjust size as needed
  scale_x_continuous(trans = 'log10', 
                     breaks = c( .5, 1, 2, 4, 8, 12),  
                     labels = c(".5", "1", "2", "4", "8", "12")) +  
  theme_bw() + 
  theme(text = element_text(size = 11, family = "sans"), 
        legend.position = "top", legend.title = element_blank()) 

Fig3b_Qerb

# Combine into one figure
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

# Now look at difference data for Fig 1B
sfoae_diff <- oae %>% group_by(Subject, freq, Sex) %>% 
  summarize(Difference = amp_dp[Sedated == "Sedated"] - amp_dp[Sedated == "Awake"])
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
  geom_line(size = .75, data = sfoae_diff_sum, aes(x = freq, y = avg, color = Sex)) +
  geom_errorbar(data = sfoae_diff_sum, 
                aes(x = freq, ymax=avg+sem, ymin=avg-sem, color = Sex), 
                width = .03, size = .75 )+
  scale_x_continuous(trans = 'log10', 
                     breaks = c( .5, 1, 2, 4, 8, 12),  
                     labels = c(".5", "1", "2", "4", "8", "12")) +  
  annotate("text", x = .7, y = 24, label = "Higher Sedated", size = 3, hjust = "left", 
           color = "#808080", family = "sans") + 
  annotate("text", x = .7, y = -21, label = "Higher Awake", size =3, hjust = "left", 
           color = "#808080", family = "sans") + 
  theme_bw() + 
  theme(text = element_text(size = 11, family = "sans"), 
        legend.position = "top", legend.title = element_blank()) 
Fig5a

# Now look at difference data for Fig 1B
sfoae_diff <- oae %>% group_by(Subject, freq, Sex) %>% 
  summarize(Difference = amp_sf[Sedated == "Sedated"] - amp_sf[Sedated == "Awake"])
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
  geom_line(size = .75, data = sfoae_diff_sum, aes(x = freq, y = avg, color = Sex)) +
  geom_errorbar(data = sfoae_diff_sum, 
                aes(x = freq, ymax=avg+sem, ymin=avg-sem, color = Sex), 
                width = .03, size = .75 )+
  scale_x_continuous(trans = 'log10', 
                     breaks = c( .5, 1, 2, 4, 8, 12),  
                     labels = c(".5", "1", "2", "4", "8", "12")) +  
  annotate("text", x = .7, y = 24, label = "Higher Sedated", size = 3, hjust = "left", 
           color = "#808080", family = "sans") + 
  annotate("text", x = .7, y = -26, label = "Higher Awake", size = 3, hjust = "left", 
           color = "#808080", family = "sans") + 
  theme_bw() + 
  theme(text = element_text(size = 11, family = "sans"), 
        legend.position = "top", legend.title = element_blank()) 
Fig5b


# Now look at difference data for Fig 1B
sfoae_diff <- oae %>% group_by(Subject, freq, Sex) %>% 
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
  geom_line(size = .75, data = sfoae_diff_sum, aes(x = freq, y = avg, color = Sex)) +
  geom_errorbar(data = sfoae_diff_sum, 
                aes(x = freq, ymax=avg+sem, ymin=avg-sem, color = Sex), 
                width = .03, size = .75 )+
  scale_x_continuous(trans = 'log10', 
                     breaks = c( .5, 1, 2, 4, 8, 12),  
                     labels = c(".5", "1", "2", "4", "8", "12")) +  
  annotate("text", x = .7, y = 14, label = "Higher Sedated", size = 3, hjust = "left", 
           color = "#808080", family = "sans") + 
  annotate("text", x = .7, y = -10.5, label = "Higher Awake", size = 3, hjust = "left", 
           color = "#808080", family = "sans") + 
  theme_bw() + 
  theme(text = element_text(size = 11, family = "sans"), 
        legend.position = "top", legend.title = element_blank()) 
Fig5c


ggarrange(Fig5a, Fig5b, Fig5c, 
          labels = c("A", "B", "C"),
          vjust = 0,
          ncol = 1, 
          nrow = 3, 
          common.legend = TRUE, 
          legend = "top", 
          font.label = list(family = "sans"))


ggsave("./figs/fig5.tiff",
       plot = last_plot(),
       width = 84,
       height = 150,
       units = "mm",
       dpi = 600)

mod_dpS <- lmer(amp_dp ~ freq * Sedated +  Sedated:Sex + Sex +  (1|Subject), data = oae)
mod_sfS<- lmer(amp_sf ~ freq * Sedated  +  Sedated:Sex +Sex + (1|Subject), data = oae)
mod_qerbS <- lmer(qerb ~ freq * Sedated  +  Sedated:Sex + Sex + (1|Subject), data = oae)

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
  theme(text = element_text(size = 11, family = "sans"), 
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
  
  annotate("text", x = 7.75, y = 44, label = "Awake", color = "black", hjust = "center", family = "sans") + 
  annotate("text", x = 7.75, y = 35, label = "20 min.", color = col_sed[1], hjust = "center", family = "sans") + 
  annotate("text", x = 7.75, y = 26, label = "60 min.", color = col_sed[2], hjust = "center", family = "sans") + 
  annotate("text", x = 7.75, y = 17, label = "100 min.", color = col_sed[3], hjust = "center", family = "sans") + 
  annotate("text", x = 7.75, y = 8, label = "120 min.", color = col_sed[4], hjust = "center", family = "sans") + 
  
  xlab("Frequency (kHz)") + 
  ylab("SF Amp. (dB EPL)") +
  ylim(-40, 45) +
  scale_x_continuous(trans = 'log10', 
                     breaks = c( .5, 1, 2, 4, 8, 16),  
                     labels = c(".5", "1", "2", "4", "8", "16")) +  
  theme_bw() + 
  theme(text = element_text(size = 11, family = "sans"), 
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
  theme(text = element_text(size = 11, family = "sans"), 
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
  annotate("text", x = .75, y = 18, label = "Awake", color = "black", hjust = "center", family = "sans") + 
  annotate("text", x = .75, y = 16, label = "20 min.", color = col_sed[1], hjust = "center", family = "sans") + 
  annotate("text", x = .75, y = 14, label = "60 min.", color = col_sed[2], hjust = "center", family = "sans") + 
  annotate("text", x = .75, y = 12, label = "100 min.", color = col_sed[3], hjust = "center", family = "sans") + 
  annotate("text", x = .75, y = 10, label = "120 min.", color = col_sed[4], hjust = "center", family = "sans") + 
  theme_bw() + 
  theme(text = element_text(size = 11, family = "sans"), 
        legend.position = "top", legend.title = element_blank()) 

Qfig443

top_row <- ggarrange(DP443, SF443, 
                     ncol = 1, nrow = 2, 
                     labels = c("A", "B"),
                     font.label = list(family = "sans"))
top_row
ggsave("./figs/fig6.tiff",
       plot = last_plot(),
       width = 84,
       height = 100,
       units = "mm",
       dpi = 600)

ggarrange(phase443, Qfig443, 
          labels = c("A", "B"),
          ncol = 1, 
          nrow = 2, 
          align = "hv", 
          font.label = list(family = "sans"))

ggsave("./figs/fig7.tiff",
       plot = last_plot(),
       width = 84,
       height = 100,
       units = "mm",
       dpi = 600)











