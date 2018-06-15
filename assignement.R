setwd ("D://R_course_PhD/advanced_statistics")

# EXPLORATORY ANALYSIS OF A PROLONGED VERSION OF ASRT TO REVEAL WHETHER HABIT FORMATION, SKILL
# AUTOMATIZATION IS ASSOCIATED WITH WORKING MEMORY PERFORMANCE IN CONTRAST WITH LEARNING PERFORMANCE? 


# basic ASRT analysis with bayesian statistics and correlational analysis included
# is building a regression model with avg rt, avg acc & composit neuropsychological scores meaningful?
# how to define the offset of acqusition and onset of automatization? - based on the sample performance

# required packages for data manipulation, exploratoy analysis & data visualization
# do i need some other packages for variance analysis and bayesian statistics?
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readxl)
library(ggpubr)
library(tidyr)
library(corrplot)
library(plotrix)

gsrt_raw <- read_excel("gsrt_data_by_block.xlsx")

# count the number of participants, observations, NA-s

N_participants <- gsrt_raw %>% 
  group_by(ID) %>% 
  count(ID)
N_blocks <- gsrt_raw %>% 
  group_by(block) %>% 
  count(block)
N_epochs <- gsrt_raw %>% 
  group_by(epoch) %>% 
  count(epoch)

# search for missing neuropsychological data

NA_neuropsy <- gsrt_raw %>% 
  select(digit_span, counting_span, wcst_pers_error_rate, ant_alerting, ant_orienting, 
         ant_executive, gng_dprime, gng_false_alarm_rate, running_span) %>% 
  summarise_all(funs(sum(is.na(.))))

NA_neuropsy_row <- gsrt_raw %>% 
  filter(block == 1) %>% 
  select(ID, digit_span, counting_span, wcst_pers_error_rate, ant_alerting, ant_orienting, 
         ant_executive, gng_dprime, gng_false_alarm_rate, running_span)

apply(NA_neuropsy_row, MARGIN = 1, function(x) sum(is.na(x)))  

# ID_104 is missing most of the neuropsychological data, so he/she is excluded from further analyses.

gsrt_30 <- gsrt_raw %>% 
  filter(ID != "ID_104")

# calculating means for the neuropsychological data

digit_span <- NA_neuropsy_row %>% 
  summarize(mean_ds = mean(digit_span),
            sd_ds = sd(digit_span),
            min_ds = min(digit_span),
            max_ds = max(digit_span))
counting_span <- NA_neuropsy_row %>% 
  summarize(mean_cs = mean(counting_span),
            sd_cs = sd(counting_span),
            min_cs = min(counting_span),
            max_cs = max(counting_span))
ANT <- NA_neuropsy_row %>% 
  drop_na() %>% 
  summarize(mean_o = mean(ant_orienting),
            sd_o = sd(ant_orienting),
            min_o = min(ant_orienting),
            max_o = max(ant_orienting),
            mean_a = mean(ant_alerting),
            sd_a = sd(ant_alerting),
            min_a = min(ant_alerting),
            max_a = max(ant_alerting),
            mean_e = mean(ant_executive),
            sd_e = sd(ant_executive),
            min_e = min(ant_executive),
            max_e = max(ant_executive))
go_no_go <- NA_neuropsy_row %>% 
  drop_na() %>% 
  summarize(mean_dp = mean(gng_dprime),
            sd_dp = sd(gng_dprime),
            min_dp = min(gng_dprime),
            max_dp = max(gng_dprime),
            mean_fa = mean(gng_false_alarm_rate),
            sd_fa = sd(gng_false_alarm_rate),
            min_fa = min(gng_false_alarm_rate),
            max_fa = max(gng_false_alarm_rate))
running_span <- NA_neuropsy_row %>% 
  drop_na() %>% 
  summarize(mean_rs = mean(running_span),
            sd_rs = sd(running_span),
            min_rs = min(running_span),
            max_rs = max(running_span))
WCST <- NA_neuropsy_row %>% 
  drop_na() %>% 
  summarize(mean_pe = mean(wcst_pers_error_rate),
            sd_pe = sd(wcst_pers_error_rate),
            min_pe = min(wcst_pers_error_rate),
            max_pe = max(wcst_pers_error_rate))

# boxplot for ouliers on the ASRT task

#function for detecting outliers
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))}

gsrt_30 <- gsrt_30 %>%
  group_by(epoch) %>% 
  mutate(outlier_H_rt = ifelse(is_outlier(H_rt), ID,"")) %>% 
  mutate(outlier_L_rt = ifelse(is_outlier(L_rt), ID,"")) %>% 
  mutate(outlier_H_acc = ifelse(is_outlier(H_acc), ID,"")) %>% 
  mutate(outlier_L_acc = ifelse(is_outlier(L_acc), ID,""))

ggplot(gsrt_30, aes(y = H_rt, x = factor(epoch))) +
  geom_boxplot(notch = TRUE)+
  labs(y = "RTs for high frequency-high probability elements", x = "Epochs",
       title = "Outlier exploration I.") +
  coord_cartesian(ylim = c(200, 480), xlim = c(0, 10)) +
  geom_text(aes(label = outlier_H_rt, hjust = -0.3), size = 2.3)
  
ggplot(gsrt_30, aes(y = L_rt, x = factor(epoch))) +
  geom_boxplot(notch = TRUE)+
  labs(y = "RTs for low frequency-low probability elements", x = "Epochs",
       title = "Outlier exploration II.") +
  coord_cartesian(ylim = c(220, 500), xlim = c(0, 10)) +
  geom_text(aes(label = outlier_L_rt, hjust = -0.3), size = 2.3)


ggplot(gsrt_30, aes(y = H_acc, x = factor(epoch))) +
  geom_boxplot(notch = TRUE)+
  labs(y = "Accuracy on high frequency-high probability elements", x = "Epochs",
       title = "Outlier exploration III.") +
  coord_cartesian(ylim = c(0.6, 1), xlim = c(0, 10)) +
  geom_text(aes(label = outlier_H_acc, hjust = -0.3), size = 2.3)

ggplot(gsrt_30, aes(y = L_acc, x = factor(epoch))) +
  geom_boxplot(notch = TRUE)+
  labs(y = "Accuracy on low frequency-low probability elements", x = "Epochs",
       title = "Outlier exploration IV.") +
  coord_cartesian(ylim = c(0.5, 1), xlim = c(0, 10)) +
  geom_text(aes(label = outlier_L_acc, hjust = -0.3), size = 2.3)


ggplot(gsrt_30, aes(y = H_rt, x = block, color = ID)) +
  geom_path() +
  labs(y = "Reaction time (ms)", x = "Blocks") +
  coord_cartesian(ylim = c(200, 500))

ggplot(gsrt_30, aes(y = L_rt, x = block, color = ID)) +
  geom_path() +
  labs(y = "Reaction time (ms)", x = "Blocks") +
  coord_cartesian(ylim = c(200, 500))  

ggplot(gsrt_30, aes(y = H_acc, x = block, color = ID)) +
  geom_path() +
  labs(y = "Accuracy", x = "Blocks")

ggplot(gsrt_30, aes(y = L_acc, x = block, color = ID)) +
  geom_path() +
  labs(y = "Accuracy", x = "Blocks")

# Noone is excluded based on accuracy, however ID_106 is excluded due to
# outlier performance (i.e. faster RTs on both measurments during
# the whole task compared to the sample).

gsrt_29 <- gsrt_30 %>% 
  filter(ID != "ID_106")

# Conducting repeated measures ANOVA on learning scores

gsrt_anova <- gsrt_29 %>%
  mutate(LH_dif_rt = L_rt - H_rt) %>% 
  mutate(HL_dif_acc = H_acc - L_acc)

shapiro.test(gsrt_anova$LH_dif_rt)
shapiro.test(gsrt_anova$HL_dif_acc)

#not normal :(

ggqqplot(gsrt_anova$LH_dif_rt)
ggqqplot(gsrt_anova$HL_dif_acc)

ggplot(gsrt_anova, aes(x = LH_dif_rt))+
  geom_density()
ggplot(gsrt_anova, aes(x = HL_dif_acc))+
  geom_density()

# but based on the plots, normality is not extremly violated and RM ANOVA is
# sensitive only to extreme violations, hence there's no need for robust tests.

gsrt_anova$ID <- as.factor(gsrt_anova$ID)

anova_time_rt <- aov(LH_dif_rt ~ epoch + Error(ID/epoch), data=gsrt_anova)
summary(anova_time_rt)

anova_time_acc <- aov(HL_dif_acc ~ epoch + Error(ID/epoch), data=gsrt_anova)
summary(anova_time_acc)

pairwise.t.test(gsrt_anova$LH_dif_rt, gsrt_anova$epoch, p.adj = "holm")
pairwise.t.test(gsrt_anova$HL_dif_acc, gsrt_anova$epoch, p.adj = "holm")

# visualize ASRT performance

gsrt_plot <- gsrt_anova %>% 
  group_by(epoch) %>% 
  mutate(avg_h_rt = mean(H_rt)) %>% 
  mutate(avg_l_rt = mean(L_rt)) %>% 
  mutate(avg_h_acc = mean(H_acc)) %>% 
  mutate(avg_l_acc = mean(L_acc)) %>% 
  mutate(std_error_h_rt = std.error(H_rt, na.rm)) %>% 
  mutate(std_error_l_rt = std.error(L_rt, na.rm)) %>% 
  mutate(std_error_h_acc = std.error(H_acc, na.rm)) %>% 
  mutate(std_error_l_acc = std.error(L_acc, na.rm)) %>% 
  ungroup()

ggplot(gsrt_plot)+
  geom_point(mapping = (aes(y = gsrt_plot$avg_h_rt, x = factor(epoch),
                            color = "High frequency &
  probability trials")), size = 2) +
  geom_point(mapping = (aes(y = gsrt_plot$avg_l_rt, x = factor(epoch),
                            color = "Low frequency &
  probability trials")), size = 2) +
  geom_line(mapping = aes(x = epoch, y = gsrt_plot$avg_h_rt,
                          color = "High frequency &
  probability trials"), size = 1) +
  geom_line(mapping = aes(x = epoch, y = gsrt_plot$avg_l_rt,
                          color = "Low frequency &
  probability trials"), size = 1) +
  geom_errorbar(aes(x = factor(epoch), ymin = gsrt_plot$avg_h_rt - gsrt_plot$std_error_h_rt, 
                    ymax = gsrt_plot$avg_h_rt + gsrt_plot$std_error_h_rt),
                position = "dodge", width = 0.2, color = "grey60") +
  geom_errorbar(aes(x = factor(epoch), ymin = gsrt_plot$avg_l_rt - gsrt_plot$std_error_l_rt,
                    ymax = gsrt_plot$avg_l_rt + gsrt_plot$std_error_l_rt),
                position = "dodge", width = 0.2, color = "grey60") +
  labs(y = "Difference score of high and
  low frequency&probability trials (ms)",
       x = "Epochs", title = "Habit learning & acquisition", caption = "Error bars represent the standard error of means") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"),
        plot.title = element_text(color = "grey5", size = 12),
        text = element_text(color = "grey25", size = 10)) +
  coord_cartesian(ylim = c(275, 375))
ggsave("reaction_time.jpg")

ggplot(gsrt_plot)+
  geom_point(mapping = (aes(y = gsrt_plot$avg_h_acc, x = factor(epoch),
                            color = "High frequency &
  probability trials")), size = 2) +
  geom_point(mapping = (aes(y = gsrt_plot$avg_l_acc, x = factor(epoch),
                            color = "Low frequency &
  probability trials")), size = 2) +
  geom_line(mapping = aes(x = epoch, y = gsrt_plot$avg_h_acc,
                          color = "High frequency &
  probability trials"), size = 1) +
  geom_line(mapping = aes(x = epoch, y = gsrt_plot$avg_l_acc,
                          color = "Low frequency &
  probability trials"), size = 1) +
  geom_errorbar(aes(x = factor(epoch), ymin = gsrt_plot$avg_h_acc - gsrt_plot$std_error_h_acc, 
                    ymax = gsrt_plot$avg_h_acc + gsrt_plot$std_error_h_acc),
                position = "dodge", width = 0.2, color = "grey60") +
  geom_errorbar(aes(x = factor(epoch), ymin = gsrt_plot$avg_l_acc - gsrt_plot$std_error_l_acc,
                    ymax = gsrt_plot$avg_l_acc + gsrt_plot$std_error_l_acc),
                position = "dodge", width = 0.2, color = "grey60") +
  labs(y = "Difference score of high and
  low frequency&probability trials (accuracy)",
       x = "Epochs", title = "Habit learning & acquisition", caption = "Error bars represent the stadard error of mean") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"),
        plot.title = element_text(color = "grey5", size = 12),
        text = element_text(color = "grey25", size = 10)) +
  coord_cartesian(ylim = c(0.85, 1))
ggsave("accuracy.jpg", width = 10, height = 7)

# Offset of further learning is defined in the 4th epoch, so lets split the data

gsrt_corr_learning <- gsrt_plot %>% 
  filter(block == 1 | block == 6 | block == 11 | 
         block == 16 | block == 21 | block == 26 |
         block == 31 | block == 36 | block == 41) %>% 
  filter(epoch <= 4) %>% 
  drop_na()

# visualize the relationship between learning performance in RT and neuropsychological measurments.
# avergae accuracy and reaction time is also tested.

ggplot(gsrt_corr_learning) +
  geom_smooth(method = lm, mapping = aes(y = digit_span, x = LH_dif_rt)) +
  geom_point(mapping = aes(y = digit_span, x = LH_dif_rt, color = factor(epoch))) +
  labs(y = "Verbal short term memory", x = "learning performance")
ggplot(gsrt_corr_learning) +
  geom_smooth(method = lm, mapping = aes(y = counting_span, x = LH_dif_rt)) +
  geom_point(mapping = aes(y = counting_span, x = LH_dif_rt, color = factor(epoch))) +
  labs(y = "Central executive", x = "learning performance")
ggplot(gsrt_corr_learning) +
  geom_smooth(method = lm, mapping = aes(y = running_span, x = LH_dif_rt)) +
  geom_point(mapping = aes(y = running_span, x = LH_dif_rt, color = factor(epoch))) +
  labs(y = "Updating", x = "learning performance")
ggplot(gsrt_corr_learning) +
  geom_smooth(method = lm, mapping = aes(y = ant_orienting, x = LH_dif_rt)) +
  geom_point(mapping = aes(y = ant_orienting, x = LH_dif_rt, color = factor(epoch))) +
  labs(y = "Orienting attention", x = "learning performance")
ggplot(gsrt_corr_learning) +
  geom_smooth(method = lm, mapping = aes(y = ant_alerting, x = LH_dif_rt)) +
  geom_point(mapping = aes(y = ant_alerting, x = LH_dif_rt, color = factor(epoch))) +
  labs(y = "Alertingattention", x = "learning performance")
ggplot(gsrt_corr_learning) +
  geom_smooth(method = lm, mapping = aes(y = ant_executive, x = LH_dif_rt)) +
  geom_point(mapping = aes(y = ant_executive, x = LH_dif_rt, color = factor(epoch))) +
  labs(y = "Executive attention", x = "learning performance")
ggplot(gsrt_corr_learning) +
  geom_smooth(method = lm, mapping = aes(y = wcst_pers_error_rate, x = LH_dif_rt)) +
  geom_point(mapping = aes(y = wcst_pers_error_rate, x = LH_dif_rt, color = factor(epoch))) +
  labs(y = "Shifting", x = "learning performance")
ggplot(gsrt_corr_learning) +
  geom_smooth(method = lm, mapping = aes(y = gng_dprime, x = LH_dif_rt)) +
  geom_point(mapping = aes(y = gng_dprime, x = LH_dif_rt, color = factor(epoch))) +
  labs(y = "Sensitivity", x = "learning performance")
ggplot(gsrt_corr_learning) +
  geom_smooth(method = lm, mapping = aes(y = gng_false_alarm_rate, x = LH_dif_rt)) +
  geom_point(mapping = aes(y = gng_false_alarm_rate, x = LH_dif_rt, color = factor(epoch))) +
  labs(y = "Inhibition", x = "learning performance")

ggplot(gsrt_corr_learning) +
  geom_smooth(method = lm, mapping = aes(y = avg_rt, x = LH_dif_rt)) +
  geom_point(mapping = aes(y = avg_rt, x = LH_dif_rt, color = factor(epoch))) +
  labs(y = "accuracy", x = "learning performance")
ggplot(gsrt_corr_learning) +
  geom_smooth(method = lm, mapping = aes(y = avg_acc, x = LH_dif_rt)) +
  geom_point(mapping = aes(y = avg_acc, x = LH_dif_rt, color = factor(epoch))) +
  labs(y = "reaction time", x = "learning performance")


gsrt_corr_habit <- gsrt_plot %>% 
  filter(block == 1 | block == 6 | block == 11 | 
           block == 16 | block == 21 | block == 26 |
           block == 31 | block == 36 | block == 41) %>% 
  filter(epoch > 4) %>% 
  drop_na()

ggplot(gsrt_corr_habit) +
  geom_smooth(method = lm, mapping = aes(y = digit_span, x = LH_dif_rt)) +
  geom_point(mapping = aes(y = digit_span, x = LH_dif_rt, color = factor(epoch))) +
  labs(y = "Verbal short term memory", x = "habit acquisition performance")
ggplot(gsrt_corr_habit) +
  geom_smooth(method = lm, mapping = aes(y = counting_span, x = LH_dif_rt)) +
  geom_point(mapping = aes(y = counting_span, x = LH_dif_rt, color = factor(epoch))) +
  labs(y = "Central executive", x = "habit acquisition performance")
ggplot(gsrt_corr_habit) +
  geom_smooth(method = lm, mapping = aes(y = running_span, x = LH_dif_rt)) +
  geom_point(mapping = aes(y = running_span, x = LH_dif_rt, color = factor(epoch))) +
  labs(y = "Updating", x = "habit acquisition performance")
ggplot(gsrt_corr_habit) +
  geom_smooth(method = lm, mapping = aes(y = ant_orienting, x = LH_dif_rt)) +
  geom_point(mapping = aes(y = ant_orienting, x = LH_dif_rt, color = factor(epoch))) +
  labs(y = "Orienting attention", x = "habit acquisition performance")
ggplot(gsrt_corr_habit) +
  geom_smooth(method = lm, mapping = aes(y = ant_alerting, x = LH_dif_rt)) +
  geom_point(mapping = aes(y = ant_alerting, x = LH_dif_rt, color = factor(epoch))) +
  labs(y = "Alertingattention", x = "habit acquisition performance")
ggplot(gsrt_corr_habit) +
  geom_smooth(method = lm, mapping = aes(y = ant_executive, x = LH_dif_rt)) +
  geom_point(mapping = aes(y = ant_executive, x = LH_dif_rt, color = factor(epoch))) +
  labs(y = "Executive attention", x = "habit acquisition performance")
ggplot(gsrt_corr_habit) +
  geom_smooth(method = lm, mapping = aes(y = wcst_pers_error_rate, x = LH_dif_rt)) +
  geom_point(mapping = aes(y = wcst_pers_error_rate, x = LH_dif_rt, color = factor(epoch))) +
  labs(y = "Shifting", x = "habit acquisition performance")
ggplot(gsrt_corr_habit) +
  geom_smooth(method = lm, mapping = aes(y = gng_dprime, x = LH_dif_rt)) +
  geom_point(mapping = aes(y = gng_dprime, x = LH_dif_rt, color = factor(epoch))) +
  labs(y = "Sensitivity", x = "habit acquisition performance")
ggplot(gsrt_corr_habit) +
  geom_smooth(method = lm, mapping = aes(y = gng_false_alarm_rate, x = LH_dif_rt)) +
  geom_point(mapping = aes(y = gng_false_alarm_rate, x = LH_dif_rt, color = factor(epoch))) +
  labs(y = "Inhibition", x = "habit acquisition performance")

ggplot(gsrt_corr_habit) +
  geom_smooth(method = lm, mapping = aes(y = avg_rt, x = LH_dif_rt)) +
  geom_point(mapping = aes(y = avg_rt, x = LH_dif_rt, color = factor(epoch))) +
  labs(y = "accuracy", x = "habit acquisition performance")
ggplot(gsrt_corr_habit) +
  geom_smooth(method = lm, mapping = aes(y = avg_acc, x = LH_dif_rt)) +
  geom_point(mapping = aes(y = avg_acc, x = LH_dif_rt, color = factor(epoch))) +
  labs(y = "reaction time", x = "habit acquisition performance")


# correlation
plot_learn <- cor(gsrt_corr_learning[10:19])

corrplot(plot_learn, method = "ellipse", type = "upper", diag = FALSE)

plot_habit <- cor(gsrt_corr_habit[10:19])

corrplot(plot_habit, method = "ellipse", type = "upper", diag = FALSE)

plot_learn_2 <- cor(gsrt_corr_learning[19:24])

corrplot(plot_learn_2, method = "ellipse", type = "upper", diag = FALSE)

plot_habit_2 <- cor(gsrt_corr_habit[19:24])


shapiro.test(gsrt_anova$digit_span)
shapiro.test(gsrt_anova$counting_span)
shapiro.test(gsrt_anova$running_span)
shapiro.test(gsrt_anova$wcst_pers_error_rate)
shapiro.test(gsrt_anova$ant_orienting)
shapiro.test(gsrt_anova$ant_alerting)
shapiro.test(gsrt_anova$ant_executive)
shapiro.test(gsrt_anova$gng_dprime)
shapiro.test(gsrt_anova$gng_false_alarm_rate)
shapiro.test(gsrt_anova$avg_rt)
shapiro.test(gsrt_anova$avg_acc)


corrplot(plot_habit_2, method = "ellipse", type = "upper", diag = FALSE)

cor.test(gsrt_corr_learning$LH_dif_rt, gsrt_corr_learning$digit_span,
         method = "spearman")
cor.test(gsrt_corr_learning$LH_dif_rt, gsrt_corr_learning$counting_span,
         method = "spearman")
cor.test(gsrt_corr_learning$LH_dif_rt, gsrt_corr_learning$running_span,
         method = "spearman")
cor.test(gsrt_corr_learning$LH_dif_rt, gsrt_corr_learning$ant_orienting,
         method = "spearman")
cor.test(gsrt_corr_learning$LH_dif_rt, gsrt_corr_learning$ant_alerting,
         method = "spearman")
cor.test(gsrt_corr_learning$LH_dif_rt, gsrt_corr_learning$ant_executive,
         method = "spearman")
cor.test(gsrt_corr_learning$LH_dif_rt, gsrt_corr_learning$wcst_pers_error_rate,
         method = "spearman")
cor.test(gsrt_corr_learning$LH_dif_rt, gsrt_corr_learning$gng_dprime,
         method = "spearman")
cor.test(gsrt_corr_learning$LH_dif_rt, gsrt_corr_learning$gng_false_alarm_rate,
         method = "spearman")
cor.test(gsrt_corr_learning$LH_dif_rt, gsrt_corr_learning$avg_rt,
         method = "spearman")
cor.test(gsrt_corr_learning$LH_dif_rt, gsrt_corr_learning$avg_acc,
         method = "spearman")


cor.test(gsrt_corr_habit$LH_dif_rt, gsrt_corr_habit$digit_span,
         method = "spearman")
cor.test(gsrt_corr_habit$LH_dif_rt, gsrt_corr_habit$counting_span,
         method = "spearman")
cor.test(gsrt_corr_habit$LH_dif_rt, gsrt_corr_habit$running_span,
         method = "spearman")
cor.test(gsrt_corr_habit$LH_dif_rt, gsrt_corr_habit$ant_orienting,
         method = "spearman")
cor.test(gsrt_corr_habit$LH_dif_rt, gsrt_corr_habit$ant_alerting,
         method = "spearman")
cor.test(gsrt_corr_habit$LH_dif_rt, gsrt_corr_habit$ant_executive,
         method = "spearman")
cor.test(gsrt_corr_habit$LH_dif_rt, gsrt_corr_habit$wcst_pers_error_rate,
         method = "spearman")
cor.test(gsrt_corr_habit$LH_dif_rt, gsrt_corr_habit$gng_dprime,
         method = "spearman")
cor.test(gsrt_corr_habit$LH_dif_rt, gsrt_corr_habit$gng_false_alarm_rate,
         method = "spearman")
cor.test(gsrt_corr_habit$LH_dif_rt, gsrt_corr_habit$avg_rt,
         method = "spearman")
cor.test(gsrt_corr_habit$LH_dif_rt, gsrt_corr_habit$avg_acc,
         method = "spearman")

# no correlation was found

