library(tidyverse)
library(rstatix)

# Session3

NCDB <- readxl::read_xlsx("data/Session 3 Materials-20210409/NCDB.xlsx") %>% 
  mutate(Grade = as.factor(Grade)) %>%
  mutate(Tumor_size = ifelse(Tumor_size>500, NA_real_, Tumor_size), log_tumor = log10(Tumor_size)) %>% 
  filter(!is.na(log_tumor) | !is.na(Grade))

colnames(NCDB)

NCDB %>% 
  ggplot(aes(x = Grade)) + geom_bar() 
NCDB %>% 
  ggplot(aes(x = Tumor_size)) + geom_bar() 
NCDB %>%
  group_by(Grade) %>%
  get_summary_stats(Tumor_size, type = "mean_sd")

# Check normaity
# Build the linear model
model  <- lm(log_tumor ~ Grade, data = NCDB)
# Create a QQ plot of residuals
ggqqplot(residuals(model))
# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))

NCDB %>%
  group_by(Grade) %>%
  shapiro_test(log_tumor)
plot(model, 1)
res.aov <- NCDB %>% anova_test(log_tumor ~ Grade)
res.aov

my_comparisons <- list( c("1", "2"), c("1", "3"), c("3", "2") )
NCDB %>% filter(!is.na(Grade)) %>% 
  ggplot(aes(x = Grade, y = log_tumor)) + geom_boxplot() +
stat_compare_means(comparisons = my_comparisons, label.y = c(3, 3.5, 4))+
  stat_compare_means(method = "anova", label.y = 5)     # Add global Anova p-value
library(multcompView)
ANOVA <- aov(model)
summary(ANOVA)

# Tukey test to study each pair of treatment :
TUKEY <- TukeyHSD(x=ANOVA, 'Grade', conf.level=0.95, )
plot(TUKEY , las=1 , col="brown")
TUKEY

# Welch?
NCDB %>% cohens_d(log_tumor ~ Grade, var.equal = FALSE)
stat.test <- NCDB %>%
  t_test(log_tumor ~ Grade) %>%
  add_significance()
stat.test <- stat.test %>% add_xy_position(x = "Grade")
bxp <- ggboxplot(
  NCDB, x = "Grade", y = "log_tumor", 
  ylab = "log_tumor", xlab = "Groups", add = "jitter"
)
bxp + 
  stat_pvalue_manual(stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(stat.test, detailed = TRUE))
library(onewaytests)
t.test(NCDB$log_tumor, NCDB$Grade, alternative = c("two.sided", "less", "greater")) # Warning both vector need to be num

# Van der Waerden
ns.test(NCDB$log_tumor, NCDB$Grade, alternative = c("two.sided", "less", "greater"), 
        paired = FALSE, compared = FALSE, alpha = 0.05)
waerden.test(y, trt, alpha=0.05, group=TRUE, main=NULL,console=FALSE)





# Session4

Categorical <- readxl::read_xlsx("data/Session 4 Materials -20210409/Bios 102 - Data for Categorical Analysis.xlsx", sheet = "data1") %>% 
  mutate(ETHNICITY = factor(ETHNICITY, levels = c("nh", "h"))) %>% 
  mutate(RACE = ifelse(RACE == "White", 1, 0))

# tbl1 <- Categorical %>% select(BIRADS, ETHNICITY, Response) %>% 
#   tbl_uvregression(method = survival::coxph, 
#                    y = (Surv(time = Categorical$month_at_os, 
#                              event = Categorical$os_event)),
#                    exponentiate = TRUE) %>% bold_p(t = .05) %>% add_nevent() %>% 
#   bold_labels() %>% italicize_levels()
# tbl2 <- coxph(Surv(time = Categorical$month_at_os, 
#                    event = Categorical$os_event) ~ BIRADS + ETHNICITY, data =  Categorical) %>%
#   tbl_regression(exponentiate = TRUE) %>% bold_p(t = .05)
# tbl <- tbl_merge(list(tbl1, tbl2), tab_spanner = c("**Univariate**", "**Multivariate**"))
library(gtsummary)
model <- glm(Response ~ ETHNICITY, data = Categorical, family = binomial)
tbl1 <- tbl_regression(model) %>% bold_p(t = .05)
tbl2 <- tbl_regression(model, exponentiate = TRUE) %>% bold_p(t = .05)
tbl_merge(list(tbl1, tbl2), tab_spanner = c("**Estimate**", "**Exp**"))


