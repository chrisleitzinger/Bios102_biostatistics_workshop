library(tidyverse)

# Session1

nursing_home <- readxl::read_xls("data/Session 1 Materials-20210402/nursing_home.xls")

summary(nursing_home$BED)

# fexp_for_bed_less_then_median <- 
nursing_home %>% 
  filter(BED < median(BED)) %>% summarize(x = mean(FEXP, na.rm = TRUE)) 

nursing_home %>% 
    group_by(RURAL) %>% summarize(x = sd(NSAL, na.rm = TRUE)) 

nursing_home %>% summarize(x = median(TDAYS, na.rm = TRUE)) 


# Session2

BrainVolume <- readxl::read_xlsx("data/Session 2 Materials-20210402/BrainVolume.xlsx")

BrainVolume %>% pivot_longer(cols = c(affected, unaffected), names_to = "test", values_to = "size") %>% 
  ggplot(aes(x= test, y = size)) + geom_boxplot() + stat_compare_means()


Bladder <- readxl::read_xlsx("data/Session 2 Materials-20210402/Bladder2.xlsx")

Bladder %>% pivot_longer(cols = c(EGFR, KRT20), names_to = "gene", values_to = "expression") %>% 
  ggplot(aes(x= gene, y = expression)) + geom_boxplot() + stat_compare_means()

Bladder %>% 
  ggplot(aes(x= gender, y = KRT20)) + geom_boxplot() + stat_compare_means()


Bladder %>% 
  ggplot(aes(x= SubtypeII, y = TP63)) + geom_boxplot() + stat_compare_means()



