# 舒子豪
# 2022/06/06
# 该脚本用于凋落物矿化速率数据的分析
source("scripts/func/func_anova.r",encoding = "utf-8")
library(tidyverse)
Mineralization_exp1 <- readxl::read_xlsx("data/Indoor/Litter_nutrients/Mineralization.xlsx") %>% 
    mutate(CO2_efflux = (m2-m1)*1.69/0.008*exp_time) %>% 
    filter(CO2_efflux > 0) %>% 
    split(list(.$Type,.$Time))
CO2_efflux_exp1 <- rbind(func_anova(Mineralization_exp1[[1]],element = "CO2_efflux"),func_anova(Mineralization_exp1[[2]],element = "CO2_efflux"),
                         func_anova(Mineralization_exp1[[3]],element = "CO2_efflux",ignore = TRUE),func_anova(Mineralization_exp1[[4]],element = "CO2_efflux",ignore = TRUE))
CO2_efflux_exp1

Mineralization_exp2 <- readxl::read_xlsx("data/Indoor/Litter_qulity/Mineralization.xlsx") %>% 
    mutate(CO2_efflux = (m2-m1)*1.69/0.008*exp_time) %>% 
    filter(CO2_efflux > 0) %>% 
    split(list(.$Type,.$Time))
CO2_efflux_exp2 <- rbind(func_anova(Mineralization_exp2[[1]],element = "CO2_efflux"),func_anova(Mineralization_exp2[[2]],element = "CO2_efflux"))
CO2_efflux_exp2
