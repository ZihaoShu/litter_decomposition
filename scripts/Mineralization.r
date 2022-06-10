# 舒子豪
# 2022/06/06

# 该脚本用于凋落物矿化速率数据的分析

library(tidyverse)
Mineralization_data <- readxl::read_xlsx("data/Indoor/Litter_nutrients/Mineralization.xlsx") %>% 
    mutate(CO2_efflux = (m2-m1)*1.69/0.008*24)
head(Mineralization_data)
