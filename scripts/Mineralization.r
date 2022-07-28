# 舒子豪
# 2022/06/06
# 该脚本用于凋落物矿化速率数据的分析
source("scripts/func/func_anova.r",encoding = "utf-8")
library(tidyverse)
Mineralization <- readxl::read_xlsx("data/Indoor/Mineralization.xlsx") %>% 
    mutate(CO2_efflux = (m2-m1)*1.69/0.008*exp_time) %>% 
    mutate(Parallel = factor(Parallel),
        exp = factor(exp),
        Group = factor(Group,level = c("CK","N1","N2","P1","P2","NP1","NP2")),
        Type = factor(Type)) %>% 
    filter(CO2_efflux > 0) %>% 
    split(list(.$Type,.$Time,.$exp))

# 删除split分组带来的空数据框
Mineralization_2 <- vector("list",length(Mineralization))
for(i in 1:length(Mineralization)){
    Mineralization_2[[i]] <- if(length(Mineralization[[i]][[10]]) != 0){
        Mineralization[[i]]
    }else {
       0
    }
}#根据CO2_efflux列长度进行判定，若长度为0则为空数据框
Mineralization_2 <- Mineralization_2[lapply(Mineralization_2, length) != 1] #删除Mineralization列表中赋值为0的数据框

# 计算均值及显著性
output <- vector("list",length(Mineralization_2))
for(i in 1:length(Mineralization_2)){
    output[[i]] <- func_anova(Mineralization_2[[i]],Groupname = "Group",element = "exp",value = "CO2_efflux",ignore = TRUE)
}
output
CO2_efflux <- Reduce(rbind,output)
write.csv(file = "output/Indoor/CO2_efflux.csv",CO2_efflux)

