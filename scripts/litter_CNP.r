# 舒子豪
# 2022/05/28
# 该脚本用于凋落物碳氮磷含量的分析
source("scripts/func/theme_szh.r")
source("scripts/func/func_anova.r",encoding = "utf-8")
options(dplyr.summarise.inform = FALSE) #解决group_by分组之后summarise报错
library(tidyverse)
library(agricolae) # 调用HSD.test()函数对aov()结果进行整理
library(patchwork)
# 数据导入
litter_CNP <- readxl::read_xlsx("data/Insuit/litter_CNP.xlsx",col_names = TRUE) %>% 
    mutate(Parallel = factor(Parallel),
           Test = factor(Test),
           Group = factor(Group,level = c("CK","N1","N2","P1","P2","NP1","NP2")),
           Type = factor(Type))

# 观察数据的分布情况，剔除异常值
## Carbon
litter_C_plot <- ggplot(litter_CNP,aes(x = Parallel,y = Carbon))+
    geom_boxplot()+
    geom_point(aes(color = Test),size = 3)+
    facet_grid(Type~Group)+
    theme_szh()+
    labs(x = " ",
         y = "Carbon(mg/g)")
ggsave("D:/shuzihao/litter_decomposition/output/Insuit/plots/Carbon_60_raw_data.png",
       litter_C_plot,width = 15,height = 12,dpi = 300)
## Nitrogen
litter_N_plot <- ggplot(litter_CNP,aes(x = Parallel,y = Nitrogen))+
    geom_boxplot()+
    geom_point(aes(color = Test),size = 3)+
    facet_grid(Type~Group)+
    theme_szh()+
    labs(x = " ",
         y = "Nitrogen(mg/g)")
ggsave("D:/shuzihao/litter_decomposition/output/Insuit/plots/Nitrogen_60_raw_data.png",
       litter_N_plot,width = 15,height = 12,dpi = 300)
# 元素含量计算
litter_CNP[c(16,23,26,28,32,33,45,47,51,53,58,62,68,74,77,78,79,87,93,94,95,97,98,101,106,110,161,173,174),8] <- NA
litter_CNP[c(15,20,26,47,68,107,110,152),7] <- NA
litter_CNP <- litter_CNP %>% 
    gather(key = "element",value = "value",Nitrogen,Carbon,Phosphorus) %>%  
    split(list(.$Type,.$element))
output <- vector("list",6)
for(i in 1:6){
    output[[i]] <- func_anova(litter_CNP[[i]],Groupname = "Group",element = "element",value = "value",ignore = TRUE)
}
element_data <- rbind(output[[1]],output[[2]],output[[3]],output[[4]])
write.csv(file = "output/Insuit/element_data.csv",element_data)
