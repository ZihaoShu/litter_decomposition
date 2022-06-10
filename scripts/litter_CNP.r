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

litter_carbon <- litter_CNP %>% 
    slice(-c(16,23,26,28,45,47,62,68,74,77,78,79,87,93,95,97,98,101,106,110,161)) %>% 
    split(list(.$Type))
func_anova(litter_carbon[[1]],element = "Carbon")
func_anova(litter_carbon[[2]],element = "Carbon")


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
litter_nitrogen <- litter_CNP %>%
    split(list(.$Type))

func_anova(litter_nitrogen[[1]],element = "Nitrogen")
func_anova(litter_nitrogen[[2]],element = "Nitrogen")

# 元素含量计算
element_content <- litter_CNP %>% 
    gather(value = "Value",key = "Element",Nitrogen,Carbon,Phosphorus) %>% 
    group_by(Group,Type,Element,Decomposition_time) %>% 
    summarise(Mean = mean(Value,na.rm = TRUE),
              Sd = sd(Value,na.rm = TRUE)) %>% 
    mutate(Element = factor(Element))
write.csv(element_content,"output/Insuit/element_content.csv",row.names=FALSE)
