# 舒子豪
# 该脚本用于室内分解实验的CO2释放速率数据的分析
# 2022/5/17

library(tidyverse)
library(ggpmisc)
source("scripts/func/theme_szh.r")

# 数据导入
CO2_emission_exp1 <- readxl::read_xlsx("data/Indoor/Litter_nutrients/CO2_emission_exp1.xlsx",col_names = TRUE)

# CO2释放速率的计算
decomposition_time_1 <- CO2_emission_exp1 %>% 
    filter(Decomposition_time == 1) %>% 
    split(list(.$Group,.$Type,.$Parallel))

decompostion_time_1_coef <- decomposition_time_1 %>% 
    map(~lm(CO2 ~ Time_lag,data = .)) %>% 
    map(summary) %>% 
    map_dbl(~.$coefficients[[2]])
name <- names(decompostion_time_1_coef) %>% 
    str_split_fixed("\\.",n = 3) #用正则表达式匹配"."将字符分隔开
decompostion_time_1_coef <- rbind(decompostion_time_1_coef,name[,1],name[,2],name[,3])
rownames(decompostion_time_1_coef) <- c("Coef","Group","Type","Parallel")
decompostion_time_1_coef <- t(decompostion_time_1_coef)
write.csv(decompostion_time_1_coef,file = "output/Indoor/Litter_nutrients/CO2_coef_day1.csv",
          row.names = TRUE)

plots <- decomposition_time_1 %>% 
  map(~ggplot(.,aes(x = Time_lag,y = CO2))+
         geom_point(position = position_dodge(0.1),size = 6)+
         geom_smooth(method = "lm",formula = y ~ x,se = FALSE,color="red",size = 1.5)+
         stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label..,..p.value.label..,sep = "~~~")), 
                      formula = y~x,parse=T,size= 12)+       #显示公式、R方以及P值
         theme_szh()+
         labs(x = "Time(min)",
              y = expression(CO[2](ppm))))
paths <- stringr::str_c("D:/shuzihao/litter_decomposition/output/Indoor/Litter_nutrients/plots/decomposition_time_1/1_",
                        names(plots),".png")
pwalk(list(paths,plots),ggsave,width = 15,height = 12,dpi = 300)

# 同室外计算方法
head(CO2_emission_exp1)
df <- CO2_emission_exp1 %>%
  mutate(Parallel = factor(Parallel)) %>% 
  split(list(.$Group,.$Type))
df
model_coef <- df %>% 
  map(~lm(CO2 ~ Rank,data = .)) %>% 
  map(summary) %>% 
  map_dbl(~.$coefficients[[2]])
name <- names(model_coef) %>% 
    str_split_fixed("\\.",n = 3) #用正则表达式匹配"."将字符分隔开
model_coef <- rbind(model_coef,name[,1],name[,2])
rownames(model_coef) <- c("Coef","Group","Type")
model_coef <- t(model_coef)
model_coef

plots <- df %>% 
  map(~ggplot(.,aes(x = Rank,y = CO2))+
         geom_point(aes(color = Parallel),position = position_dodge(0.1),size = 6)+
         geom_smooth(method = "lm",formula = y ~ x,se = FALSE,color="red",size = 1.5)+
         stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label..,..p.value.label..,sep = "~~~")), 
                      formula = y~x,parse=T,size= 12)+       #显示公式、R方以及P值
         theme_szh()+
         labs(x = "Time(min)",
              y = expression(CO[2](ppm))))
paths <- stringr::str_c("D:/shuzihao/litter_decomposition/output/Indoor/Litter_nutrients/plots/test/",
                        names(plots),".png")  #这里只能写绝对路径吗？
pwalk(list(paths,plots),ggsave,width = 15,height = 12,dpi = 300)
