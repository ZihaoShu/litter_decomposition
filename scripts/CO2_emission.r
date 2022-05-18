# 舒子豪
# 该脚本用于室内分解实验的CO2释放速率数据的分析
# 2022/5/17

library(tidyverse)
library(ggpmisc)
CO2_emission_exp1 <- readxl::read_xlsx("data/raw/CO2_emission_exp1.xlsx",col_names = TRUE)

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
write.csv(decompostion_time_1_coef,file = "data/temp/CO2_emission_exp1/decomposition_time_1_coef.csv",
          row.names = TRUE)

plots <- decomposition_time_1 %>% 
  map(~ggplot(.,aes(x = Time_lag,y = CO2))+
         geom_point(position = position_dodge(0.1),size = 6)+
         geom_smooth(method = "lm",formula = y ~ x,se = FALSE,color="red",size = 1.5)+
         stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label..,..p.value.label..,sep = "~~~")), 
                      formula = y~x,parse=T,size= 12)+       #显示公式、R方以及P值
         theme_bw()+
         theme(
           axis.title = element_text(color = "black",size = 40),
           axis.text = element_text(color = "black",size = 32)
         )+
         labs(x = "Time(min)",
              y = "CO2(ppm)")) 
paths <- stringr::str_c("D:/shuzihao/litter_decomposition/output/plots/CO2_emission_exp1/decomposition_time_1/1_",
                        names(plots),".png")
pwalk(list(paths,plots),ggsave,width = 15,height = 12,dpi = 300)
