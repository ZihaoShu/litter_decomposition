# 舒子豪
# 该脚本用于凋落物附着物呼吸数据的处理
######################################
# version 1
# 2022/04/01

# version 2
# 2022/04/07
# 版本更新,使用purrr包进行迭代


library(tidyverse)
library(ggpmisc) #使用stat_poly_eq函数显示公式和R方

# 读取数据室外凋落物附着物CO2排放通量数据
# 剔除数据中的异常值，以及NA值，对数据框按照Group和Type分成列表
litter_respiration <- readxl::read_xlsx("data/raw/litter_respiration.xlsx") %>% 
  slice(-c(16,17,20,31,32,40,45,46,47,48,56,57,60,116,174,175,176,208,216)) %>% 
  filter(CO2 != 0) %>% 
  select(Group,Rank,Type,CO2) %>%
  split(list(.$Group,.$Type))

# 对数据分别进行线性拟合，提取CO2释放速率，并输出数据
model_coef <- litter_respiration %>% 
  map(~lm(CO2 ~ Rank,data = .)) %>% 
  map(summary) %>% 
  map_dbl(~.$coefficients[[2]])
write.csv(model_coef,file = "output/analysis/litter_respiration/first_respiration_data.csv",
          row.names = TRUE)

# 对列表中每个数据框进行绘图并拟合函数，显示公式、R方以及P值
# 输出图片
plots <- first_data %>% 
  map(~ggplot(.,aes(x = Rank,y = CO2))+
         geom_point(position = position_dodge(0.1),size = 6)+
         geom_smooth(method = "lm",formula = y ~ x,se = FALSE,color="red",size = 1.5)+
         stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label..,..p.value.label..,sep = "~~~")), 
                      formula = y~x,parse=T,size= 12)+       #显示公式、R方以及P值
         theme_bw()+
         theme(
           axis.title = element_text(color = "black",size = 40),
           axis.text = element_text(color = "black",size = 32),
           title = element_text(color = "black",size = 40),
           plot.title = element_text(hjust = 0.9)
         )+
         labs(x = "Time(min)",
              y = "CO2(ppm)")) 
paths <- stringr::str_c("D:/shuzihao/litter_decomposition/output/plots/litter_respiration/first_",
                        names(plots),".png")  #这里只能写绝对路径吗？
pwalk(list(paths,plots),ggsave,width = 15,height = 12,dpi = 300)


## 凋落物CO2释放速率的计算
CO2_evolution_func <- function(respiration,mass,temperature){
  k <- respiration                    #CO2浓度随时间变化的线性回归斜率 ppm CO2/h
  M <- 44                             #CO2的摩尔质量44g/mol
  m <- mass                           #放入的凋落物质量(干重),g
  T_0 <- 273.15                       #标准状态下的温度,K
  T_1 <- T_0+temperature              #采样时的温度,K。输入的temperature单位为℃，这里加上T_O换算成K
  V <- 0.5                            #气室的体积,L
  V_0 <- 22.4                         #标准状况下CO2的摩尔体积,22.4L/mol
  P <- 1.013                          #采样时的气压,kPa
  P_0 <- 1.013                        #标准大气压1.013kPa
  y <- (k*M*V*P*T_0)/(m*V_0*P_0*T_1)  #凋落物CO2释放速率的计算公式，μgCO2/(g·h)
  y
}



