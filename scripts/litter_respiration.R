# 舒子豪
# 该脚本用于凋落物附着物呼吸数据的处理
######################################
# version 1
# 2022/04/01

# version 2
# 2022/04/07
# 版本更新,使用purrr包进行迭代


library(tidyverse)
library(ggpmisc)


first_data <- readxl::read_xlsx("data/raw/litter_respiration_situ_first.xlsx") %>% 
  slice(-c(16,17,20,31,32,40,45,46,47,48,56,57,60,116,174,175,176,208,216)) %>% 
  filter(CO2 != 0) %>% 
  select(Group,Rank,Type,CO2) %>%
  split(list(.$Group,.$Type))

# 计算mean和sd的公式
# data_summary <- function(data){
  require(plyr)
  summary_func <- function(x,col, na.rm = TRUE){
    c(mean = mean(x[[col]]),
      sd = sd(x[[col]], na.rm = TRUE))
  }
  data_sum <- ddply(data, "Rank", .fun = summary_func, "CO2")                                  
  data_sum <- rename(data_sum, c("mean" = "CO2"))
} 

model_coef <- first_data %>% 
  map(~lm(CO2 ~ Rank,data = .)) %>% 
  map(summary) %>% 
  map_dbl(~.$coefficients[[2]])
write.csv(model_coef,file = "output/analysis/litter_respiration/first_respiration_data.csv",
          row.names = TRUE)

plots <- first_data %>% 
  map(~ggplot(.,aes(x = Rank,y = CO2))+
         geom_point(position = position_dodge(0.1),size = 6)+
         geom_smooth(method = "lm",formula = y ~ x,se = FALSE,color="red",size = 1.5)+
         stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label..,..p.value.label..,sep = "~~~")),
                      formula = y~x,parse=T,size= 12)+
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
                        names(plots),".png") #这里只能写绝对路径吗？
pwalk(list(paths,plots),ggsave,width = 15,height = 12,dpi = 300)

## 凋落物CO2浓度随时间变化的线性回归斜率
## first_respiration_calculation <- readxl::read_xlsx("data/temp/first_respiration_calculation.xlsx")
## first_output <- vector("list",14)
## for (i in 3:16) {
##   first_output[[i]] <- format(coef(lm(formula = first_respiration_calculation[[i]]~first_respiration_calculation[[2]]))[2], 
##                               digits = 4)
## }
## first_output <- dplyr::bind_rows(first_output)
## first_respiration_data <- tibble(
##   Group = c(rep(c("CK","N1","N2","P1","P2","NP1","NP2"),2)),
##   Type = c(rep("stem",7),rep("leaf",7)),
##   CO2_respiration = as.numeric(first_output[[1]])*60   #将CO2气体浓度随时间变化的单位从ppm/min转化为ppm/h
## )
## write.csv(first_respiration_data,file = "output/analysis/litter_respiration/first_respiration_data.csv",
##           row.names = FALSE)

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



