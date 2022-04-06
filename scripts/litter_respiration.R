# 舒子豪
# 该脚本用于凋落物附着物呼吸数据的处理
######################################
# version 1
# 2022/04/01

library(tidyverse)

# 凋落物附着物回归拟合函数
plot_use_model_func <- function(data,groupname,typename){
  data_summary <- function(data){
    require(plyr)
    summary_func <- function(x,col){
      c(mean = mean(x[[col]], na.rm = TRUE),
        sd = sd(x[[col]], na.rm = TRUE))
    }
    data_sum <- ddply(data, c("Group","Rank"), .fun = summary_func, # 可修改参数，将c("Group","Rank")改为groupnames
                      "CO2")                                        # 将"CO2"改为varname，在参数中添加这两项
    data_sum <- rename(data_sum, c("mean" = "CO2"))
  }
  model_lm <- filter(data,Group == groupname, Type == typename) %>% 
    lm(formula = CO2 ~ Rank)
  l <- list(a = as.numeric(format(coef(model_lm)[2], digits = 4)),
            b = as.numeric(format(coef(model_lm)[1], digits = 4)),
            r2 = format(summary(model_lm)$r.squared, digits = 4),
            p = format(summary(model_lm)$coefficients[2,4], digits = 4))
  eq <- substitute(italic(y) == a %.% italic(x) + b~","~italic(R)^2~"="~r2~","~italic(P)~"="~p, l)
  p <- filter(data,Type == typename) %>% 
    data_summary() %>% 
    filter(Group == groupname) %>% 
    ggplot(aes(x = Rank,y = CO2))+
    geom_point(position = position_dodge(0.1),size = 6)+
    geom_errorbar(aes(ymin = CO2 - sd, ymax = CO2 + sd),
                  width = 1.2, position = position_dodge(0.1),size = 1.2)+
    geom_smooth(method = "lm",formula = y~x)+
    geom_text(aes(x = 40, y = 400, label = as.character(as.expression(eq))), parse = TRUE,size = 10)+
    theme_classic()+
    theme(
      axis.title = element_text(color = "black",size = 40),
      axis.text = element_text(color = "black",size = 32)
    )+
    labs(x = "Time(min)",
         y = "CO2(ppm)",
         title = groupname)
  return(p)
} 

# 2022/3/22第一次取样数据拟合
litter_respiration_situ_first <- readxl::read_xlsx("data/raw/litter_respiration_situ_first.xlsx")
first_data <- litter_respiration_situ_first[-c(16,17,20,31,32,40,45,46,47,48,56,57,60, # 剔除异常值
                                               116,174,175,176,208,216),] %>% 
  filter(CO2 != "NA")
first_data$CO2 <- as.numeric(first_data$CO2)
first_data$N2O <- as.numeric(first_data$N2O)
first_data$CH4 <- as.numeric(first_data$CH4)
## 茎凋落物CO2浓度随时间变化的线性回归斜率
ggsave("output/plots/litter_respiration/first_CK_stem.png",
       plot_use_model_func(data = first_data,groupname = "CK",typename = "stem"),
       width = 15,height = 12,dpi = 300)
ggsave("output/plots/litter_respiration/first_N1_stem.png",
       plot_use_model_func(data = first_data,groupname = "N1",typename = "stem"),
       width = 15,height = 12,dpi = 300)
ggsave("output/plots/litter_respiration/first_N2_stem.png",
       plot_use_model_func(data = first_data,groupname = "N2",typename = "stem"),
       width = 15,height = 12,dpi = 300)
ggsave("output/plots/litter_respiration/first_P1_stem.png",
       plot_use_model_func(data = first_data,groupname = "P1",typename = "stem"),
       width = 15,height = 12,dpi = 300)
ggsave("output/plots/litter_respiration/first_P2_stem.png",
       plot_use_model_func(data = first_data,groupname = "P2",typename = "stem"),
       width = 15,height = 12,dpi = 300)
ggsave("output/plots/litter_respiration/first_NP1_stem.png",
       plot_use_model_func(data = first_data,groupname = "NP1",typename = "stem"),
       width = 15,height = 12,dpi = 300)
ggsave("output/plots/litter_respiration/first_NP2_stem.png",
       plot_use_model_func(data = first_data,groupname = "NP2",typename = "stem"),
       width = 15,height = 12,dpi = 300)

## 叶凋落物CO2浓度随时间变化的线性回归斜率
ggsave("output/plots/litter_respiration/first_CK_leaf.png",
       plot_use_model_func(data = first_data,groupname = "CK",typename = "leaf"),
       width = 15,height = 12,dpi = 300)
ggsave("output/plots/litter_respiration/first_N1_leaf.png",
       plot_use_model_func(data = first_data,groupname = "N1",typename = "leaf"),
       width = 15,height = 12,dpi = 300)
ggsave("output/plots/litter_respiration/first_N2_leaf.png",
       plot_use_model_func(data = first_data,groupname = "N2",typename = "leaf"),
       width = 15,height = 12,dpi = 300)
ggsave("output/plots/litter_respiration/first_P1_leaf.png",
       plot_use_model_func(data = first_data,groupname = "P1",typename = "leaf"),
       width = 15,height = 12,dpi = 300)
ggsave("output/plots/litter_respiration/first_P2_leaf.png",
       plot_use_model_func(data = first_data,groupname = "P2",typename = "leaf"),
       width = 15,height = 12,dpi = 300)
ggsave("output/plots/litter_respiration/first_NP1_leaf.png",
       plot_use_model_func(data = first_data,groupname = "NP1",typename = "leaf"),
       width = 15,height = 12,dpi = 300)
ggsave("output/plots/litter_respiration/first_NP2_leaf.png",
       plot_use_model_func(data = first_data,groupname = "NP2",typename = "leaf"),
       width = 15,height = 12,dpi = 300)

## 凋落物CO2浓度随时间变化的线性回归斜率
first_respiration_calculation <- readxl::read_xlsx("data/temp/first_respiration_calculation.xlsx")
first_output <- vector("list",14)
for (i in 3:16) {
  first_output[[i]] <- format(coef(lm(formula = first_respiration_calculation[[i]]~first_respiration_calculation[[2]]))[2], 
                              digits = 4)
}
first_output <- dplyr::bind_rows(first_output)
first_respiration_data <- tibble(
  Group = c(rep(c("CK","N1","N2","P1","P2","NP1","NP2"),2)),
  Type = c(rep("stem",7),rep("leaf",7)),
  CO2_respiration = as.numeric(first_output[[1]])*60   #将CO2气体浓度随时间变化的单位从ppm/min转化为ppm/h
)
write.csv(first_respiration_data,file = "output/analysis/litter_respiration/first_respiration_data.csv",
          row.names = FALSE)

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



