# 舒子豪
# 该脚本用于凋落物附着物呼吸数据的处理
######################################
# version 1
# 2022/04/01

library(tidyverse)

# 回归计算函数
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
  l <- list(a = as.numeric(format(coef(model_lm)[1], digits = 4)),
            b = as.numeric(format(coef(model_lm)[2], digits = 4)),
            r2 = format(summary(model_lm)$r.squared, digits = 4),
            p = format(summary(model_lm)$coefficients[2,4], digits = 4))
  eq <- substitute(italic(y) == a %.% italic(x) + b~","~italic(R)^2~"="~r2~","~italic(P)~"="~p, l)
  p <- filter(data,Type == typename) %>% 
    data_summary() %>% 
    filter(Group == groupname) %>% 
    ggplot(aes(x = Rank,y = CO2))+
    geom_point(position = position_dodge(0.1),size = 3)+
    geom_errorbar(aes(ymin = CO2 - sd, ymax = CO2 + sd),
                  width = 0.6, position = position_dodge(0.1),size = 0.6)+
    geom_smooth(method = "lm",formula = y~x)+
    geom_text(aes(x = 50, y = 500, label = as.character(as.expression(eq))), parse = TRUE,size = 6)+
    theme_classic()+
    theme(
      legend.position = c(0.63,0.25),
      axis.title = element_text(color = "black",size = 20),
      axis.text = element_text(color = "black",size = 16),
      strip.text = element_text(color = 'black', face = 'bold', size = 20)
    )+
    labs(x = "Time(min)",
         y = "CO2(ppm)",
         title = groupname)
  return(p)
} 

# 2022/3/22第一次取样数据拟合
litter_respiration_situ_first <- readxl::read_xlsx("data/raw/litter_respiration_situ_first.xlsx")
first_data <- litter_respiration_situ_first[-c(16,17,20,31,32,40,116,208,216),] %>% 
  filter(CO2 != "NA")
first_data$CO2 <- as.numeric(first_data$CO2)
first_data$N2O <- as.numeric(first_data$N2O)
first_data$CH4 <- as.numeric(first_data$CH4)
## 茎凋落物附着呼吸拟合
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

## 叶凋落物附着物呼吸拟合
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

## 呼吸速率计算及差异性检验

