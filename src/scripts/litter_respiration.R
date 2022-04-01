# 舒子豪
# 该脚本用于凋落物附着物呼吸数据的处理
######################################
# version 1
# 2022/04/01

library(tidyverse)

litter_respiration_situ_first <- readxl::read_xlsx("data/raw/litter_respiration_situ_first.xlsx")
View(litter_respiration_situ_first)
first_data <- litter_respiration_situ_first[,-c(4,5)] %>% 
  filter(CO2 != "NA")
first_data$CO2 <- as.numeric(first_data$CO2)
first_data$N2O <- as.numeric(first_data$N2O)
first_data$CH4 <- as.numeric(first_data$CH4)
View(first_data)

plot_use_model_func <- function(data,x,y,varname,groupnames){
  data_summary <- function(data,varname,groupnames){
    require(plyr)
    summary_func <- function(x,col){
      c(mean = mean(x[[col]], na.rm = TRUE),
        sd = sd(x[[col]], na.rm = TRUE))
    }
    data_sum <- ddply(data, groupnames, .fun = summary_func,
                      varname)
    data_sum <- rename(data_sum, c("mean" = varname))
    return(data_sum)
  }
  df <- filter(data,Group == x, Type == y)
  model_lm <- lm(df,formula = CO2 ~ Rank)
  l <- list(a = as.numeric(format(coef(model_lm)[1], digits = 4)),
            b = as.numeric(format(coef(model_lm)[2], digits = 4)),
            r2 = format(summary(model_lm)$r.squared, digits = 4),
            p = format(summary(model_lm)$coefficients[2,4], digits = 4))
  eq <- substitute(italic(y) == a %.% italic(x) + b~","~italic(R)^2~"="~r2~","~italic(P)~"="~p, l)
  plot_data1 <- filter(data,Type == y)
  plot_data2 <- data_summary(plot_data1,varname,groupnames) 
  plot_data3 <- filter(plot_data2,Group == x)
  p <- ggplot(plot_data3,aes(x = Rank,y = CO2))+
    geom_point(position = position_dodge(0.1),size = 3)+
    geom_errorbar(aes(ymin = CO2 - sd, ymax = CO2 + sd),
                  width = 0.6, position = position_dodge(0.1),size = 0.6)+
    geom_smooth(method = "lm",formula = y~x)+
    geom_text(aes(x = 15, y = 2500, label = as.character(as.expression(eq))), parse = TRUE,size = 6)+
    theme_classic()+
    theme(
      legend.position = c(0.63,0.25),
      axis.title = element_text(color = "black",size = 20),
      axis.text = element_text(color = "black",size = 16),
      strip.text = element_text(color = 'black', face = 'bold', size = 20)
    )+
    labs(x = "Time(min)",
         y = "CO2(ppm)",
         title = x)
  p
}
CK_stem <- plot_use_model_func(data = first_data,x = "P1",y = "stem",
                               varname = "CO2",groupnames = c("Group","Rank"))
CK_stem





