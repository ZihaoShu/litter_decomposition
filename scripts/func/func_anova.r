# 舒子豪
# 2022/06/09
# 在使用前请先运行source("func_anova.r",encoding = "utf-8")将该函数调入
# 使用shapiro.test()及bartlett.test()函数对数据的正态性及方差齐性进行检验
# 满足正态性及方差齐性之后使用agricolae包中的HSD.test()函数对ano()计算结果进行汇总统计，
# 默认是进行正态性及方差检验之后再进行显著性检验，考虑到样本量的问题加入ignore = TRUE参数,可直接进行显著性检验
# 参数意义：data——导入数据,Groupname——分组变量,element——进行显著性分析的元素名称,value——进行显著性分析的观测数据

func_anova <- function(data,Groupname,element,value,ignore = FALSE){
    source("scripts/func/theme_szh.r")
    require(agricolae)
    require(tidyverse)
    names(data)[names(data) == Groupname] = "Group"
    names(data)[names(data) == value] = "value"
    if(ignore == FALSE){
    shapiro = data %>% 
        group_by(Group) %>% 
        summarise(p_value = shapiro.test(value)[[2]]) %>%  #检验数据是否符合正态分布
        mutate(score = (p_value-0.05)/abs(p_value-0.05)) #若得分为-1或NaN则为该组数据不符合
    if(sum(shapiro$score) != length(shapiro[[1]])){
        print("数据不符合正态性")
        shapiro
    }else{
    p_norm = bartlett.test(value ~ Group,data)[[3]] #检验方差齐性
    if(p_norm < 0.05){
        print("数据不满足方差齐性")
        p_norm
    }else{
        HSD = HSD.test(aov(value ~ Group,data),"Group")     #进行显著性检验，并添上字母标记
        HSD$means$Group = rownames(HSD$means)
        HSD$groups$Group = rownames(HSD$groups)
        df = merge(HSD$means,HSD$groups[,-1],by = "Group")
        name = names(df[2])
        df = df[,-c(7,8,9)]
        Type = c(rep(data[1,]$Type,7))
        df$Type = Type
        Time = c(rep(data[1,]$Time,7))
        df$Time  = Time
        Element = c(rep(data[1,]$element,7))
        df$Element  = Element
        df
        }
    }
    }else{
        HSD = HSD.test(aov(value ~ Group,data),"Group")
        HSD$means$Group = rownames(HSD$means)
        HSD$groups$Group = rownames(HSD$groups)
        df = merge(HSD$means,HSD$groups[,-1],by = "Group")
        name = names(df[2])
        df = df[,-c(7,8,9)]
        Type = c(rep(data[1,]$Type,7))
        df$Type = Type
        Time = c(rep(data[1,]$Time,7))
        df$Time  = Time
        Element = c(rep(data[1,]$element,7))
        df$Element  = Element
        df
    }
}
