# 舒子豪
# 2022/06/09
# 用于显著性检验并绘制柱状图
# 在使用前请先运行source("func_anova.r",encoding = "utf-8")将该函数调入
# 首先使用shapiro.test()函数对数据进行正态性检验，然后使用bartlett.test()函数检验数据方差齐性，
# 最后数据在满足正态性及方差齐性的条件下进行显著性检验，并绘制柱状图

func_anova <- function(data,element){
    source("scripts/func/theme_szh.r")
    require(agricolae)
    require(tidyverse)
    names(data)[names(data) == element] = "value"
    shapiro = data %>% 
        group_by(Group) %>% 
        summarise(p_value = shapiro.test(value)[[2]]) %>%  #检验数据是否符合正态分布
        mutate(score = (p_value-0.05)/abs(p_value-0.05))
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
        df$Group = factor(df$Group,level = c("CK","N1","N2","P1","P2","NP1","NP2"))
        name = names(df[2])
        p = ggplot(df,aes(x = Group,y = value))+
        geom_col(fill = "gray")+
        geom_errorbar(aes(ymin = value - std,ymax = value + std),position=position_dodge(.9),width = 0.2)+
        geom_text(aes(label = groups, y = value + std),size = 6,vjust = -0.5) +
        theme_szh()+
        labs(x = " ",
             y = paste0(name,"(mg/g)"))
        p
    }
}
}
