# 舒子豪
# 2022/06/06
# 用于ggplot2绘图主题的调整
# 在使用前请先运行source("theme_szh.r")将该函数调入

theme_szh <- function(...){
    theme_bw(...) +
    theme(
        text = element_text(family = "serif"),
        rect = element_rect(fill = "white"),
        plot.margin = unit(rep(0.5,4), 'lines'),
        # 绘图区域
        panel.background = element_rect(fill = "transparent", color = "transparent"),
        panel.border = element_rect(fill = "transparent", color = 'black',size = 1.6),
        # 坐标轴
        axis.title = element_text(color = 'black', vjust = 0.1),
        axis.ticks.length = unit(-0.3,"lines"), # 刻度
        axis.title.x = element_text(size = 40, color = "black"),
        axis.title.y = element_text(size = 40, color = "black"),
        axis.text.x = element_text(size = 32, color = "black"),
        axis.text.y = element_text(size = 32, color = "black"),
        axis.ticks = element_line(colour = "grey20"),
        # 图例
        legend.title = element_text(size = 40, color = "black"),
        legend.key = element_rect(fill = "transparent", color = "transparent"),
        legend.text = element_text(color = "black",size = 32),
        # 分面
        strip.text = element_text(size = 40),
        strip.background = element_rect(size = 1)
        )
}
