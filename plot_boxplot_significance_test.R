library(ggplot2)
library(ggpubr)
library(tidyr) # For pivot_longer

# 示例数据
set.seed(123)
data <- data.frame(
  group = rep(c("A", "B", "C"), each = 20),
  geneA = c(rnorm(20, mean = 5), rnorm(20, mean = 6), rnorm(20, mean = 7)),
  geneB = c(rnorm(20, mean = 5.5), rnorm(20, mean = 6.5), rnorm(20, mean = 7.5))
)

# 将数据从宽格式转换为长格式
data_long <- pivot_longer(data, cols = -group, names_to = "gene", values_to = "expression")

# 绘制箱线图
p <- ggplot(data_long, aes(x = group, y = expression, fill = group)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.4) +
  facet_wrap(~gene, scales = "fixed") + # 使用facet_wrap按基因分面,对齐y轴
  # stat_compare_means(aes(group = group), method = "anova", label.y = 8) + # 添加ANOVA检验
  stat_compare_means(aes(group = group),
    comparisons = list(c("A", "B")),
    method = "t.test", label = "p.signif"
  ) +
  labs(
    title = "Group Comparison for Each Gene",
    x = "Group",
    y = "Expression Level"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid = element_blank(),
    panel.background = element_rect(colour = "black", fill = NA),
  )

# 显示图形
print(p)
