# 导入所需的库
library(tidyr)
library(ggplot2)
library(ggpubr)

#' @title 表达数据箱线图绘制与显著性检验
#' @description 该函数接受表达数据和分组信息，将数据转化为长格式，绘制箱线图并使用Wilcoxon秩和检验进行显著性比较。
#' @param data 数据框，包含表达数据和分组信息。
#' @param group_col 字符串，表示分组列的列名。
#' @return 无返回值。该函数直接绘制并显示箱线图。
#' @usage
#' plot_expression(data, "group")
#' @examples
#' # 创建示例数据框
#' data <- data.frame(
#'   group = rep(c("Control", "Treatment"), each = 10),
#'   gene1 = rnorm(20),
#'   gene2 = rnorm(20),
#'   gene3 = rnorm(20)
#' )
#' # 绘制表达数据箱线图
#' plot_expression(data, "group")
#' @details 该函数首先将宽格式的数据转化为长格式，便于使用`ggplot2`进行绘图。然后，使用`stat_compare_means`函数进行显著性比较，显示p值符号。
#' @keywords plot boxplot significance test
#' @note 请确保输入的数据框包含指定的分组列和基因表达数据列。
#' @import tidyr ggplot2 ggpubr
#' @importFrom
#' tidyr pivot_longer
#' @export
plot_boxplot_significance_test <- function(data, group_col) {
  # 转化数据为长格式
  # 这里将除了分组列之外的所有列转化为长格式，基因名称放在'gene'列，表达值放在'expression'列
  long_data <- tidyr::pivot_longer(data, cols = !.data[[group_col]], names_to = "gene", values_to = "expression")

  # 自定义显著性水平符号
  symnum_args <- list(
    cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, Inf),
    symbols = c("****", "***", "**", "*", "ns")
  )

  # 创建箱线图
  p <- ggplot(long_data, aes(x = gene, y = expression, fill = .data[[group_col]])) +
    geom_boxplot() + # 绘制箱线图
    stat_compare_means(aes(group = .data[[group_col]]),
      method = "wilcox.test", # 使用Wilcoxon秩和检验
      label = "p.signif", # 显示显著性符号
      symnum.args = symnum_args # 使用自定义的显著性符号
    ) +
    # 自定义颜色（如果需要可以取消注释）
    # scale_fill_manual(
    #   values = c("Control" = "blue", "Treatment" = "red"),
    #   limits = c("Control", "Treatment")  # 分组因子水平控制整个顺序，limits可以指定图例顺序
    # ) +
    scale_x_discrete(name = NULL) + # 移除x轴标签
    theme_pubr() # 使用简洁的主题

  p
}
