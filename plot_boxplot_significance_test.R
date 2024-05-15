# Load necessary libraries
library(reshape2)
library(ggplot2)

# Generate sample data
set.seed(123)
dat_x <- data.frame(
  group = rep(c("A", "B"), each = 50),
  value = c(rnorm(50, mean = 5, sd = 1), rnorm(50, mean = 6, sd = 1))
)

# Reshape the data using melt function
melt_res <- melt(dat_x, id.vars = "group")

# Define the boxplot_brca function
boxplot_brca <- function(melt_res) {
  comp <- wilcox.test(value ~ group, data = melt_res)
  stars <- rev(c("ns", "*", "**", "***"))[findInterval(comp$p.value, sort(c(1, 0.05, 0.01, 0.001))) + 1]
  ggplot(melt_res, aes(group, value)) +
    geom_violin(aes(fill = group)) +
    geom_boxplot(aes(fill = group), width = 0.1) +
    scale_fill_manual(values = c("#FDAF91FF", "#0099B4FF")) +
    annotate("text", x = 1.5, y = max(melt_res$value) + 0.05, label = stars, size = 6) +
    ylab("") +
    xlab("") +
    theme(
      axis.title = element_text(size = 12, color = "black"),
      axis.text = element_text(size = 12, color = "black"),
      legend.position = "none",
      panel.grid = element_blank(),
      panel.background = element_rect(colour = "black", fill = NA)
    )
}

# Plot the results
boxplot_brca(melt_res)
