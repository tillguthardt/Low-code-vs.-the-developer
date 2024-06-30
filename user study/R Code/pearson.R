install.packages("ggplot2")
install.packages("reshape2")

library(ggplot2)
library(reshape2)

create_correlation_plot <- function(input_csv, output_pdf) {
  pearson_table <- read.csv(input_csv)

  pearson_table[, sapply(pearson_table, is.numeric)] <- sapply(pearson_table[, sapply(pearson_table, is.numeric)], as.numeric)

  numeric_data <- pearson_table[, sapply(pearson_table, is.numeric)]

  cor_matrix <- cor(numeric_data, method = "pearson")
  p_matrix <- matrix(NA, ncol = ncol(numeric_data), nrow = ncol(numeric_data))
  rownames(p_matrix) <- colnames(numeric_data)
  colnames(p_matrix) <- colnames(numeric_data)

  for(i in seq_along(numeric_data)) {
    for(j in seq_along(numeric_data)) {
      p_matrix[i,j] <- cor.test(numeric_data[[i]], numeric_data[[j]], method = "pearson")$p.value
    }
  }

  melted_cor <- melt(cor_matrix)
  melted_p <- melt(p_matrix)

  cor_data <- merge(melted_cor, melted_p, by = c("Var1", "Var2"))
  names(cor_data) <- c("Var1", "Var2", "cor", "p.value")

  plot <- ggplot(data = cor_data, aes(x = Var1, y = Var2, fill = cor)) +
    geom_tile(color = "white") +
    geom_tile(data = subset(cor_data, p.value < 0.05), color = "black", size = 1.5, fill = NA) +  
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name = "Pearson Correlation (r)") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 8), 
      axis.text.y = element_text(size = 8), 
      axis.title = element_blank(),
       legend.title = element_text(size = 8),
      legend.text = element_text(size = 8),
    ) +
    guides(fill = guide_colourbar(
        title.position = "top", 
        barwidth = 1,
        barheight = 5,
      )
    )

  pdf(output_pdf, width=3.5, height=2)
  print(plot)
  dev.off()
}

create_correlation_plot("user study/raw data/pearson.csv", "user study/plots/generated-pearson.pdf")
create_correlation_plot("user study/raw data/pearson-no-outliers.csv", "user study/plots/generated-pearson-no-outliers.pdf")