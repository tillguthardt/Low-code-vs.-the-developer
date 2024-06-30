install.packages("ggplot2")
install.packages("reshape2")
install.packages("car")

analyse_und_plot <- function(data, pdf_name) {
  data$Group <- factor(data$Group, levels = c(1, 2), labels = c("Without Experience", "With Experience"))
  
  pdf(pdf_name, width=8, height=5)

  summary1 <- summary(data$Duration[data$Group == "Without Experience"])
  summary2 <- summary(data$Duration[data$Group == "With Experience"])
  print("Summary Statistics for Group Without Experience:")
  print(summary1)
  print("Summary Statistics for Group With Experience:")
  print(summary2)

  # normal distribution tests for 'Duration' based on 'Group'
  shapiro1 <- shapiro.test(data$Duration[data$Group == "Without Experience"])
  shapiro2 <- shapiro.test(data$Duration[data$Group == "With Experience"])
  print("Shapiro-Wilk Normality Test for Group Without Experience:")
  print(shapiro1)
  print("Shapiro-Wilk Normality Test for Group With Experience:")
  print(shapiro2)

  # test for homogeneity of variance
  library(car)
  levene <- leveneTest(Duration ~ Group, data = data)
  print("Levene's Test for Homogeneity of Variance:")
  print(levene)

  # t-tests for independent samples
  
  t_test_equal <- t.test(Duration ~ Group, data=data, var.equal=TRUE)   # for variance homogeneity

  t_test_unequal <- t.test(Duration ~ Group, data=data, var.equal=FALSE) # Welch test for variance inhomogeneity

  print("T-Test with Equal Variances:")
  print(t_test_equal)
  print("Welch T-Test with Unequal Variances:")
  print(t_test_unequal)

  par(mar = c(5, 4, 2, 2) + 0.1)

  boxplot(Duration ~ Group, data=data, xlab="Group", ylab="Duration (min)",range=0,)

  dev.off()
}

data <- read.csv("user study/raw data/groups.csv")
data_oa <- read.csv("user study/raw data/groups-no-outliers.csv")

analyse_und_plot(data, "user study/plots/groups.pdf")
analyse_und_plot(data_oa, "user study/plots/groups-no-outliers.pdf")