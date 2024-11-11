show_correlation <- function(){
  png("credit-cor.png", width=2000, height=250, res=300)
  corrplot::corrplot(cor(credit)[29:30,],
                     tl.col = "black",
                     cl.ratio = 0.2,
                     cl.cex = 0.5,
                     cl.length = 3,
                     tl.cex = 0.75,
                     method="circle")
  dev.off()
}

logit.data <- read_csv("p-estimates.csv")

visualize_amount <- function(){
  ggplot(logit.data, mapping=aes(x = Amount, y = Estimate)) +
    geom_line() +
    geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "#88DDAA", alpha = 0.2) +
    labs(
      x = "Amount (Adjusted for Controls)",
      y = "Predicted Probability of Fraud"
    )+
    theme_minimal()
}

visualize_margins <- function(){
  margins = logit.data$Estimate*(1-logit.data$Estimate)*coef(logit.model)[2]
  ggplot(logit.data, aes(x = Amount, y = margins)) +
    geom_line(color = "black", size = 1) +
    theme_minimal() +
    labs(
      x = "Amount (Adjusted for Controls)",
      y = "Marginal Effect",
    )
}