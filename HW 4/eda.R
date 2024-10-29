pdays_full <- ggplot(bank, aes(x = pdays)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white", alpha = 0.7) +
  labs(x = "pdays", y = "Frequency") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

pdays_filtered <- ggplot(bank[bank$pdays != 999, ], aes(x = pdays)) +
  geom_histogram(bins = 30, fill = "darkorange", color = "white", alpha = 0.7) +
  labs(x = "pdays", y = "") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

pdays <- pdays_full + pdays_filtered

ggsave("pdays.png", pdays, width=12)

y.plot <- ggplot(mapping=aes(x=as.numeric(bank$y)))+
  geom_histogram(bins=6, fill="gray", alpha=0.7)+
  labs(x = "Events of Customers Opening an Account",
       y = "Frequency")+
  theme_minimal(base_size = 14)

ggsave("y-plot.png", y.plot, width=6)
