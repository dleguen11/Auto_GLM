library(ggplot2)

pp_segment <- df %>%
  group_by(territory) %>%
  summarise(
    actual = mean(pure_premium),
    predicted = mean(pred_pure_premium)
  )

ggplot(pp_segment, aes(x = actual, y = predicted, label = territory)) +
  geom_point(size = 4, color = "darkblue") +
  geom_text(vjust = -0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title = "Actual vs Predicted Pure Premium by Territory",
    x = "Actual",
    y = "Predicted"
  )