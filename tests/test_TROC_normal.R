###
mu <- c(0.8, 1, 2.5, 2.5)
sigma <- c(1, 1, 1, 1)

TROC_N_est <- TROC_normal(mu = mu, sigma = sigma)
MROC_N_est <- MROC_normal(mu = mu, sigma = sigma)
dt_TROC_N_est <- data.frame(MSp = MROC_N_est$MSp, MSe = MROC_N_est$MSe,
                            TSe = TROC_N_est$TSe)

require(ggplot2)
ggplot(dt_TROC_N_est, aes(x = 1 - MSp, y = TSe)) +
  geom_line() +
  geom_line(aes(y = MSe), color = "blue") +
  xlab("1 - Specificity") +
  ylab("Sensitivity") +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  theme_bw()

