###
require(ggplot2)

mu <- c(0.8, 0.9, 1)
sigma <- c(1, 1.2, 0.5)

dt_DIK <- area_DIK_normal(mu = mu, sigma = sigma)

ggplot(dt_DIK, aes(x = xx)) +
  geom_line(aes(y = fx1), colour = "black") +
  geom_line(aes(y = fx2), colour = "blue") +
  geom_line(aes(y = fx3), colour = "red") +
  geom_ribbon(aes(ymin = 0, ymax = fx_ovl), alpha = 0.3) +
  xlab("X") + ylab("Density") +
  theme_bw()

DIK_normal(mu = mu, sigma = sigma)
TAUC_normal(mu = mu, sigma = sigma)
ITFNR_normal(mu = mu, sigma = sigma)

###
mu <- c(0.8, 0.9, 1)
sigma <- c(1, 1, 1)
dt_DIK <- area_DIK_normal(mu = mu, sigma = sigma)

ggplot(dt_DIK, aes(x = xx)) +
  geom_line(aes(y = fx1), colour = "black") +
  geom_line(aes(y = fx2), colour = "blue") +
  geom_line(aes(y = fx3), colour = "red") +
  geom_ribbon(aes(ymin = 0, ymax = fx_ovl), alpha = 0.3) +
  xlab("X") + ylab("Density") +
  theme_bw()

DIK_normal(mu = mu, sigma = sigma)
TAUC_normal(mu = mu, sigma = sigma)
ITFNR_normal(mu = mu, sigma = sigma)

###
mu <- c(1, 1)
sigma <- c(1, 0.25)
dt_DIK <- area_DIK_normal(mu = mu, sigma = sigma)

ggplot(dt_DIK, aes(x = xx)) +
  geom_line(aes(y = fx1), colour = "black") +
  geom_line(aes(y = fx2), colour = "blue") +
  geom_ribbon(aes(ymin = 0, ymax = fx_ovl), alpha = 0.3) +
  xlab("X") + ylab("Density") +
  theme_bw()

DIK_normal(mu = mu, sigma = sigma)
TAUC_normal(mu = mu, sigma = sigma)
ITFNR_normal(mu = mu, sigma = sigma)

###
mu <- c(0, 0.1, 1)
sigma <- c(1, 1, 1)
dt_DIK <- area_DIK_normal(mu = mu, sigma = sigma)

ggplot(dt_DIK, aes(x = xx)) +
  geom_line(aes(y = fx1), colour = "black") +
  geom_line(aes(y = fx2), colour = "blue") +
  geom_line(aes(y = fx3), colour = "red") +
  geom_ribbon(aes(ymin = 0, ymax = fx_ovl), alpha = 0.3) +
  xlab("X") + ylab("Density") +
  theme_bw()

DIK_normal(mu = mu, sigma = sigma)
TAUC_normal(mu = mu, sigma = sigma)
ITFNR_normal(mu = mu, sigma = sigma)

###
mu <- c(0.8, 1, 2.5, 2.5)
sigma <- c(1, 1, 1, 1)

dt_DIK <- area_DIK_normal(mu = mu, sigma = sigma)

ggplot(dt_DIK, aes(x = xx)) +
  geom_line(aes(y = fx1), colour = "black") +
  geom_line(aes(y = fx2), colour = "blue") +
  geom_line(aes(y = fx3), colour = "red") +
  geom_line(aes(y = fx4), colour = "forestgreen") +
  geom_ribbon(aes(ymin = 0, ymax = fx_ovl), alpha = 0.3) +
  xlab("X") + ylab("Density") +
  theme_bw()

DIK_normal(mu = mu, sigma = sigma)
TAUC_normal(mu = mu, sigma = sigma)
ITFNR_normal(mu = mu, sigma = sigma)
