################################
#'## print interaction
################################

regression1 <- lm(stocking_rate ~ agricultural_credits, data = subset(dsdrop3, zdc_bin == 0))
regression2 <- lm(stocking_rate ~ agricultural_credits, data = subset(dsdrop3, zdc_bin == 1))
# Plot the data points and linear regressions
ggplot(dsdrop3, aes(x = agricultural_credits, y = stocking_rate, color = factor(zdc_bin))) +
  geom_point() +
  #geom_smooth(method = "lm", formula = dsdrop3$stocking_rate ~ dsdrop3$zdc_exposure, se = FALSE, color = "black") +
  geom_abline(intercept = coef(regression1)[1], slope = coef(regression1)[2], color = "blue") +
  geom_abline(intercept = coef(regression2)[1], slope = coef(regression2)[2], color = "red") +
  labs(x = "ABC credits/total agricultural credits", y = "Stocking rates") +
  scale_color_manual(values = c("blue", "red"), labels = c("0", "1"), name = "ZDC binary")+
  theme_classic()
ggsave('Figures/SI_1.png', height = 800 ,width =1400 ,units = 'px')
