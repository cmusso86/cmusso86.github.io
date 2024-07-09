rm(list=ls())
install.packages("recalibratiNN")
library(recalibratiNN)
load("data/diamonds_example_gaussian.RData")


y2.pit <- PIT_global(
  ycal = y.val,
  yhat = y2.val,
  mse = y2.mse.val
)

gg_PIT_global(y2.pit)

pit_local <- PIT_local(
  xcal = x.val,
  ycal = y.val,
  yhat = y2.val,
  mse = y2.mse.val
)

gg_PIT_local(pit_local)


y2.recal <- recalibrate(
  yhat_new = y2.test,
  pit_values = y2.pit,
  mse = y2.mse.val,
  space_cal = h2.val,
  space_new = h2.test,
  type = 'local'
)

y2.recal2 <- recalibrate(
  yhat_new = y2.test,
  pit_values = y2.pit,
  mse = y2.mse.val,
  space_cal = h2.val,
  space_new = h2.test,
  type = 'local'
)


y_hat_recalib <- y2.recal$y_samples_calibrated_wt

# empirical p-value distribution
pit_new <- purrr::map_dbl(
  1:length(y.test), ~{
    mean(y_hat_recalib[.,] <= y.test[.] )
  })
gg_PIT_global(pit_new, print_p = F)


## pit local

n_neighbours <- 800
clusters <- 6

# calculating centroids
cluster_means_cal <- stats::kmeans(x.test, clusters)$centers
cluster_means_cal <- cluster_means_cal[order(cluster_means_cal[,1]),]


# finding neighbours
knn_cal <- RANN::nn2(x.test, cluster_means_cal,  k=n_neighbours)$nn.idx


# geting corresponding ys (real and estimated)
y_new_local <- purrr::map(1:nrow(knn_cal),  ~y.test[knn_cal[.,]])
y_hat_local <-purrr::map(1:nrow(knn_cal),  ~y_hat_recalib[knn_cal[.,],])


# calculate pit_local

pits <- matrix(NA, nrow=6, ncol=800)
for (i in 1:clusters) {
  pits[i,] <- purrr::map_dbl(1:length(y_new_local[[1]]), ~{
    mean(y_hat_local[[i]][.,] <= y_new_local[[i]][.])
  })
}


as.data.frame(t(pits)) %>%
  pivot_longer(everything()) %>%
  ggplot()+
  geom_density(aes(value,
                   color=name,
                   fill=name),
               alpha=0.5,
               bounds = c(0, 1))+
  geom_hline(yintercept=1, linetype="dashed")+
  scale_color_brewer(palette="Set2")+
  scale_fill_brewer(palette="Set2")+
  theme_classic()+
  theme(legend.position = "none")+
  labs(title = "After Local Calibration",
       subtitle= "It looks so much better!!",
       x="PIT-values", y="Density")

