library(ggpubr)
library(ggplot2)
library(RColorBrewer)
library(netClust)
library(reshape2)
# library(extrafont)

###########################
## All Methods displayed ##
###########################

## plot netClust versions for different numbers of covariates
res1 <- readRDS("results/results--k_clust-6--n_vars-20--n_bg-0--n_it-30--different--TRUE.rds")
# nice_plot(res1$correct_samples)
nbg_data <- melt(res1$correct_samples[,1:9])
nbg_data$nbg <- 0
res2 <- readRDS("results/results--k_clust-6--n_vars-20--n_bg-2--n_it-30--different--TRUE.rds")
# nice_plot(res2$correct_samples)
nbg_data_temp <- melt(res2$correct_samples[,1:9])
nbg_data_temp$nbg <- 2
nbg_data <- rbind(nbg_data,nbg_data_temp)
res3 <- readRDS("results/results--k_clust-6--n_vars-20--n_bg-4--n_it-30--different--TRUE.rds")
nbg_data_temp <- melt(res3$correct_samples[,1:9])
# nice_plot1 <- nice_plot(res3$correct_samples)
nbg_data_temp$nbg <- 4
nbg_data <- rbind(nbg_data,nbg_data_temp)
res4 <- readRDS("results/results--k_clust-6--n_vars-20--n_bg-6--n_it-30--different--TRUE.rds")
# nice_plot(res4$correct_samples)
nbg_data_temp <- melt(res4$correct_samples[,1:9])
nbg_data_temp$nbg <- 6
nbg_data <- rbind(nbg_data,nbg_data_temp)

colnames(nbg_data)[2] <- "Method"
color_list <- RColorBrewer::brewer.pal(n = 11, name = "RdYlBu")
cols <- color_list[c(2,9,4,10,1,7,5,8,3)]

# publication figure 2

# remove Mclust (since Gaussian mixture model not interesting for binary data)
nbg_data_reduced <- nbg_data[!nbg_data$Method=="Mclust (cov & var)"&!nbg_data$Method=="Mclust (var)",]
# create labels for publication
levels(nbg_data_reduced$Method) <- c("Cov-adjust (cov. & var.)", "BMMM (cov. & var.)",
                             "BMMM (var.)", "K-means (cov. & var.)", "K-means (var.)",
                             "MC", "MC2", "BMM (cov. & var.)", "BMM (var.)")

color_list <- RColorBrewer::brewer.pal(n = 11, name = "RdYlBu")
cols <- color_list[c(2,9,4,1,10,5,3)]

plot_nbg <- ggplot2::ggplot(data=nbg_data_reduced, aes(x=factor(nbg), y=value, fill = Method, colour = Method))+
  geom_boxplot(outlier.shape = NA, alpha = 0.5) +
  # geom_point(pch = 21, position = position_jitterdodge(0.15), cex = 0.4, alpha = 0.3) +
  # geom_jitter(pch = 21, cex = 0.8, alpha = 0.3) +
  geom_point(position = position_jitterdodge(), alpha = 0.5, pch = 21, cex = 0.9) +
  scale_colour_manual(values = cols) + scale_fill_manual(values = cols) +
  labs(y = "Adjusted Rand Index (ARI)", x = "Number of Covariates") +
  theme_minimal(); plot_nbg

plot_nbg_nolegend <- ggplot2::ggplot(data=nbg_data_reduced, aes(x=factor(nbg), y=value, fill = Method, colour = Method))+
  geom_boxplot(outlier.shape = NA, alpha = 0.5) +
  # geom_point(pch = 21, position = position_jitterdodge(0.15), cex = 0.4, alpha = 0.3) +
  # geom_jitter(pch = 21, cex = 0.8, alpha = 0.3) +
  geom_point(position = position_jitterdodge(), alpha = 0.5, pch = 21, cex = 0.9) +
  scale_colour_manual(values = cols) + scale_fill_manual(values = cols) +
  labs(y = "Adjusted Rand Index (ARI)", x = "Number of Covariates") +
  theme_minimal()+
  theme(legend.position = "none"); plot_nbg_nolegend


# save
png("~/Desktop/plot_nbg.png", width = 18, height = 7, units = 'cm', res = 300)
plot_nbg
dev.off()



