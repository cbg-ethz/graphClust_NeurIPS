library(ggpubr)
library(ggplot2)
library(RColorBrewer)
library(netClust)
library(reshape2)
# library(extrafont)

# # define functions
# nice_plot <- function(samples){
#   test_data <- melt(samples)
#   test_data$Var3 <- c(rep("Cov-Adjust", dim(samples)[1]), rep("netClust", 2*dim(samples)[1]),
#                       rep("kmeans", 2*dim(samples)[1]), rep("Mclust", 2*dim(samples)[1]),
#                       rep("BMM", 2*dim(samples)[1]))
# 
#   color_list <- RColorBrewer::brewer.pal(n = 8, name = "RdYlBu")
#   cols <- color_list[c(1, 3, 8, 3, 8, 3, 8, 3, 8)]
# 
#   names(test_data) <- c("Var1", "Method", "value", "Var3")
# 
#   p1 <- ggplot2::ggplot(data=test_data, aes(x=reorder(Var3, -value), y=value, fill = Method, colour = Method))+
#     geom_boxplot(outlier.shape = NA, alpha = 0.5) +
#     # geom_point(pch = 21, position = position_jitterdodge(0.15), cex = 0.4, alpha = 0.3) +
#     geom_jitter(pch = 21, cex = 0.8, alpha = 0.5) +
#     # geom_point(position = position_jitterdodge(), alpha = 0.3, pch = 21, cex = 0.9) +
#     scale_colour_manual(values = cols) + scale_fill_manual(values = cols) +
#     labs(y = "Adjusted Rand Index (ARI)", x = "Method") +
#     theme_minimal()+
#     theme(legend.position = "none")
# 
#   # get label
#   cols <- color_list[c(1, 3, 8, 3, 8, 3, 8, 3, 8)]
#   samples <- samples[,1:3]
#   test_data <- melt(samples)
#   names(test_data) <- c("Var1", "Data", "value")
# 
#   levels(test_data$Data)[1] <- "Covariate-Adjustment"
#   levels(test_data$Data)[2] <- "Variables & Covariates"
#   levels(test_data$Data)[3] <- "Variables"
# 
#   my_legend2 <- ggplot2::ggplot(data=test_data, aes(x=reorder(Data, -value), y=value, fill = Data, colour = Data))+
#     geom_boxplot(outlier.shape = NA, alpha = 0.3) +
#     # geom_point(pch = 21, position = position_jitterdodge(0.15), cex = 0.4, alpha = 0.3) +
#     geom_jitter(pch = 21, cex = 0.8, alpha = 0.3) +
#     # geom_point(position = position_jitterdodge(), alpha = 0.3, pch = 21, cex = 0.9) +
#     scale_colour_manual(values = cols) + scale_fill_manual(values = cols) +
#     labs(y = "Adjusted Rand Index (ARI)", x = "Method") +
#     theme_minimal()
# 
#   legend_2 <- get_legend(my_legend2)
# 
#   p_nice <- ggarrange(p1, legend_2, widths=c(0.65,0.35))
# 
#   return(p_nice)
# }

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

####################################


###############################
## Varied Number of Clusters ##
###############################

## plot netClust versions for different numbers of covariates
res1 <- readRDS("results/results--k_clust-2--n_vars-20--n_bg-10--n_it-20--different--TRUE.rds")
nice_plot(res1$correct_samples)
nbg_data <- melt(res1$correct_samples[,1:9])
nbg_data$nbg <- 2
res2 <- readRDS("results/results--k_clust-4--n_vars-20--n_bg-10--n_it-20--different--TRUE.rds")
nice_plot(res2$correct_samples)
nbg_data_temp <- melt(res2$correct_samples[,1:9])
nbg_data_temp$nbg <- 4
nbg_data <- rbind(nbg_data,nbg_data_temp)
res3 <- readRDS("results/results--k_clust-6--n_vars-20--n_bg-10--n_it-20--different--TRUE.rds")
nbg_data_temp <- melt(res3$correct_samples[,1:9])
nice_plot1 <- nice_plot(res3$correct_samples)
nbg_data_temp$nbg <- 6
nbg_data <- rbind(nbg_data,nbg_data_temp)
res4 <- readRDS("results/results--k_clust-8--n_vars-20--n_bg-10--n_it-20--different--TRUE.rds")
nice_plot(res4$correct_samples)
nbg_data_temp <- melt(res4$correct_samples[,1:9])
nbg_data_temp$nbg <- 8
nbg_data <- rbind(nbg_data,nbg_data_temp)

colnames(nbg_data)[2] <- "Method"
color_list <- RColorBrewer::brewer.pal(n = 11, name = "RdYlBu")
cols <- color_list[sample(c(1:9),9)]
cols <- color_list[c(2,9,4,10,1,7,5,8,3)]

plot_nclust <- ggplot2::ggplot(data=nbg_data, aes(x=factor(nbg), y=value, fill = Method, colour = Method))+
  geom_boxplot(outlier.shape = NA, alpha = 0.5) +
  # geom_point(pch = 21, position = position_jitterdodge(0.15), cex = 0.4, alpha = 0.3) +
  # geom_jitter(pch = 21, cex = 0.8, alpha = 0.3) +
  geom_point(position = position_jitterdodge(), alpha = 0.5, pch = 21, cex = 0.9) +
  scale_colour_manual(values = cols) + scale_fill_manual(values = cols) +
  labs(y = "Adjusted Rand Index (ARI)", x = "Number of Clusters") +
  theme_minimal(); plot_nclust

plot_nclust_nolegend <- ggplot2::ggplot(data=nbg_data, aes(x=factor(nbg), y=value, fill = Method, colour = Method))+
  geom_boxplot(outlier.shape = NA, alpha = 0.5) +
  # geom_point(pch = 21, position = position_jitterdodge(0.15), cex = 0.4, alpha = 0.3) +
  # geom_jitter(pch = 21, cex = 0.8, alpha = 0.3) +
  geom_point(position = position_jitterdodge(), alpha = 0.5, pch = 21, cex = 0.9) +
  scale_colour_manual(values = cols) + scale_fill_manual(values = cols) +
  labs(y = "Adjusted Rand Index (ARI)", x = "Number of Clusters") +
  theme_minimal()+
  theme(legend.position = "none"); plot_nclust_nolegend


# save
png("~/Desktop/plot_nclust.png", width = 14, height = 12, units = 'cm', res = 300)
plot_nclust
#don't forget to embed fonts.
#embed_fonts("plotname.pdf", outfile = "plotname_embed.pdf")
dev.off()

####################################


####################################
## Combine them ##
####################################

legend_3 <- get_legend(plot_nclust)

p_all <- ggarrange(plot_nbg_nolegend, plot_nclust_nolegend, legend_3, nrow = 1, widths=c(0.4,0.4,0.2))

# save
png("~/Desktop/plot_all.png", width = 22, height = 12, units = 'cm', res = 300)
p_all
dev.off()

####################################


# save
png("~/Desktop/plot_graphs.png", width = 18, height = 21, units = 'cm', res = 300)
par(mfrow = c(2, 3))
n_bg <- 4
node_col <- rep("darkgrey", n_bg)
names(node_col) <- as.character(20+1:n_bg)
graph::plot(res3$sampled_results$bayes_nets[[1]]$DAG, nodeAttrs=list(fillcolor=node_col))
graph::plot(res3$sampled_results$bayes_nets[[2]]$DAG, nodeAttrs=list(fillcolor=node_col))
graph::plot(res3$sampled_results$bayes_nets[[3]]$DAG, nodeAttrs=list(fillcolor=node_col))
graph::plot(res3$sampled_results$bayes_nets[[4]]$DAG, nodeAttrs=list(fillcolor=node_col))
graph::plot(res3$sampled_results$bayes_nets[[5]]$DAG, nodeAttrs=list(fillcolor=node_col))
graph::plot(res3$sampled_results$bayes_nets[[6]]$DAG, nodeAttrs=list(fillcolor=node_col))
#don't forget to embed fonts.
#embed_fonts("plotname.pdf", outfile = "plotname_embed.pdf")
dev.off()
par(mfrow = c(1,1))

# save
png("~/Desktop/plot_graphs_blue.png", width = 18, height = 21, units = 'cm', res = 300)
par(mfrow = c(2, 3))
n_bg <- 4
node_col <- rep(cols[3], n_bg)
names(node_col) <- as.character(20+1:n_bg)
graph::plot(res3$sampled_results$bayes_nets[[1]]$DAG, nodeAttrs=list(fillcolor=node_col))
graph::plot(res3$sampled_results$bayes_nets[[2]]$DAG, nodeAttrs=list(fillcolor=node_col))
graph::plot(res3$sampled_results$bayes_nets[[3]]$DAG, nodeAttrs=list(fillcolor=node_col))
graph::plot(res3$sampled_results$bayes_nets[[4]]$DAG, nodeAttrs=list(fillcolor=node_col))
graph::plot(res3$sampled_results$bayes_nets[[5]]$DAG, nodeAttrs=list(fillcolor=node_col))
graph::plot(res3$sampled_results$bayes_nets[[6]]$DAG, nodeAttrs=list(fillcolor=node_col))
#don't forget to embed fonts.
#embed_fonts("plotname.pdf", outfile = "plotname_embed.pdf")
dev.off()
par(mfrow = c(1,1))

## plot netClust versions for different numbers of covariates

res1 <- readRDS("results/results--k_clust-6--n_vars-20--n_bg-0--n_it-30--different--TRUE.rds")
nice_plot(res1$correct_samples)
nbg_data <- melt(res1$correct_samples[,2:9])
nbg_data$nbg <- 0
res2 <- readRDS("results/results--k_clust-6--n_vars-20--n_bg-2--n_it-30--different--TRUE.rds")
nice_plot(res2$correct_samples)
nbg_data_temp <- melt(res2$correct_samples[,2:9])
nbg_data_temp$nbg <- 2
nbg_data <- rbind(nbg_data,nbg_data_temp)
res3 <- readRDS("results/results--k_clust-6--n_vars-20--n_bg-4--n_it-30--different--TRUE.rds")
nbg_data_temp <- melt(res3$correct_samples[,2:9])
nbg_data_temp$nbg <- 4
nbg_data <- rbind(nbg_data,nbg_data_temp)
nice_plot(res3$correct_samples)
res4 <- readRDS("results/results--k_clust-6--n_vars-20--n_bg-6--n_it-30--different--TRUE.rds")
nice_plot(res4$correct_samples)
nbg_data_temp <- melt(res4$correct_samples[,2:9])
nbg_data_temp$nbg <- 6
nbg_data <- rbind(nbg_data,nbg_data_temp)

colnames(nbg_data)[2] <- "Method"

ggplot2::ggplot(data=nbg_data, aes(x=reorder(nbg, value), y=value, fill = Method, colour = Method))+
  geom_boxplot(outlier.shape = NA, alpha = 0.3) +
  # geom_point(pch = 21, position = position_jitterdodge(0.15), cex = 0.4, alpha = 0.3) +
  # geom_jitter(pch = 21, cex = 0.8, alpha = 0.3) +
  geom_point(position = position_jitterdodge(), alpha = 0.3, pch = 21, cex = 0.9) +
  scale_colour_manual(values = cols[2:9]) + scale_fill_manual(values = cols[2:9]) +
  labs(y = "Adjusted Rand Index (ARI)", x = "Number of Covariates") +
  theme_minimal()


samples <- res3$correct_samples[,2:9]
test_data <- melt(samples)
test_data$Var3 <- c(rep("netClust", 2*dim(samples)[1]),
                    rep("kmeans", 2*dim(samples)[1]), rep("Mclust", 2*dim(samples)[1]),
                    rep("BMM", 2*dim(samples)[1]))

color_list <- RColorBrewer::brewer.pal(n = 4, name = "RdYlBu")
cols <- color_list[c(3, 4, 3, 4, 3, 4, 3, 4)]

names(test_data) <- c("Var1", "Method", "value", "Var3")

temp_plot <- ggplot2::ggplot(data=test_data, aes(x=reorder(Var3, -value), y=value, fill = Method, colour = Method))+
  geom_boxplot(outlier.shape = NA, alpha = 0.3) +
  # geom_point(pch = 21, position = position_jitterdodge(0.15), cex = 0.4, alpha = 0.3) +
  geom_jitter(pch = 21, cex = 0.8, alpha = 0.3) +
  # geom_point(position = position_jitterdodge(), alpha = 0.3, pch = 21, cex = 0.9) +
  scale_colour_manual(values = cols) + scale_fill_manual(values = cols) +
  labs(y = "Adjusted Rand Index (ARI)", x = "Method") +
  theme_minimal()+
  theme(legend.position = "none")


p_other <- ggarrange(temp_plot, legend_1, widths=c(0.7,0.3))

# save
png("~/Desktop/plot_other.png", width = 14, height = 12, units = 'cm', res = 300)
p_other
#don't forget to embed fonts.
#embed_fonts("plotname.pdf", outfile = "plotname_embed.pdf")
dev.off()

p1 <- ggplot2::ggplot(data=nbg_data, aes(x=reorder(nbg, value), y=value, fill = Method, colour = Method))+
  geom_boxplot(outlier.shape = NA, alpha = 0.3) +
  # geom_point(pch = 21, position = position_jitterdodge(0.15), cex = 0.4, alpha = 0.3) +
  # geom_jitter(pch = 21, cex = 0.8, alpha = 0.3) +
  geom_point(position = position_jitterdodge(), alpha = 0.3, pch = 21, cex = 0.9) +
  scale_colour_manual(values = cols) + scale_fill_manual(values = cols) +
  labs(y = "Adjusted Rand Index (ARI)", x = "Number of Covariates") +
  theme_minimal()+
  theme(legend.position = "none")

p_nbg_other <- ggarrange(p1, legend_1, widths=c(0.85,0.15))

# save
png("~/Desktop/plot_nbg_other.png", width = 18, height = 21, units = 'cm', res = 300)
p_nbg_other
#don't forget to embed fonts.
#embed_fonts("plotname.pdf", outfile = "plotname_embed.pdf")
dev.off()




