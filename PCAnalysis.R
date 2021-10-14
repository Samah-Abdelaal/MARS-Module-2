# PCA


# PCA respect and satisfaction


pca_resp <- prcomp(t(resp_satis), scale = TRUE)

plot(pca_resp$x[,1], pca_resp$x[,2])

pcavar_resp <- pca_resp$sdev^2
pcavar_per_resp <- round(pcavar_resp / sum(pcavar_resp) *100, 1)

barplot(pcavar_per_resp,
        main = "Scree Plot",
        xlab = "Principal Component",
        ylab = "Percent Variation")

(pca_resp_data <- data.frame(Sample = rownames(pca_resp$x),
                             X = pca_resp$x[, 1],
                             Y = pca_resp$x[, 2]))

ggplot(data = pca_resp_data,
       aes(x = X,
           y = Y,
           label = Sample)) +
  geom_text() +
  xlab(paste("PC1 - ", pcavar_per_resp[1], "%", sep = ""))+
  ylab(paste("PC1 - ", pcavar_per_resp[2], "%", sep = ""))+
  theme_bw() +
  ggtitle("My PCA Graph")

pca_resp <- prcomp(resp_satis, scale = TRUE)

loading_scores <- pca_resp$rotation[,1]
resp_scores <- abs(loading_scores)
resp_scores_ranked <- sort(resp_scores, decreasing = TRUE)
(top_10_resp <- names(resp_scores_ranked[1:10]))

# Different method

svd.stuff <- svd(scale(t(resp_satis)))

svd.data <- data.frame(Sample = colnames(resp_satis),
                       X = svd.stuff$u[,1] * svd.stuff$d[1],
                       Y = svd.stuff$u[,2] * svd.stuff$d[2])

# WTF
svd.pcs <- t(svd.stuff$v) %*% t(scale(resp_satis, center = TRUE))
# non-conformable arguments

svd.df <- ncol(resp_satis) - 1
svd.var <- svd.stuff$d^2 / svd.df
svd.var.per <- round(svd.var / sum(svd.var) * 100, 1)

ggplot(data = svd.data,
       aes(x = X,
           y = Y,
           label = Sample)) +
  geom_text() +
  xlab(paste("PC - ", svd.var.per[1], "%", sep = "")) +
  ylab(paste("PC - ", svd.var.per[2], "%", sep = "")) +
  theme_bw() +
  ggtitle("svd")

# third method

cov.mat <- cov(scale(t(resp_satis), center = TRUE))
dim(cov.mat)

eigen.stuff <- eigen(cov.mat, symmetric = TRUE)
dim(eigen.stuff$vectors)

head(eigen.stuff$vectors[, 1:2])


eigen.pcs <- t(t(eigen.stuff$vectors)) %*% t(scale(resp_satis, center = TRUE))
# same problem

eigen.data <- data.frame(Sample = rownames(eigen.pcs))

# PCA


# PCA openness and sharing


pca_open <- prcomp(t(open_share), scale = TRUE)

plot(pca_open$x[,1], pca_open$x[,2])

pcavar_open <- pca_open$sdev^2
pcavar_per_open <- round(pcavar_open / sum(pcavar_open) *100, 1)

barplot(pcavar_per_open,
        main = "Scree Plot",
        xlab = "Principal Component",
        ylab = "Percent Variation")

(pca_open_data <- data.frame(Sample = rownames(pca_open$x),
                             X = pca_open$x[, 1],
                             Y = pca_open$x[, 2]))

ggplot(data = pca_open_data,
       aes(x = X,
           y = Y,
           label = Sample)) +
  geom_text() +
  xlab(paste("PC1 - ", pcavar_per_open[1], "%", sep = ""))+
  ylab(paste("PC1 - ", pcavar_per_open[2], "%", sep = ""))+
  theme_bw() +
  ggtitle("My PCA Graph")

pca_open <- prcomp(open_share, scale = TRUE)

loading_scores <- pca_open$rotation[,1]
open_scores <- abs(loading_scores)
open_scores_ranked <- sort(open_scores, decreasing = TRUE)
(top_10_open <- names(open_scores_ranked[1:10]))

# Different method

svd.stuff <- svd(scale(t(open_share)))

svd.data <- data.frame(Sample = colnames(open_share),
                       X = svd.stuff$u[,1] * svd.stuff$d[1],
                       Y = svd.stuff$u[,2] * svd.stuff$d[2])

# WTF
svd.pcs <- t(svd.stuff$v) %*% t(scale(open_share, center = TRUE))
# non-conformable arguments

svd.df <- ncol(open_share) - 1
svd.var <- svd.stuff$d^2 / svd.df
svd.var.per <- round(svd.var / sum(svd.var) * 100, 1)

ggplot(data = svd.data,
       aes(x = X,
           y = Y,
           label = Sample)) +
  geom_text() +
  xlab(paste("PC - ", svd.var.per[1], "%", sep = "")) +
  ylab(paste("PC - ", svd.var.per[2], "%", sep = "")) +
  theme_bw() +
  ggtitle("svd")

# third method

cov.mat <- cov(scale(t(open_share), center = TRUE))
dim(cov.mat)

eigen.stuff <- eigen(cov.mat, symmetric = TRUE)
dim(eigen.stuff$vectors)

head(eigen.stuff$vectors[, 1:2])


eigen.pcs <- t(t(eigen.stuff$vectors)) %*% t(scale(open_share, center = TRUE))
# same problem

eigen.data <- data.frame(Sample = rownames(eigen.pcs))


# PCA associated communication factors


pca_ass <- prcomp(t(ass_factors), scale = TRUE)

plot(pca_ass$x[,1], pca_ass$x[,2])

pcavar_ass <- pca_ass$sdev^2
pcavar_per_ass <- round(pcavar_ass / sum(pcavar_ass) *100, 1)

barplot(pcavar_per_ass,
        main = "Scree Plot",
        xlab = "Principal Component",
        ylab = "Percent Variation")

(pca_ass_data <- data.frame(Sample = rownames(pca_ass$x),
                             X = pca_ass$x[, 1],
                             Y = pca_ass$x[, 2]))

ggplot(data = pca_ass_data,
       aes(x = X,
           y = Y,
           label = Sample)) +
  geom_text() +
  xlab(paste("PC1 - ", pcavar_per_ass[1], "%", sep = ""))+
  ylab(paste("PC1 - ", pcavar_per_ass[2], "%", sep = ""))+
  theme_bw() +
  ggtitle("My PCA Graph")

pca_ass <- prcomp(ass_factors, scale = TRUE)

loading_scores <- pca_ass$rotation[,1]
ass_scores <- abs(loading_scores)
ass_scores_ranked <- sort(ass_scores, decreasing = TRUE)
(top_10_ass <- names(ass_scores_ranked[1:10]))

# Different method

svd.stuff <- svd(scale(t(ass_factors)))

svd.data <- data.frame(Sample = colnames(ass_factors),
                       X = svd.stuff$u[,1] * svd.stuff$d[1],
                       Y = svd.stuff$u[,2] * svd.stuff$d[2])

# WTF
svd.pcs <- t(svd.stuff$v) %*% t(scale(ass_factors, center = TRUE))
# non-conformable arguments

svd.df <- ncol(ass_factors) - 1
svd.var <- svd.stuff$d^2 / svd.df
svd.var.per <- round(svd.var / sum(svd.var) * 100, 1)

ggplot(data = svd.data,
       aes(x = X,
           y = Y,
           label = Sample)) +
  geom_text() +
  xlab(paste("PC - ", svd.var.per[1], "%", sep = "")) +
  ylab(paste("PC - ", svd.var.per[2], "%", sep = "")) +
  theme_bw() +
  ggtitle("svd")

# third method

cov.mat <- cov(scale(t(ass_factors), center = TRUE))
dim(cov.mat)

eigen.stuff <- eigen(cov.mat, symmetric = TRUE)
dim(eigen.stuff$vectors)

head(eigen.stuff$vectors[, 1:2])


eigen.pcs <- t(t(eigen.stuff$vectors)) %*% t(scale(ass_factors, center = TRUE))
# same problem

eigen.data <- data.frame(Sample = rownames(eigen.pcs))
# PCA


# PCA all


pca_all <- prcomp(t(pcall), scale = TRUE)

plot(pca_all$x[,1], pca_all$x[,2])

pcavar_all <- pca_all$sdev^2
pcavar_per_all <- round(pcavar_all / sum(pcavar_all) *100, 1)

barplot(pcavar_per_all,
        main = "Scree Plot",
        xlab = "Principal Component",
        ylab = "Percent Variation")

(pca_all_data <- data.frame(Sample = rownames(pca_all$x),
                             X = pca_all$x[, 1],
                             Y = pca_all$x[, 2]))

ggplot(data = pca_all_data,
       aes(x = X,
           y = Y,
           label = Sample)) +
  geom_text() +
  xlab(paste("PC1 - ", pcavar_per_all[1], "%", sep = ""))+
  ylab(paste("PC1 - ", pcavar_per_all[2], "%", sep = ""))+
  theme_bw() +
  ggtitle("My PCA Graph")

pca_all <- prcomp(pcall, scale = TRUE)

loading_scores <- pca_all$rotation[,1]
all_scores <- abs(loading_scores)
all_scores_ranked <- sort(all_scores, decreasing = TRUE)
(top_10_all <- names(all_scores_ranked[1:10]))

# Different method

svd.stuff <- svd(scale(t(pcall)))

svd.data <- data.frame(Sample = colnames(pcall),
                       X = svd.stuff$u[,1] * svd.stuff$d[1],
                       Y = svd.stuff$u[,2] * svd.stuff$d[2])

# WTF
svd.pcs <- t(svd.stuff$v) %*% t(scale(pcall, center = TRUE))
# non-conformable arguments

svd.df <- ncol(pcall) - 1
svd.var <- svd.stuff$d^2 / svd.df
svd.var.per <- round(svd.var / sum(svd.var) * 100, 1)

ggplot(data = svd.data,
       aes(x = X,
           y = Y,
           label = Sample)) +
  geom_text() +
  xlab(paste("PC - ", svd.var.per[1], "%", sep = "")) +
  ylab(paste("PC - ", svd.var.per[2], "%", sep = "")) +
  theme_bw() +
  ggtitle("svd")

# third method

cov.mat <- cov(scale(t(pcall), center = TRUE))
dim(cov.mat)

eigen.stuff <- eigen(cov.mat, symmetric = TRUE)
dim(eigen.stuff$vectors)

head(eigen.stuff$vectors[, 1:2])


eigen.pcs <- t(t(eigen.stuff$vectors)) %*% t(scale(pcall, center = TRUE))
# same problem

eigen.data <- data.frame(Sample = rownames(eigen.pcs))

