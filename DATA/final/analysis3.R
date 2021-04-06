library(factoextra)
library(cluster)
x <- readRDS('simpleFINAL.RDS')
for(i in c('k3', 'k4', 'k5')){x[, i] <- factor(x[, i])}
v <- lapply(split(x, x$ID), function(z) z[, c(1:2, 4:15)])
k <- function(x){x[, grep('pattern_norm', colnames(x))]}
kplot <- function(x, kvar = NULL, main = NULL, plot = TRUE, v = 1){
  if(is.null(kvar)){
    stopifnot('k' %in% colnames(x))
    kvar <- 'k'
  }
  if(is(kvar, 'kmeans')){
    x$kvar <- kvar$cluster
    kvar <- 'kvar'
  }
  values <- c('cat_pattern', 'room_pattern', 
              switch(v, 'time_pattern', c('time_forward', 'time_backward')))
  if(any(grepl('norm', colnames(x)))){values <- paste0(values, '_norm')}
  patterns <- c('Category', 'Room', 'Forward', 'Reverse')
  if(v == 1){patterns <- c(patterns[1:2], 'Time')}
  new <- structure(data.frame(
    Value = unlist(x[, values]),
    Pattern = factor(rep(patterns, each = nrow(x)), levels = patterns),
    Cluster = factor(paste('Cluster', rep(x[, kvar], switch(v, 3, 4))))
  ), row.names = 1:(switch(v, 3, 4) * nrow(x)))
  if(plot){
    require(ggplot2)
    g <- ggplot(new, aes(x = Pattern, y = Value)) +
      geom_boxplot(aes(fill = Pattern)) + 
      facet_grid(~ Cluster) + theme_bw()
    if(!is.null(main)){g <- g + ggtitle(main)}
    new <- g
  }
  return(new)
}
km <- function(x, centers = 3, seed = 1, nstart = 25){
  if(!is.null(seed)){set.seed(seed)}
  xx <- x[, grep('pattern_norm', colnames(x))]
  kk <- kmeans(xx, centers = centers, nstart = nstart)
  x$k <- factor(kk$cluster)
  return(x)
}


# v1: overall means by ID
v1 <- data.frame(do.call(rbind, lapply(v, function(z){
  z1 <- t(data.frame(colMeans(z[, -(1:3)])))
  return(data.frame(ID = unique(z$ID), z1))
})))

set.seed(1)
gs1 <- clusGap(k(v1), kmeans, 10, 100, nstart = 25)
fviz_gap_stat(gs1) # 3
fviz_nbclust(k(v1), kmeans) # 2
kv1 <- kmeans(k(v1), centers = 3, nstart = 25)
kplot(v1, kv1)

# v2: means by condition and ID
v2 <- structure(data.frame(do.call(rbind, lapply(v, function(z){
  z1 <- split(z, z$condition)
  z2 <- suppressWarnings(data.frame(do.call(rbind, lapply(z1, function(i){
    t(data.frame(colMeans(i[, -(1:3)])))
  }))))
  z3 <- data.frame(ID = unique(z$ID), condition = names(z1), z2)
  return(z3)
}))), row.names = 1:280)

set.seed(1)
gs2 <- clusGap(k(v2), kmeans, 10, 100, nstart = 25)
fviz_gap_stat(gs2) # 5
fviz_nbclust(k(v2), kmeans) # 2
kv2 <- kmeans(k(v2), centers = 5, nstart = 25)
v2 <- km(v2, centers = 5)
kplot(v2)

# v3: means by session and ID
v3 <- structure(data.frame(do.call(rbind, lapply(v, function(z){
  z1 <- split(z, z$session)
  z2 <- suppressWarnings(data.frame(do.call(rbind, lapply(z1, function(i){
    t(data.frame(colMeans(i[, -(1:3)])))
  }))))
  z3 <- data.frame(ID = unique(z$ID), session = names(z1), z2)
  return(z3)
}))), row.names = 1:168)

set.seed(1)
gs3 <- clusGap(k(v3), kmeans, 10, 100, nstart = 25)
fviz_gap_stat(gs3) # 2
fviz_nbclust(k(v3), kmeans) # 2
kv3 <- kmeans(k(v3), centers = 3, nstart = 25)
v3 <- km(v3, centers = 2)
kplot(v3)

# v4: means by session, condition, and ID
v4 <- structure(data.frame(do.call(rbind, lapply(v, function(z){
  z1 <- split(z, z$condition)
  z2 <- lapply(z1, function(i) split(i, i$session))
  z3 <- data.frame(do.call(rbind, lapply(z2, function(i) data.frame(do.call(rbind, lapply(i, function(j){
    data.frame(ID = unique(j$ID), session = unique(j$session), 
               condition = unique(j$condition), 
               t(data.frame(colMeans(j[, -(1:3)]))))
  }))))))
  z3 <- z3[order(z3$session), ]
  return(z3)
}))), row.names = 1:840)

set.seed(1)
gs4 <- clusGap(k(v4), kmeans, 10, 100, nstart = 25)
fviz_gap_stat(gs4) # 3
fviz_nbclust(k(v4), kmeans) # 2
kv4 <- kmeans(k(v4), centers = 3, nstart = 25)
v4 <- km(v4, centers = 3)
kplot(v4)

################################################################################
################################################################################
for(i in c('ID', 'session', 'condition', 'k')){
  v4[, i] <- factor(v4[, i])
}

library(lme4)
library(MCMCglmm)
vignette('CourseNotes', 'MCMCglmm')

m0 <- MCMCglmm(k ~ trait + condition + session - 1, random = ~ID, 
               data = v4, rcov = ~us(trait):units, 
               family = 'categorical')

m1 <- MCMCglmm(k ~ trait + condition * session - 1, random = ~ID, 
               data = v4, rcov = ~us(trait):units, 
               family = 'categorical')

levels(x$k3) <- c('2', '1', '3')
x$k3 <- factor(as.numeric(as.character(x$k3)))
levels(x$k3) <- c('None', 'Room', 'CatRoom')

m1 <- lmer(item_correct ~ condition + trial + (1|ID) + (1|session),
           data = x, REML = FALSE)
m2 <- lmer(item_correct ~ condition + trial + k3 + (1|ID) + (1|session),
           data = x, REML = FALSE)
m3 <- lmer(item_correct ~ condition * k3 + trial + (1|ID) + (1|session),
           data = x, REML = FALSE)
anova(m1, m2, m3)
summary(m3)


levels(v4$k) <- c('2', '3', '1')
v4$k <- factor(as.numeric(as.character(v4$k)))
levels(v4$k) <- c('None', 'Room', 'CatRoom')

mm1 <- lmer(item_correct ~ condition + session + (1|ID),
            data = v4, REML = FALSE)
mm2 <- lmer(item_correct ~ condition + session + k + (1|ID),
            data = v4, REML = FALSE)
anova(mm1, mm2)
summary(mm2)

### MCMC functions
plot.estimates <- function(x){
  if('Sol' %in% names(x)){x <- x$Sol}
  if(class(x) != "summary.mcmc"){x <- summary(x)}
  n <- dim(x$statistics)[1]
  par(mar = c(2, 7, 4, 1))
  plot(x$statistics[, 1], n:1,
       yaxt = "n", ylab = "",
       xlim = range(x$quantiles) * 1.2,
       pch = 19,
       main = "Posterior means and 95% credible intervals")
  grid()
  axis(2, at = n:1, rownames(x$statistics), las = 2)
  arrows(x$quantiles[, 1], n:1, x$quantiles[, 5], n:1, code = 0)
  abline(v = 0, lty = 2)
}

plot.acfs <- function(x){ # Need to fix for larger models
  if('Sol' %in% names(x)){x <- x$Sol}
  n <- dim(x)[2]
  par(mfrow = c(ceiling(n/2), 2), mar = c(3, 2, 3, 0))
  for(i in 1:n){
    acf(x[, i], lag.max = 100, main = colnames(x)[i])
    grid()
  }
}

trace.plots <- function(x){ # Need to fix for larger models
  if('Sol' %in% names(x)){x <- x$Sol}
  n <- dim(x)[2]
  par(mfrow = c(ceiling(n/2), 2), mar = c(0, 0.5, 1, 0.5))
  for(i in 1:n){
    plot(as.numeric(x[, i]), t = "l", main = colnames(x)[i], xaxt = "n", yaxt = "n")
  }
}

