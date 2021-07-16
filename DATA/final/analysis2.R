### Memory Museum Pilot Study Analysis - PART 2
# Trevor James Swanson
# 11/30/2020

### FUNCTIONS
splam <- function(x){
  if(is(x, 'list')){
    x <- data.frame(do.call(rbind, lapply(x, function(z) data.frame(do.call(rbind, z)))))
    rownames(x) <- 1:nrow(x)
  } else {
    x <- setNames(lapply(split(x, x$ID), function(z) setNames(split(z, z$trial), paste0('t', 1:(nrow(z)/9)))), paste0('p', 1:56))
    if(all(sapply(x, sapply, nrow) == 1)){x <- lapply(x, function(z) structure(do.call(rbind, z), row.names = 1:length(z)))}
  }
  return(x)
}

svar <- function(x, FUN = NULL, v = 'X'){
  splam(lapply(splam(x), function(z){
    lapply(z, function(k){
      k$X <- FUN(k)
      names(k)[which(names(k) == 'X')] <- v
      return(k)
    })
  }))
}

sorder <- function(x, type = c('word', 'loc', 'time')){
  splamit <- is(x, 'list')
  if(isTRUE(splamit)){x <- splam(x)}
  type <- match.arg(type)
  z <- c('ID', 'session', 'trial', 'condition')
  zc <- colnames(x)[endsWith(colnames(x), '_correct')]
  if(length(zc) > 0){colnames(x)[which(colnames(x) == zc)] <- paste0(zc, '1')}
  z1 <- colnames(x)[endsWith(colnames(x), '1')]
  z0 <- colnames(x)[endsWith(colnames(x), '0')]
  x <- splam(lapply(splam(x), function(j){
    lapply(j, function(k){
      if(type == 'word'){
        k1 <- apply(k[, c('cat1', 'src1')], 1, paste0, collapse = '.')
        k0 <- apply(k[, c('cat0', 'src0')], 1, paste0, collapse = '.')
      } else {
        k1 <- k[, paste0(type, '1')]
        k0 <- k[, paste0(type, '0')]
      }
      kk <- match(k0, k1)
      return(cbind(k[, z], k[kk, z1], k[, z0]))
    })
  }))
  if(length(zc) > 0){
    x <- data.frame(x[, setdiff(colnames(x), paste0(zc, '1'))], x[, paste0(zc, '1')])
    colnames(x)[grep('_correct1$', colnames(x))] <- gsub('1', '', colnames(x)[grep('_correct1$', colnames(x))])
  }
  if(isTRUE(splamit)){x <- splam(x)}
  return(x)
}


### Check performance by item
x <- readRDS('fullData2.RDS')
x <- x[, c(1:4, 6:15)]
x <- sorder(x, type = 'word')
x$item_correct1 <- as.numeric(x$loc1 == x$loc0)
x$room_correct1 <- as.numeric(x$room1 == x$room0)
x$time_correct1 <- as.numeric(x$time1 == x$time0)

x <- sorder(x, type = 'loc')
x$cat_correct1 <- as.numeric(x$cat1 == x$cat0)
catroom <- function(x){
  k1 <- apply(x[, c('cat1', 'room1')], 1, paste0, collapse = '.')
  k0 <- apply(x[, c('cat0', 'room0')], 1, paste0, collapse = '.')
  as.numeric(k1 %in% k0)
}
x <- svar(x, catroom, 'cat_room_correct1')
x <- sorder(x, type = 'time')
#saveRDS(x, 'fullData3.RDS')


### Total performance
x <- svar(x, function(x) sum(x$item_correct1)/9, 'item_total')
x <- svar(x, function(x) sum(x$room_correct1)/9, 'room_total')
x <- svar(x, function(x) sum(x$time_correct1)/9, 'time_total')
x <- svar(x, function(x) sum(x$cat_correct1)/9, 'cat_total')
x <- svar(x, function(x) sum(x$cat_room_correct1)/9, 'cat_room_total')
xx <- structure(x[seq(1, nrow(x), by = 9), -grep('1$|0$', colnames(x))], row.names = 1:(nrow(x)/9))


### REACTION TIME
source('extractData.R')
out <- getData(getJSONS(), matchPs = TRUE)
rmPractice <- TRUE
rts <- lapply(out, function(z){
  z1 <- sapply(z, function(k){
    k1 <- as.numeric(subset(k, stimulus == 'sort_trial')[, 'rt'])
    if(length(k1) == 32 & isTRUE(rmPractice)){k1 <- k1[-(1:2)]}
    return(k1)
  })
  data.frame(ID = rep(1:56, each = nrow(z1)), 
             trial = rep(1:nrow(z1), 56), 
             rt = c(z1))
})
rts$dat2$trial <- rts$dat2$trial + max(rts$dat1$trial)
rts$dat3$trial <- rts$dat3$trial + max(rts$dat2$trial)
rts <- data.frame(do.call(rbind, rts))
rts <- structure(rts[order(rts$ID), ], row.names = 1:nrow(rts))
rts$ID <- factor(rts$ID)

xx$rt <- rts$rt
colnames(xx) <- gsub('total', 'correct', colnames(xx))
#saveRDS(xx, 'simple3.RDS')


#################################### ANALYSIS 
x <- readRDS('fullData2.RDS')
x$correct <- NULL

perm <- function(v){
  n <- length(v)
  if(n == 1){
    return(v)
  } else {
    X <- NULL
    for(i in 1:n){X <- rbind(X, cbind(v[i], perm(v[-i])))}
    X
  }
}

triple <- function(v){matrix(rep(v, 3), ncol = 3)}

combs <- perm(1:3)
combs <- data.frame(t(cbind(triple(combs[, 1]), triple(combs[, 2]), triple(combs[, 3]))))


codecat <- function(x){
  cat2 <- factor(x$cat1)
  levels(cat2) <- 1:3
  as.numeric(as.character(cat2))
}

x <- svar(x, codecat, 'cat2')
# ID 50, trial 1, example of ordering

x <- splam(x)

# Cat correlations
out <- vector('list', length(x))
for(i in seq_along(out)){
  out[[i]] <- vector('list', length(x[[i]]))
  for(j in seq_along(x[[i]])){
    out[[i]][[j]] <- cor(x[[i]][[j]]$cat2, combs)
  }
  out[[i]] <- data.frame(trial = 1:length(out[[i]]), do.call(rbind, out[[i]]))
}
cats <- data.frame(ID = rep(1:56, each = nrow(out[[1]])), do.call(rbind, out))

# Room correlations
out <- vector('list', length(x))
for(i in seq_along(out)){
  out[[i]] <- vector('list', length(x[[i]]))
  for(j in seq_along(x[[i]])){
    out[[i]][[j]] <- cor(x[[i]][[j]]$room1, combs)
  }
  out[[i]] <- data.frame(trial = 1:length(out[[i]]), do.call(rbind, out[[i]]))
}
rooms <- data.frame(ID = rep(1:56, each = nrow(out[[1]])), do.call(rbind, out))

# Uniques
cats$uni <- apply(abs(cats[, -(1:2)]), 1, function(z) length(unique(z)))
rooms$uni <- apply(abs(rooms[, -(1:2)]), 1, function(z) length(unique(z)))

cats1 <- abs(cats[, c(1:5, 9)])
rooms1 <- abs(rooms[, c(1:5, 9)])

cats1$means <- rowMeans(cats1[, 3:5])
colnames(cats1)[6:7] <- c('cats_uni', 'cats_means')
rooms1$means <- rowMeans(rooms1[, 3:5])
colnames(rooms1)[6:7] <- c('rooms_uni', 'rooms_means')

y <- readRDS('fullData3.RDS')
y <- sorder(y, 'word')
y <- splam(y)
out <- vector('list', length(y))
for(i in seq_along(out)){
  out[[i]] <- numeric(length(y[[i]]))
  for(j in seq_along(y[[i]])){
    out[[i]][j] <- cor(y[[i]][[j]]$time1, y[[i]][[j]]$time0)
  }
}

final <- data.frame(cats1[, c(1, 2, 7)], rooms1[, 7, drop = FALSE], time = unlist(out))
colnames(final)[3:5] <- c('cat_pattern', 'room_pattern', 'time_pattern')
x <- splam(x)
y <- splam(y)

### CLUSTERING
library(factoextra)
library(cluster)
#dat <- final[, 3:5]

normal <- function(x){
  (x - min(x))/(max(x) - min(x))
}

final2 <- final
for(i in 3:5){
  final2[, i] <- normal(final2[, i])
}

dat <- final2[, 3:5]

# Determine optimal number of clusters
fviz_nbclust(dat, kmeans, method = 'wss')

set.seed(1)
gap_stat <- clusGap(dat, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
#saveRDS(gap_stat, 'gap_stat2.RDS')
fviz_gap_stat(gap_stat)

# Perform clustering
set.seed(1)
km3 <- kmeans(dat, centers = 3, nstart = 25)
set.seed(1)
km4 <- kmeans(dat, centers = 4, nstart = 25)
set.seed(1)
km5 <- kmeans(dat, centers = 5, nstart = 25)

fviz_cluster(km3, data = dat)
fviz_cluster(km4, data = dat)
fviz_cluster(km5, data = dat)

aggregate(dat, by = list(cluster = km3$cluster), mean)
aggregate(dat, by = list(cluster = km4$cluster), mean)
aggregate(dat, by = list(cluster = km5$cluster), mean)

final2$k3 <- km3$cluster
final2$k4 <- km4$cluster
final2$k5 <- km5$cluster

# Reformat simple dataset
final3 <- setNames(final2[, 3:5], paste0(colnames(final2[, 3:5]), '_norm'))
xx$cat_room_correct <- NULL
xx <- data.frame(xx, final[, -(1:2)], final3, k3 = km3$cluster, 
                 k4 = km4$cluster, k5 = km5$cluster)

#write.csv(xx, 'simpleFINAL.csv', row.names = FALSE)
#saveRDS(xx, 'simpleFINAL.RDS')

new3 <- structure(
  data.frame(Value = unlist(xx[, 13:15]), 
             Pattern = rep(c('Category', 'Room', 'Time'), each = nrow(xx)),
             Cluster = rep(xx$k3, 3)), 
  row.names = 1:(3 * nrow(xx))
)
new3$Pattern <- factor(new3$Pattern)
new3$Cluster <- factor(new3$Cluster)
levels(new3$Cluster) <- paste('Cluster', 1:3)

new4 <- new3
new4$Cluster <- rep(factor(xx$k4), 3)
levels(new4$Cluster) <- paste('Cluster', 1:4)

new5 <- new3
new5$Cluster <- rep(factor(xx$k5), 3)
levels(new5$Cluster) <- paste('Cluster', 1:5)


pdf('ClusterDistributions.pdf', height = 7, width = 10)
ggplot(new3, aes(x = Pattern, y = Value)) +
  geom_boxplot(aes(fill = Pattern)) + 
  facet_grid(~ Cluster) + theme_bw() +
  ggtitle('Three-Cluster Solution')

ggplot(new4, aes(x = Pattern, y = Value)) +
  geom_boxplot(aes(fill = Pattern)) + 
  facet_grid(~ Cluster) + theme_bw() +
  ggtitle('Four-Cluster Solution')

ggplot(new5, aes(x = Pattern, y = Value)) +
  geom_boxplot(aes(fill = Pattern)) + 
  facet_grid(~ Cluster) + theme_bw() +
  ggtitle('Five-Cluster Solution')
dev.off()
