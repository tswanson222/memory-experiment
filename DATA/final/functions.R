if(!require(ggplot2)){install.packages('ggplot2', dependencies = TRUE)}
if(!require(lme4)){install.packages('lme4', dependencies = TRUE)} # v1.1-19
if(!require(lsmeans)){install.packages('lsmeans', dependencies = TRUE)}
if(!require(effects)){install.packages('effects', dependencies = TRUE)}
if(!require(factoextra)){install.packages('factoextra', dependencies = TRUE)}
if(!require(cluster)){install.packages('cluster', dependencies = TRUE)}
if(!require(MCMCglmm)){install.packages('MCMCglmm', dependencies = TRUE)}
if(!require(RLRsim)){install.packages('RLRsim', dependencies = TRUE)}
if(!require(pbkrtest)){install.packages('pbkrtest', dependencies = TRUE)}
if(!require(lattice)){install.packages('lattice', dependencies = TRUE)}
if(!require(sjPlot)){install.packages('sjPlot', dependencies = TRUE)}
if(!require(forecast)){install.packages('forecast', dependencies = TRUE)}


# splam
splam <- function(x){
  if(is(x, 'list')){
    x <- data.frame(do.call(rbind, lapply(x, function(z) data.frame(do.call(rbind, z)))))
    rownames(x) <- 1:nrow(x)
  } else {
    ids <- as.numeric(as.character(unique(x$ID)))
    x <- setNames(lapply(split(x, x$ID), function(z) setNames(split(z, z$trial), paste0('t', 1:(nrow(z)/9)))), paste0('p', ids))
    if(all(sapply(x, sapply, nrow) == 1)){x <- lapply(x, function(z) structure(do.call(rbind, z), row.names = 1:length(z)))}
  }
  return(x)
}

# svar
svar <- function(x, FUN = NULL, v = 'X'){
  splam(lapply(splam(x), function(z){
    lapply(z, function(k){
      k$X <- FUN(k)
      names(k)[which(names(k) == 'X')] <- v
      return(k)
    })
  }))
}

# sorder
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

# normal
normal <- function(x){(x - min(x))/(max(x) - min(x))}

# kplot
kplot <- function(x, kvar = NULL, main = NULL, plot = TRUE, v = 2){
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

# exclude: remove participants
exclude <- function(x, thresh = FALSE, var = c('correct', 'rt'), dat = TRUE){
  if(is.character(thresh)){var <- thresh}
  var <- switch(match.arg(var), correct = 'item_correct', rt = 'rt')
  if(isTRUE(thresh)){
    thresh <- ifelse(var == 'rt', ifelse(any(sign(x$rt) %in% -1), 1, 49000), .99)
  }
  if(any(grepl('src', colnames(x)))){
    messages("Havent coded yet")
  } else {
    perf <- sapply(lapply(split(x, x$ID), '[[', var), mean)
    if(is.numeric(thresh)){
      ids <- names(which(perf >= thresh))
      if(isTRUE(dat)){
        x <- subset(x, !ID %in% ids)
        return(x)
      } else {
        return(ids)
      }
    }
    return(perf)
  }
}

# plmer
plmer <- function(x, ylim = NULL, title = NULL, type = 'std', 
                  sort.est = TRUE, vline.color = 'gray80', ...){
  out <- suppressMessages(
    plot_model(x, type = type, title = title, sort.est = sort.est,
               vline.color = vline.color, ...) + theme_bw()
  )
  if(!is.null(ylim)){
    if(length(ylim) == 2){
      out <- suppressMessages(out + ylim(ylim[1], ylim[2]))
    }
  }
  return(out)
}

# rmp
rmp <- function(x, onlyp = FALSE){
  if(isTRUE(onlyp)){
    x <- subset(x, trial %in% 1:2)
  } else {
    x <- subset(x, !trial %in% 1:2)
    x$trial <- x$trial - 2
  }
  return(x)
}

# fmt
clust <- function(x, centers = 3, seed = 1, scalert = TRUE, getk = FALSE, rm = NULL, add_err = FALSE){
  fix <- isTRUE(rm)
  if(is.character(x)){x <- readRDS(x)}
  if(!identical(add_err, FALSE) & is.character(add_err)){
    dat <- readRDS(add_err)
    #idcorr <- sapply(lapply(split(x, x$ID), '[[', 'item_correct'), mean)
    #dat <- subset(dat, !ID %in% which(idcorr > .99))
    #dat$ID <- factor(dat$ID)
    dat$room_error <- as.numeric(dat$item_correct1 == 0 & dat$room_correct1 == 1)
    dat$cat_error <- as.numeric(dat$item_correct1 == 0 & dat$cat_correct1 == 1)
    dat <- splam(dat)
    x$room_error <- unname(unlist(lapply(dat, function(z) sapply(z, function(k) mean(k$room_error)))))
    x$cat_error <- unname(unlist(lapply(dat, function(z) sapply(z, function(k) mean(k$cat_error)))))
  }
  if(!isTRUE(getk) & !is.null(rm)){
    if(!identical(rm, FALSE)){
      if(isTRUE(rm)){rm <- .99}
      rm <- unname(which(sapply(lapply(split(x, x$ID), '[[', 'item_correct'), mean) > rm))
      x <- subset(x, !ID %in% rm)
      x$ID <- factor(x$ID)
    }
  }
  if(isTRUE(scalert)){x$rt <- scale(x$rt)}
  if(!any(grepl('forward', colnames(x)))){
    x$time_forward_norm <- normal(ifelse(x$time_pattern < 0, 0, x$time_pattern))
    x$time_backward_norm <- normal(abs(ifelse(x$time_pattern > 0, 0, x$time_pattern)))
    out <- x[, grep('norm', colnames(x))]
    x <- x[, setdiff(colnames(x), c(colnames(out), gsub('_norm', '', colnames(out))))]
    x <- data.frame(x, out)
    colnames(x) <- gsub('_norm', '', colnames(x))
    x <- x[, -grep('^k', colnames(x))]
  }
  dat <- x[, c(grep('pattern', colnames(x))[1:2], grep('ward', colnames(x)))]
  set.seed(seed)
  k <- kmeans(dat, centers = centers, nstart = 25)
  # Edited to change to an order I like
  x$k <- factor(k$cluster)
  if(nrow(x) %in% c(5040, 4860)){
    levels(x$k) <- switch(2 - isTRUE(fix), c(3, 2, 1), c(2, 1, 3))
  } else if(nrow(x) == 5152){
    levels(x$k) <- c(1, 3, 2)
  }
  x$k <- as.numeric(as.character(x$k))
  if(isTRUE(getk)){x <- k}
  return(x)
}
