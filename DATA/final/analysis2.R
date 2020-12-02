### Memory Museum Pilot Study Analysis - PART 2
# Trevor James Swanson
# 11/30/2020

### FUNCTIONS
splam <- function(x){
  if(is(x, 'list')){
    x <- data.frame(do.call(rbind, lapply(x, function(z) data.frame(do.call(rbind, z)))))
    rownames(x) <- 1:nrow(x)
  } else {
    x <- lapply(split(x, x$ID), function(z) split(z, z$trial))
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
  type <- match.arg(type)
  z <- c('ID', 'session', 'trial', 'condition')
  z1 <- colnames(x)[endsWith(colnames(x), '1')]
  z0 <- colnames(x)[endsWith(colnames(x), '0')]
  splam(lapply(splam(x), function(j){
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
xx <- structure(x[seq(1, nrow(x), by = 9), -grep('1$|0$', colnames(x))], row.names = 1:5040)


### REACTION TIME
source('extractData.R')
out <- getData(getJSONS(), matchPs = TRUE)
rts <- lapply(out, function(z){
  z1 <- sapply(z, function(k){
    k1 <- as.numeric(subset(k, stimulus == 'sort_trial')[, 'rt'])
    if(length(k1) == 32){k1 <- k1[-(1:2)]}
    return(k1)
  })
  data.frame(ID = rep(1:56, each = 30), 
             trial = rep(1:30, 56), 
             rt = c(z1))
})
rts$dat2$trial <- rts$dat2$trial + 30
rts$dat3$trial <- rts$dat3$trial + 60
rts <- data.frame(do.call(rbind, rts))
rts <- structure(rts[order(rts$ID), ], row.names = 1:5040)
rts$ID <- factor(rts$ID)

xx$rt <- rts$rt
saveRDS(xx, 'simple3.RDS')


### ANALYSIS
x <- readRDS('fullData3.RDS')


