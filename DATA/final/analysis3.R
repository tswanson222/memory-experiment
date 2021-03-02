x <- readRDS('simpleFINAL.RDS')
k <- colnames(x)

v <- lapply(split(x, x$ID), function(z) z[, c(1:2, 4, 10:15)])

v1 <- data.frame(do.call(rbind, lapply(v, function(z){
  z1 <- t(data.frame(colMeans(z[, grep('pattern', colnames(z))])))
  return(data.frame(ID = unique(z$ID), z1))
})))

v2 <- structure(data.frame(do.call(rbind, lapply(v, function(z){
  z1 <- split(z, z$condition)
  z2 <- suppressWarnings(data.frame(do.call(rbind, lapply(z1, function(i){
    t(data.frame(colMeans(i[, grep('pattern', colnames(i))])))
  }))))
  z3 <- data.frame(ID = unique(z$ID), condition = names(z1), z2)
  return(z3)
}))), row.names = 1:280)

v3 <- structure(data.frame(do.call(rbind, lapply(v, function(z){
  z1 <- split(z, z$condition)
  z2 <- lapply(z1, function(i) split(i, i$session))
  z3 <- data.frame(do.call(rbind, lapply(z2, function(i) data.frame(do.call(rbind, lapply(i, function(j){
    data.frame(ID = unique(j$ID), session = unique(j$session), 
               condition = unique(j$condition), 
               t(data.frame(colMeans(j[, grep('pattern', colnames(j))]))))
  }))))))
  z3 <- z3[order(z3$session), ]
  return(z3)
}))), row.names = 1:840)



