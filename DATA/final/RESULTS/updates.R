source('../functions.R')

### Recency effect  - see from original order
dat <- readRDS('../fullData3.RDS')
dat1 <- sorder(dat, 'loc')

r1 <- glmer(item_correct1 ~ condition + time0 + (1|ID) + (1|session) + (1|trial),
            data = dat1, family = binomial)
r2 <- glmer(item_correct1 ~ condition + time0 + time1 + (1|ID) + (1|session) + (1|trial),
            data = dat1, family = binomial)
r3 <- glmer(item_correct1 ~ condition * time1 + time0 + (1|ID) + (1|session) + (1|trial),
            data = dat1, family = binomial)
anova(r3, r2, r1)
recency <- list(r1 = r1, r2 = r2, r3 = r3)
#saveRDS(recency, 'recency.RDS')

r3 <- readRDS('r3.RDS')
plmer(r3$r1, c(.2, 2))
plmer(r3$r2, c(.1, 2))


### Plot dimensions over time
x <- clust(readRDS('../simpleFINAL.RDS'))

pdim <- function(x, subject = NULL, condition = FALSE, time = TRUE, p = NULL){
  if(is.null(p)){p <- c(paste0(c('cat', 'room'), '_pattern'), paste0('time_', c('forward', 'backward')))}
  if(!is.null(subject)){x <- subset(x, ID == subject)}
  if(!all(p %in% colnames(x))){x <- clust(x)}
  dat <- x[, p]
  k <- ifelse(isTRUE(time), length(p), 2)
  if(isTRUE(condition)){
    dat <- data.frame(stack(dat[, 1:k]), condition = rep(x$condition, k),
                      trial = rep(x$trial, k))
    ggplot(dat, aes(x = trial, y = values, color = ind)) +
      geom_line() + facet_wrap(. ~ condition) + theme_bw()
  } else {
    dat <- data.frame(stack(dat[, 1:k]), trial = rep(x$trial, k))
    ggplot(dat, aes(x = trial, y = values, color = ind)) +
      geom_line() + theme_bw()
  }
}

# Plot aggregate time patterns
x <- clust(readRDS('../simpleFINAL.RDS'))
xx <- lapply(split(x, x$ID), function(z) split(z, z$condition))
xx <- setNames(lapply(names(xx$`1`), function(z) lapply(xx, '[[', z)), names(xx$`1`))
xx <- lapply(xx, function(z) lapply(c(10, 11, 13, 14), function(k) lapply(z, '[[', k)))
for(i in 1:5){names(xx[[i]]) <- colnames(x)[c(10, 11, 13, 14)]}
xx <- lapply(xx, function(z) lapply(z, function(k) do.call(cbind, k)))
xx <- lapply(lapply(xx, function(z) lapply(z, rowMeans)), function(r) data.frame(do.call(cbind, r)))
xx <- data.frame(do.call(rbind, setNames(lapply(names(xx), function(z){
  xx[[z]]$condition <- z
  xx[[z]]$trial <- 1:18
  return(xx[[z]])
}), names(xx))))
rownames(xx) <- 1:90
xx$condition <- factor(xx$condition, levels = levels(x$condition))
xx <- data.frame(stack(xx[, 1:4]), condition = rep(xx$condition, 4),
                 trial = rep(xx$trial, 4))


g <- ggplot(xx, aes(x = trial, y = values, col = ind)) +
  geom_line() + facet_wrap(. ~ condition) + theme_bw()


# Model dimensions over time
x <- clust(readRDS('../simpleFINAL.RDS'))
room1 <- lmer(room_pattern ~ trial + (1|ID) + (1|session),
              data = x, REML = FALSE)
room2 <- lmer(room_pattern ~ trial + condition + (1|ID) + (1|session),
              data = x, REML = FALSE)
room3 <- lmer(room_pattern ~ trial * condition + (1|ID) + (1|session),
              data = x, REML = FALSE)
anova(room3, room2, room1)
plmer(room3, c(-.2, .3))


cat1 <- lmer(cat_pattern ~ trial + (1|ID) + (1|session),
              data = x, REML = FALSE)
cat2 <- lmer(cat_pattern ~ trial + condition + (1|ID) + (1|session),
              data = x, REML = FALSE)
cat3 <- lmer(cat_pattern ~ trial * condition + (1|ID) + (1|session),
              data = x, REML = FALSE)
anova(cat3, cat2, cat1)
plmer(cat2, c(-.1, .6))


forward1 <- lmer(time_forward ~ trial + (1|ID) + (1|session),
             data = x, REML = FALSE)
forward2 <- lmer(time_forward ~ trial + condition + (1|ID) + (1|session),
             data = x, REML = FALSE)
forward3 <- lmer(time_forward ~ trial * condition + (1|ID) + (1|session),
             data = x, REML = FALSE)
anova(forward3, forward2, forward1)
plmer(forward2, c(-.1, .4))


backward1 <- lmer(time_backward ~ trial + (1|ID) + (1|session),
                 data = x, REML = FALSE)
backward2 <- lmer(time_backward ~ trial + condition + (1|ID) + (1|session),
                 data = x, REML = FALSE)
backward3 <- lmer(time_backward ~ trial * condition + (1|ID) + (1|session),
                 data = x, REML = FALSE)
anova(backward3, backward2, backward1)
plmer(backward2, c(-.1, .4))


### Reaction time differences by strategy
x <- clust(readRDS('../simpleFINAL.RDS'))
x$k <- factor(x$k)
m1 <- lmer(rt ~ k + (1|ID) + (1|session), data = x, REML = FALSE)
m2 <- lmer(rt ~ k + condition + (1|ID) + (1|session), data = x, REML = FALSE)
m3 <- lmer(rt ~ k + condition + item_correct + (1|ID) + (1|session), data = x, REML = FALSE)
m4 <- lmer(rt ~ k + condition + item_correct + trial + (1|ID) + (1|session), data = x, REML = FALSE)
m5 <- lmer(rt ~ k * condition + item_correct + trial + (1|ID) + (1|session), data = x, REML = FALSE)
anova(m5, m4, m3, m2, m1)

plmer(m4, c(-.3, .1))





### More strategy adherence within conditions over time?
xx <- lapply(split(x, x$condition), function(z) table(z$k, z$session))
yy <- lapply(xx, chisq.test)
out <- data.frame(pvalue = sapply(yy, '[[', 3))


### When do they switch from initial strategy?
y <- clust(readRDS('../practice/simpleFINAL.RDS'))

for(i in 2:90){
  y$session2 <- ifelse(y$trial %in% 1:i, 0, 1)
  tab <- table(y$k, y$session2)
  out <- chisq.test(tab)
  if(out$p.value > .05){
    message(paste0('FINAL VALUE: ', i))
    break
  }
}
change <- i


### Optimal strategy - rank ordering strategies
x <- clust(readRDS('../simpleFINAL.RDS'))
x$optimal1 <- x$optimal2 <- x$condition
levels(x$optimal1) <- c(1, 1, 2, 3, 2)
levels(x$optimal2) <- c(1, 2, 2, 3, 2)

x1 <- lapply(split(x, x$session), function(z) table(z$k, z$optimal1))
y1 <- lapply(x1, chisq.test)
out1 <- data.frame(pvalue = sapply(y1, '[[', 3))
sapply(x1, function(z) sum(diag(z)))


x2 <- lapply(split(x, x$session), function(z) table(z$k, z$optimal2))
y2 <- lapply(x2, chisq.test)
out2 <- data.frame(pvalue = sapply(y2, '[[', 3))
sapply(x2, function(z) sum(diag(z)))


### Nonlinear models of dimensions
library(mgcv)
x <- clust(readRDS('../simpleFINAL.RDS'))
m1 <- gamm(room_pattern ~ s(trial) + condition, data = x, random = list(ID = ~trial), method = 'ML')
x$pred <- predict(m1$gam)
xx <- x[order(x$trial), ]
xx <- subset(xx, condition == 'Control')
plot(xx$trial, xx$room_pattern, ylab = 'Room pattern', xlab = 'Trial', main = '')
lines(xx$trial, xx$pred, lwd = 2, col = 'blue')


################################################################################
### 1) Autoregressive approaches
x <- clust(readRDS('../simpleFINAL.RDS'), rm = TRUE, add_err = '../fullData3.RDS')



