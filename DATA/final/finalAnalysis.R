### Memory Museum Pilot Study Analysis
# Trevor James Swanson
# 09/22/2020

### CONDITIONS: (numbering only relevant to original format)
# 1) Space + Semantics + Time
# 2) Space + Time
# 3) Space + Semantics
# 4) Time + Semantics
# 5) As much chaos as possible (control)


### ------------------------------------------------------------------------ ###
### --------------------------- DATA PREPARATION --------------------------- ###
### ------------------------------------------------------------------------ ###
if(!require(ggplot2)){install.packages('ggplot2', dependencies = TRUE)}
if(!require(lme4)){install.packages('lme4', dependencies = TRUE)} # v1.1-19
if(!require(lsmeans)){install.packages('lsmeans', dependencies = TRUE)}
if(!require(effects)){install.packages('effects', dependencies = TRUE)}
source('extractData.R')
out <- getData(getJSONS(), matchPs = TRUE)
sorts <- lapply(out, lapply, getSorts)
inits <- data.frame(do.call(rbind, lapply(
  lapply(out, lapply, getInits, rmPractice = FALSE), function(z){
    data.frame(do.call(rbind, z))
  })
))

if(nrow(inits)/(9 * 56) == 90){
  sorts$dat1 <- lapply(sorts$dat1, function(z){
    z1 <- z[-(1:18), ]
    z1$trial <- z1$trial - 2
    return(z1)
  })
}

simple <- lapply(sorts, function(z){
  k <- seq(1, unique(sapply(z, nrow)), by = 9)
  lapply(z, function(i) i[k, 1:3])
})

### Create datasets:
# Full data: all items, all IDs, all trials, all conditions
# Simple data: all IDs, all trials, all conditions

for(i in 1:3){
  for(j in 1:length(sorts[[i]])){
    sorts[[i]][[j]] <- cbind.data.frame(ID = j, sorts[[i]][[j]])
    simple[[i]][[j]] <- cbind.data.frame(ID = j, simple[[i]][[j]])
  }
  sorts[[i]] <- data.frame(do.call(rbind, sorts[[i]]))
  simple[[i]] <- data.frame(do.call(rbind, simple[[i]]))
}

sorts$dat2$trial <- sorts$dat2$trial + max(sorts$dat1$trial)
simple$dat2$trial <- simple$dat2$trial + max(sorts$dat1$trial)
sorts$dat3$trial <- sorts$dat3$trial + max(sorts$dat2$trial)
simple$dat3$trial <- simple$dat3$trial + max(sorts$dat2$trial)

for(i in 1:3){
  sorts[[i]] <- data.frame(ID = sorts[[i]]$ID, session = i, sorts[[i]][, -1])
  simple[[i]] <- data.frame(ID = simple[[i]]$ID, session = i, simple[[i]][, -1])
}

sorts <- data.frame(do.call(rbind, sorts))
simple <- data.frame(do.call(rbind, simple))

for(i in c('ID', 'session', 'condition')){
  sorts[[i]] <- factor(sorts[[i]])
  simple[[i]] <- factor(simple[[i]])
}

conditions <- c('SpaceSemTime', 'SpaceTime', 'SpaceSem', 'TimeSem', 'Control')
levels(sorts$condition) <- rev(levels(sorts$condition))
sorts$condition <- as.numeric(as.character(sorts$condition))
sorts$condition <- factor(sorts$condition)
levels(sorts$condition) <- rev(conditions)

levels(simple$condition) <- rev(levels(simple$condition))
simple$condition <- as.numeric(as.character(simple$condition))
simple$condition <- factor(simple$condition)
levels(simple$condition) <- rev(conditions)

sorts$correct <- sorts$correct/9
simple$correct <- simple$correct/9

rownames(sorts) <- 1:nrow(sorts)
rownames(simple) <- 1:nrow(simple)
sorts <- cbind(sorts, inits)
# write.csv(sorts, 'fullData.csv', row.names = FALSE)
# write.csv(simple, 'simpleData.csv', row.names = FALSE)

sorts <- data.frame(X = 1:nrow(sorts), sorts)
#sorts$item1 <- apply(sorts[, c('cat1', 'src1')], 1, paste0, collapse = '.')
#sorts$item0 <- apply(sorts[, c('cat0', 'src0')], 1, paste0, collapse = '.')
### Coding Error Patterns
ids <- split(sorts, sorts$ID)
ids <- lapply(ids, function(z) split(z, z$trial))
newdat <- data.frame(matrix(NA, nrow = nrow(simple), ncol = 9))
n <- 1

for(i in seq_along(ids)){
  for(j in seq_along(ids[[i]])){
    k <- ids[[i]][[j]]

    s1 <- apply(k[, grep('1$', colnames(k))[1:4]], 1, function(zz) paste0(zz, collapse = '.'))
    s2 <- apply(k[, grep('0$', colnames(k))[1:4]], 1, function(zz) paste0(zz, collapse = '.'))
    
    s3 <- apply(k[, c('cat1', 'src1', 'room1')], 1, function(zz) paste0(zz, collapse = '.'))
    s4 <- apply(k[, c('cat0', 'src0', 'room0')], 1, function(zz) paste0(zz, collapse = '.'))
    
    s5 <- apply(k[, c('cat1', 'loc1')], 1, function(zz) paste0(zz, collapse = '.'))
    s6 <- apply(k[, c('cat0', 'loc0')], 1, function(zz) paste0(zz, collapse = '.'))
    
    s7 <- apply(k[, c('cat1', 'room1')], 1, function(zz) paste0(zz, collapse = '.'))
    s8 <- apply(k[, c('cat0', 'room0')], 1, function(zz) paste0(zz, collapse = '.'))
    
    ids[[i]][[j]]$item_correct <- as.numeric(s1 %in% s2)
    ids[[i]][[j]]$time_correct <- as.numeric(s1 == s2)
    ids[[i]][[j]]$room_correct <- as.numeric(s3 %in% s4)
    ids[[i]][[j]]$cat_correct <- as.numeric(s5 %in% s6)
    ids[[i]][[j]]$cat_room_correct <- as.numeric(s7 %in% s8)
    
    newdat[n, 1:4] <- ids[[i]][[j]][1, colnames(simple)[1:4]]
    newdat[n, 5] <- sum(ids[[i]][[j]]$item_correct)/9
    newdat[n, 6] <- sum(ids[[i]][[j]]$time_correct)/9
    newdat[n, 7] <- sum(ids[[i]][[j]]$room_correct)/9
    newdat[n, 8] <- sum(ids[[i]][[j]]$cat_correct)/9
    newdat[n, 9] <- sum(ids[[i]][[j]]$cat_room_correct)/9
    n <- n + 1
  }
}

ids <- data.frame(do.call(rbind, lapply(ids, function(z) data.frame(do.call(rbind, z)))))
row.names(ids) <- 1:nrow(ids)
colnames(newdat) <- c(colnames(simple)[1:4], colnames(ids)[17:21])
for(i in c(1, 2, 4)){newdat[, i] <- factor(newdat[, i])}
levels(newdat$condition) <- levels(simple$condition)
#saveRDS(ids[, -1], 'fullData2.RDS')
#saveRDS(newdat, 'simple2.RDS')

### PLOTS
sess <- lapply(1:3, function(z) subset(simple, session == z))
par(mfrow = c(2, 2))
for(i in 1:3){hist(sess[[i]]$correct, main = '', xlab = paste('Session', i))}
idcorr <- sapply(lapply(split(simple, simple$ID), '[[', 'correct'), mean)
plot(sort(idcorr), type = 'b', ylab = 'Average Overall Performance', xlab = '')


# Altogether
dev.off()
ggplot(simple, aes(x = trial, y = correct, color = factor(ID))) +
  geom_line() + geom_point() + theme_bw()

# By Condition
dev.off()
ggplot(simple, aes(x = trial, y = correct, color = factor(ID))) +
  geom_line() + geom_point() + theme_bw() + facet_wrap(~ condition)

# By Session + Condition
simple2 <- split(simple, simple$session)
simple2$`2`$trial <- rep(1:30, 56)
simple2$`3`$trial <- rep(1:30, 56)
simple2 <- data.frame(do.call(rbind, simple2))
levels(simple2$session) <- c('SESSION 1', 'SESSION 2', 'SESSION 3')

dev.off()
ggplot(simple2, aes(x = trial, y = correct, color = factor(ID))) +
  geom_line() + geom_point() + theme_bw() + facet_wrap(~ session + condition)


### ------------------------------------------------------------------------ ###
### ------------------------------- ANALYSES ------------------------------- ###
### ------------------------------------------------------------------------ ###

### Analysis 1: Simple dataset
m0 <- lmer(correct ~ condition + (1|ID) + (1|session),
           data = simple, REML = FALSE)
m1 <- lmer(correct ~ trial + (1|ID) + (1|session),
           data = simple, REML = FALSE)
m2 <- lmer(correct ~ condition + trial + (1|ID) + (1|session),
           data = simple, REML = FALSE)
m3 <- lmer(correct ~ condition * trial + (1|ID) + (1|session), 
           data = simple, REML = FALSE)

anova(m0, m2) # Is condition necessary, or only trial?
anova(m1, m2) # Is trial necessary, or only condition?
anova(m2, m3) # Are there interactions between trial and condition?

lsmeans(m2, pairwise ~ condition, adjust = 'tukey')

simple2 <- simple
levels(simple2$condition) <- 1:5
m2 <- lmer(correct ~ condition + trial + (1|ID) + (1|session),
           data = simple2, REML = FALSE)

plot(Effect(c('condition', 'trial'), m2))
plot(Effect(c('trial', 'condition'), m2))


### ------------------------------------------------------------------------ ###
### ------------------------------- ANALYSIS 2 ----------------------------- ###
### ------------------------------------------------------------------------ ###

time <- lmer(time_correct ~ condition + trial + (1|ID) + (1|session),
             data = newdat, REML = FALSE)
lsmeans(time, pairwise ~ condition, adjust = 'tukey')

plot(Effect(c('trial', 'condition'), time))

room <- lmer(room_correct ~ condition + trial + (1|ID) + (1|session),
             data = newdat, REML = FALSE)
lsmeans(room, pairwise ~ condition, adjust = 'tukey')

catcor <- lmer(cat_correct ~ condition + trial + (1|ID) + (1|session),
               data = newdat, REML = FALSE)
lsmeans(catcor, pairwise ~ condition, adjust = 'tukey')


recency1 <- glmer(item_correct ~ condition + time1 + 
                    (1|ID) + (1|session) + (1|trial), 
                  data = ids, family = binomial)
recency2 <- glmer(item_correct ~ condition * time1 + 
                    (1|ID) + (1|session) + (1|trial), 
                  data = ids, family = binomial)

anova(recency1, recency2)


# PULL REACTION TIME; individual differences; check the superstars
# When do people use strategies? Moment of insight into cognitive map
# Putting items back in reverse order
# First and last items displayed -- are these tracked differently?
# Chance 1/9

### REACTION TIME
splam <- function(x){
  if(is(x, 'list')){
    x <- data.frame(do.call(rbind, lapply(x, function(z) data.frame(do.call(rbind, z)))))
    rownames(x) <- 1:nrow(x)
  } else {
    x <- setNames(lapply(split(x, x$ID), function(z) setNames(split(z, z$trial), paste0('t', 1:90))), paste0('p', 1:56))
    if(all(sapply(x, sapply, nrow) == 1)){x <- lapply(x, function(z) structure(do.call(rbind, z), row.names = 1:90))}
  }
  return(x)
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
newdat$rt <- rts$rt
#saveRDS(newdat, 'simple2.RDS')
newdat$rt <- scale(newdat$rt)

cor(newdat[, grep('^rt$|_correct$', colnames(newdat))])

scores <- sapply(splam(newdat), function(z) mean(z$item_correct))
rts <- sapply(splam(newdat), function(z) mean(z$rt))
cor.test(scores, rts)



r0 <- lmer(item_correct ~ condition + trial + (1|ID) + (1|session),
           data = newdat, REML = FALSE)
r1 <- lmer(item_correct ~ condition + trial + rt + (1|ID) + (1|session),
           data = newdat, REML = FALSE)

anova(r0, r1) # Shows that reaction time adds information about performance

r2 <- lmer(item_correct ~ condition + trial * rt + (1|ID) + (1|session),
           data = newdat, REML = FALSE)
r3 <- lmer(item_correct ~ condition * rt + trial + (1|ID) + (1|session),
           data = newdat, REML = FALSE)
r4 <- lmer(item_correct ~ condition * rt * trial + (1|ID) + (1|session),
           data = newdat, REML = FALSE)

anova(r1, r2) # Significant interaction between trial and reaction time
anova(r1, r3) # No interaction between condition and trial
anova(r2, r4) # No three-way interaction


