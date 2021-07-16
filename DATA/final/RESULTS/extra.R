---
title: "Extras"
#author: "Trevor James Swanson"
date: "March 10, 2021"
output: html_document
---

```{r lme0, eval = FALSE}
# Test for one random effect
m0 <- lm(item_correct ~ condition, data = x)
m1 <- lmer(item_correct ~ condition + (1|ID), data = x, REML = FALSE)
m2 <- lmer(item_correct ~ condition + (1|ID) + (1|session), data = x, REML = FALSE)

RLRsim::exactLRT(m1, m0)

# Test whether adding a random effect is good; refit with REML
m2 <- update(m2, REML = TRUE)
m1 <- update(m1, REML = TRUE)
msess <- update(m2, . ~ . - (1|ID))
RLRsim::exactRLRT(m = msess, mA = m2, m0 = m1)

# Alternative: fit reduced model and repeatedly simulate from it to compute the differences between the deviation of the full and reduced model for each simualted dataset. Compare the null distribution to the observed deviance difference
pb <- pbkrtest::PBmodcomp(m2, m1, nsim = 1000, seed = 1)
```

```{r anova5, eval = FALSE}
anova(m0, m2) # Is condition necessary, or only trial?
anova(m1, m2) # Is trial necessary, or only condition?
anova(m2, m3) # Are there interactions between trial and condition?

lsmeans(m2, pairwise ~ condition, adjust = 'tukey')

p2 <- profile(m2)
lattice::xyplot(p2, aspect = 1.3)
confint(p2)
splom(p2)
```

```{r lme2, eval = FALSE}
x2 <- x
levels(x2$condition) <- 1:5
m2 <- lmer(item_correct ~ condition + trial + (1|ID) + (1|session),
           data = x2, REML = FALSE)

plot(Effect(c('condition', 'trial'), m2))
plot(Effect(c('trial', 'condition'), m2))
```
```{r recency1, eval = FALSE}
dat <- readRDS('../fullData3.RDS')
recency1 <- glmer(item_correct1 ~ condition + time1 + 
                    (1|ID) + (1|session) + (1|trial), 
                  data = dat, family = binomial)
recency2 <- glmer(item_correct1 ~ condition * time1 + 
                    (1|ID) + (1|session) + (1|trial), 
                  data = dat, family = binomial)
```

```{r rt0, eval = FALSE}
x$rt2 <- scale(x$rt)
cor(x[, grep('^rt2$|_correct$', colnames(x))])

scores <- sapply(splam(x), function(z) mean(z$item_correct))
rts <- sapply(splam(x), function(z) mean(z$rt2))
cor.test(scores, rts)

r0 <- lmer(item_correct ~ condition + trial + (1|ID) + (1|session),
           data = x, REML = FALSE)
r1 <- lmer(item_correct ~ condition + trial + rt2 + (1|ID) + (1|session),
           data = x, REML = FALSE)

anova(r0, r1) # Shows that reaction time adds information about performance

r2 <- lmer(item_correct ~ condition + trial * rt2 + (1|ID) + (1|session),
           data = x, REML = FALSE)
r3 <- lmer(item_correct ~ condition * rt2 + trial + (1|ID) + (1|session),
           data = x, REML = FALSE)
r4 <- lmer(item_correct ~ condition * rt2 * trial + (1|ID) + (1|session),
           data = x, REML = FALSE)

anova(r1, r2) # Significant interaction between trial and reaction time
anova(r1, r3) # No interaction between condition and trial
anova(r2, r4) # No three-way interaction
```


