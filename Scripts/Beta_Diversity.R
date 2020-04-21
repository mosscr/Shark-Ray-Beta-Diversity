load('Data/comm_&_coords.Rdata')
library(vegan)
library(nlme)
plot(coords, cex = comm[, 2])

beta <- vegdist(comm)
gdist <- dist(coords)

mod <- lm(jac ~ gdist)
mod <- lm(beta ~ gdist)

plot(log10(gdist), beta)
lines(lowess(log10(gdist), beta), col='red')
abline(mod, col='dodgerblue')
        ##mod <- lm(beta ~ gdist)

jac <- log(1-vegdist(beta, 'jaccard'))
jac

##play with plotting, mantel correlolgrams
plot(gdist, jac)
abline(mod, col='dodgerblue')
lines(lowess(gdist, jac), col='red')
abline(mod, col='dodgerblue')
      ##mod <- lm(jac ~ gdist)

comm_corlog <- mantel.correlog(beta, gdist)
comm_corlog
plot(comm_corlog)
abline(v = gdist, col='red', lwd=3, lty=2)

jac_corlog <- mantel.correlog(jac, gdist)
jac_corlog
plot(jac_corlog)
abline(v = gdist, col='red', lwd=3, lty=2)
