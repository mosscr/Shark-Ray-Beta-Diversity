##Load data and packages
load('Data/comm_&_coords.Rdata')
library(vegan)

##Calculate beta diversity and the geographical distance
beta <- vegdist(comm)
gdist <- dist(coords)

##create the different linear models 
  ##Note: the model used is noted under each of the plots
mod <- lm(jac ~ gdist)
mod <- lm(beta ~ gdist)

##Create a simple plot of the geographical distance against the beta diversity 
plot(log10(gdist), beta)
lines(lowess(log10(gdist), beta), col='red')
abline(mod, col='dodgerblue')
        ##mod <- lm(beta ~ gdist)
          ##I don't believe that this is very informative

##Calculate the jaccard distance index
jac <- log(1-vegdist(beta, 'jaccard'))
jac

##plot the jaccard index against the geographical distance
plot(gdist, jac)
abline(mod, col='dodgerblue')
lines(lowess(gdist, jac), col='red')
abline(mod, col='dodgerblue')
      ##mod <- lm(jac ~ gdist)

##Run correlelograms to test if there is correlation between
  #these distance matrices (one for simple community and the other for jaccard)
comm_corlog <- mantel.correlog(beta, gdist)
comm_corlog
plot(comm_corlog)
abline(v = gdist, col='red', lwd=3, lty=2)
     ##black = statistically significant, four out of seven
    #so there may be a significant correlation 

jac_corlog <- mantel.correlog(jac, gdist)
jac_corlog
plot(jac_corlog)
abline(v = gdist, col='red', lwd=3, lty=2)
    ##black = statistically significant, only one, about 5000km 
