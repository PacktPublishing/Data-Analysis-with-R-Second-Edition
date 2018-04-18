#!/usr/bin/Rscript --vanilla

###########################################################
##                                                       ##
##   nyc-sat-scores.R                                    ##
##                                                       ##
##                Author: Tony Fischetti                 ##
##                        tony.fischetti@gmail.com       ##
##                                                       ##
###########################################################

##
## Aim: to use Bayesian analysis to compare NYC's 2010 
##      combined SAT scores against the average of the
##      rest of the country, which, according to
##      FairTest.com, is 1509
##

# workspace cleanup
rm(list=ls())

# options
options(echo=TRUE)
options(stringsAsFactors=FALSE)

# libraries
library(assertr)   # for data checking
library(runjags)   # for MCMC

# make sure everything is all set with JAGS
testjags()
# yep!


## read data file
# data was retrieved from NYC Open Data portal
# direct link: https://data.cityofnewyork.us/api/views/zt9s-n5aj/rows.csv?accessType=DOWNLOAD
nyc.sats <- read.csv("./data/SAT_Scores_NYC_2010.csv")

# let's give the columns easier names
better.names <- c("id", "school.name", "n", "read.mean",
                  "math.mean", "write.mean")
names(nyc.sats) <- better.names


# there are 460 rows but almost 700 NYC schools
# we will *assume*, then, that this is a random
# sample of NYC schools

# let's first check the veracity of this data...
#nyc.sats <- assert(nyc.sats, is.numeric,
#                   n, read.mean, math.mean, write.mean)

# It looks like check failed because there are "s"s for some rows. (??)
# A look at the data set descriptions indicates that the "s" is for schools
# with 5 or fewer students. For our purposes, let's just exclude them.


# This is a function that takes a vector, replaces all "s"s
# with NAs and make coverts all non-"s"s into numerics
remove.s <- function(vec){
  ifelse(vec=="s", NA, vec)
}

nyc.sats$n          <- as.numeric(remove.s(nyc.sats$n))
nyc.sats$read.mean  <- as.numeric(remove.s(nyc.sats$read.mean))
nyc.sats$math.mean  <- as.numeric(remove.s(nyc.sats$math.mean))
nyc.sats$write.mean <- as.numeric(remove.s(nyc.sats$write.mean))

# Remove schools with fewer than 5 test takers
nyc.sats <- nyc.sats[complete.cases(nyc.sats), ]

# Calculate a total combined SAT score
nyc.sats$combined.mean <- (nyc.sats$read.mean +
                           nyc.sats$math.mean +
                           nyc.sats$write.mean)

# Let's build a posterior distribution of the true mean
# of NYC high school's combined SAT scores.

# We're not going to look at the summary statistics because
# we don't want to bias our priors

# Specify a standard gaussian model
the.model <- "
model {
  # priors
  mu ~ dunif(0, 2400)
  stddev ~ dunif(0, 500)
  tau <- pow(stddev, -2)

  # likelihood
  for(i in 1:theLength){
     samp[i] ~ dnorm(mu, tau)
  }
}"

the.data <- list(
  samp = nyc.sats$combined.mean,
  theLength = length(nyc.sats$combined.mean)
)

results <- autorun.jags(the.model, data=the.data,
                        n.chains = 3,
                        monitor = c('mu', 'stddev'))

# View the results of the MCMC
print(results)

# Plot the MCMC diagnostics
plot(results, plot.type=c("histogram", "trace"), layout=c(2,1))
# Looks good!

# Let's extract the MCMC samples of the mean and get the
# bounds of the middle 95%
results.matrix <- as.matrix(results$mcmc)
mu.samples <- results.matrix[,'mu']
bounds <- quantile(mu.samples, c(.025, .975))

# We are 95% sure that the true mean is between 1197 and 1232

# Now let's plot the marginal posterior distribution for the mean
# of the NYC high schools' combined SAT grades and draw the 95%
# percent credible interval.
plot(density(mu.samples), main=paste("Posterior distribution of mean combined SAT",
                                     "score in NYC high schools (2010)", sep="\n"))
lines(c(bounds[1], bounds[2]), c(0, 0), lwd=3, col="red")


# Given the results, the SAT scores for NYC high schools in 2010
# are *incontrovertibly* not on par with the average SAT scores of
# the nation.
