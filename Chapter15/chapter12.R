# You don't have to install this if you copy of R is new
library(parallel)

detectCores()

system.time(
  lapply(1:4, function(i) Sys.sleep(5))
)

#######################
# NON-WINDOWS VERSION #
#######################
system.time(
  mclapply(1:4, function(i) Sys.sleep(5), mc.cores=4)
)

###################
# WINDOWS VERSION #
###################
#system.time(
#  parLapply(cl, 1:4, function(i) Sys.sleep(5))
#)

# ALL OUTPUT SHOWN WILL BE FOR A
# NON-WINDOWS COMPUTER WITH 4 CORES


haversine <- function(lat1, long1, lat2, long2, unit="km"){
  radius <- 6378      # radius of Earth in kilometers
  delta.phi <- to.radians(lat2 - lat1)
  delta.lambda <- to.radians(long2 - long1)
  phi1 <- to.radians(lat1)
  phi2 <- to.radians(lat2)
  term1 <- sin(delta.phi/2) ^ 2
  term2 <- cos(phi1) * cos(phi2) * sin(delta.lambda/2) ^ 2
  the.terms <- term1 + term2
  delta.sigma <- 2 * atan2(sqrt(the.terms), sqrt(1-the.terms))
  distance <- radius * delta.sigma
  if(unit=="km") return(distance)
  if(unit=="miles") return(0.621371*distance)
}

to.radians <- function(degrees){
  degrees * pi / 180
}




set.seed(1)

all.airport.locs <- read.csv("http://opendata.socrata.com/api/views/rxrh-4cxm/rows.csv?accessType=DOWNLOAD",
                             stringsAsFactors=FALSE)

library(magrittr)
library(assertr)
CHECKS <- . %>%
  verify(nrow(.) == 13429) %>%
  verify(names(.) %in% c("locationID", "Latitude", "Longitude")) %>%
  assert(within_bounds(0, 90), Latitude) %>%
  assert(within_bounds(0,180), Longitude)

all.airport.locs <- CHECKS(all.airport.locs)

# Let's start off with 400 airports
smp.size <- 400

# choose a random sample of airports
random.sample <- sample((1:nrow(all.airport.locs)), smp.size)
airport.locs <- all.airport.locs[random.sample, ]
row.names(airport.locs) <- NULL

head(airport.locs)


single.core <- function(airport.locs){
  running.sum <- 0
  for(i in 1:(nrow(airport.locs)-1)){
    for(j in (i+1):nrow(airport.locs)){
      # i is the row of the first lat/long pair
      # j is the row of the second lat/long pair
      this.dist <- haversine(airport.locs[i, 2],
                             airport.locs[i, 3],
                             airport.locs[j, 2],
                             airport.locs[j, 3])
      running.sum <- running.sum + this.dist
    }
  }
  # Now we have to divide by the number of
  # distances we took. This is given by
  return(running.sum /
           ((nrow(airport.locs)*(nrow(airport.locs)-1))/2))
}

system.time(ave.dist <- single.core(airport.locs))
print(ave.dist)


combn(1:10, 2)[,1:11]


small.world <- c("LAX", "ALB", "OLM", "JFK")
all.combs <- combn(1:length(small.world), 2)

for(i in 1:ncol(all.combs)){
  from <- small.world[all.combs[1, i]]
  to <- small.world[all.combs[2, i]]
  print(paste(from, " <-> ", to))
}


small.world <- c("LAX", "ALB", "OLM", "JFK")
all.combs <- combn(1:length(small.world), 2)

# instead of printing each airport pair in a string,
# we'll return the string
results <- lapply(1:ncol(all.combs), function(x){
  from <- small.world[all.combs[1, x]]
  to <- small.world[all.combs[2, x]]
  #from <- small.world[all.combs[i, 1]]
  #to <- small.world[all.combs[i, 2]]
  return(paste(from, " <-> ", to))
})

print(results)

unlist(results)

single.core.lapply <- function(airport.locs){
  all.combs <- combn(1:nrow(airport.locs), 2)
  numcombs <- ncol(all.combs)
  results <- lapply(1:numcombs, function(x){
    lat1  <- airport.locs[all.combs[1, x], 2]
    long1 <- airport.locs[all.combs[1, x], 3]
    lat2  <- airport.locs[all.combs[2, x], 2]
    long2 <- airport.locs[all.combs[2, x], 3]
    return(haversine(lat1, long1, lat2, long2))
  })
  return(sum(unlist(results)) / numcombs)
}

system.time(ave.dist <- single.core.lapply(airport.locs))
print(ave.dist)


#######################
# NON-WINDOWS VERSION #
#######################
multi.core <- function(airport.locs){
  all.combs <- combn(1:nrow(airport.locs), 2)
  numcombs <- ncol(all.combs)
  results <- mclapply(1:numcombs, function(x){
    lat1  <- airport.locs[all.combs[1, x], 2]
    long1 <- airport.locs[all.combs[1, x], 3]
    lat2  <- airport.locs[all.combs[2, x], 2]
    long2 <- airport.locs[all.combs[2, x], 3]
    return(haversine(lat1, long1, lat2, long2))
  }, mc.cores=4)
  return(sum(unlist(results)) / numcombs)
}

###################
# WINDOWS VERSION #
###################
#clusterExport(cl, c("haversine", "to.radians"))
#
#multi.core <- function(airport.locs){
#  all.combs <- combn(1:nrow(airport.locs), 2)
#  numcombs <- ncol(all.combs)
#  results <- parLapply(cl, 1:numcombs, function(x){
#      lat1  <- airport.locs[all.combs[1, x], 2]
#      long1 <- airport.locs[all.combs[1, x], 3]
#      lat2  <- airport.locs[all.combs[2, x], 2]
#      long2 <- airport.locs[all.combs[2, x], 3]
#      return(haversine(lat1, long1, lat2, long2))
#  })
#  return(sum(unlist(results)) / numcombs)
#}

system.time(ave.dist <- multi.core(airport.locs))
print(ave.dist)

library(Rcpp)

sourceCpp("our_cpp_functions.cpp")

square(3)


sourceCpp("our_cpp_functions.cpp")
to_radians_cpp(10)


sourceCpp("our_cpp_functions.cpp")

haversine(51.88, 176.65, 56.94, 154.18)
haversine_cpp(51.88, 176.65, 56.94, 154.18)


sourceCpp("our_cpp_functions.cpp")

system.time(ave.dist <- single_core_cpp(as.matrix(airport.locs[,-1])))
print(ave.dist)



the.matrix <- as.matrix(all.airport.locs[,-1])
system.time(ave.dist <- single_core_cpp(the.matrix))
print(ave.dist)




single.core.improved <- function(airport.locs){
  
  numrows <- nrow(airport.locs)
  
  running.sum <- 0
  for (i in 1:(numrows-1)) {
    this.dist <- sum(haversine(airport.locs[i,2],
                               airport.locs[i, 3],
                               airport.locs[(i+1):numrows, 2],
                               airport.locs[(i+1):numrows, 3]))
    running.sum <- running.sum + this.dist
  }
  return(running.sum / (numrows*(numrows-1)/2))
}

system.time(ave.dist <- single.core.improved(all.airport.locs))
print(ave.dist)

