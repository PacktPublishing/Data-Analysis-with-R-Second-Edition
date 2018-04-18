#!/usr/bin/Rscript --vanilla

###########################################################
##                                                       ##
##   music-recommendations.R                             ##
##                                                       ##
##                Author: Tony Fischetti                 ##
##                        tony.fischetti@gmail.com       ##
##                                                       ##
###########################################################

# workspace cleanup
rm(list=ls())

# options
options(echo=TRUE)
options(stringsAsFactors=FALSE)

# cli args
args <- commandArgs(trailingOnly=TRUE)

# libraries
library(magrittr)
library(assertr)
library(XML)



# takes a URL that produces XML http result
# and returns parsed XML object
get_xml_response <- function(a_url){
  return(xmlParse(a_url))
}


# takes a artist name and creates a url that
# can be used to query music brainz for the artist
create_artist_query_url <- function(artist){
  encoded_artist <- URLencode(artist)
  return(paste0("http://musicbrainz.org/ws/2/artist/?query=artist:",
                encoded_artist))
}


# takes the xml response from a music brainz query, and
# the place of the artist in the artist list (usually 1)
# and xml namespace
# returns a vector of the artist tags
get_tag_vector <- function(xml, place, ns){
  xpath <- paste0("//ns:artist[", place, "]/ns:tag-list/ns:tag/ns:name")
  the_nodes <- getNodeSet(xml, xpath, ns)
  return(unlist(lapply(the_nodes, xmlValue)))
}


# combines all steps
get_artists_tags <- function(artist, ns){
  the_query <- create_artist_query_url(artist)
  xml_resp <- get_xml_response(the_query)
  tag_vector <- get_tag_vector(xml_resp, 1, ns)
  return(tag_vector)
}


jaccard <- function(one, two){
  length(intersect(one, two))/length(union(one, two))
}

make_similarity_matrix <- function(artist_list, similarity_fn) {
    num <- length(artist_list)
    sim_matrix <- matrix(0, ncol = num, nrow = num)
    rownames(sim_matrix) <- names(artist_list)
    colnames(sim_matrix) <- names(artist_list)
    for(i in 1:nrow(sim_matrix)) {
        for(j in 1:ncol(sim_matrix)) {
            sim_matrix[i,j] <- round(
              similarity_fn(artist_list[[i]], artist_list[[j]]), 2)
      }
    }
    return(sim_matrix)
}





ns <- "http://musicbrainz.org/ns/mmd-2.0#"
names(ns)[1] <- "ns"



kate.bush <- get_artists_tags("Kate Bush", ns)
peter.tosh <- get_artists_tags("Peter Tosh", ns)
radiohead <- get_artists_tags("Radiohead", ns)
the.smiths <- get_artists_tags("The Smiths", ns)
the.cure <- get_artists_tags("The Cure", ns)
black.uhuru <- get_artists_tags("Black Uhuru", ns)


the.list <- list(kate.bush, peter.tosh, radiohead,
                 the.smiths, the.cure, black.uhuru)
names(the.list) <- c("kate bush", "peter tosh", "radiohead",
                     "the smiths", "the cure", "black uhuru")

(make_similarity_matrix(the.list, jaccard))


#--------------------------------------------------#
#--------------------------------------------------#
#--------------------------------------------------#


library(jsonlite)


create_artist_query_url_lfm <- function(artist){
  encoded_artist <- URLencode(artist)
  return(paste0("http://ws.audioscrobbler.com/2.0/?method=",
                "artist.gettoptags&artist=",
                encoded_artist, "&api_key=c2e57923a25c03f3d8b31",
                "7b3c8622b43&format=json"))
}

get_json_response <- function(a_url){
  return(fromJSON(a_url))
}

get_tag_vector_lfm <- function(json){
  return(json$toptags$tag$name)
}

get_artists_tags_lfm <- function(artist){
  the_query <- create_artist_query_url_lfm(artist)
  json_resp <- get_json_response(the_query)
  print(json_resp)
  tag_vector <- get_tag_vector_lfm(json_resp)
  return(tag_vector)
}


get_artists_tags_lfm("Kate Bush")



our_artists <- list("Kate Bush", "Peter Tosh", "Radiohead",
                     "The Smiths", "The Cure", "Black Uhuru")
our_artists_tags <- lapply(our_artists, get_artists_tags_lfm)
names(our_artists_tags) <- our_artists


jaccard <- function(one, two){
  length(intersect(one, two))/length(union(one, two))
}

make_similarity_matrix <- function(artist_list, similarity_fn) {
    num <- length(artist_list)
    sim_matrix <- matrix(0, ncol = num, nrow = num)
    rownames(sim_matrix) <- names(artist_list)
    colnames(sim_matrix) <- names(artist_list)
    for(i in 1:nrow(sim_matrix)) {
        for(j in 1:ncol(sim_matrix)) {
            sim_matrix[i,j] <- round(
              similarity_fn(artist_list[[i]], artist_list[[j]]), 2)
      }
    }
    return(sim_matrix)
}



(make_similarity_matrix(our_artists_tags, jaccard))



