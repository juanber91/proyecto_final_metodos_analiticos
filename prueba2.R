rottenrate <- function(movie) {
    require(RJSONIO)
    link <-
        paste(
            "http://www.omdbapi.com/?t=",
            movie,
            "&y=&plot=short&r=json&tomatoes=true",
            sep = ""
        )
    jsonData <- fromJSON(link)
    return(jsonData)
}
vrottenrate <- Vectorize(rottenrate, "movie", SIMPLIFY = F)

val <- "http://www.fandango.com/valkilmer/filmography/p38142"

library('XML')
val_movies <- readHTMLTable(val)
val_movies <- as.data.frame(val_movies)
val_movie_titles <- subset(val_movies, select = c(NULL.Title))
val_movie_titles <- as.character(val_movie_titles$NULL.Title)

val_completed <-
    do.call(rbind, lapply(vrottenrate(val_movie_titles), function(x)
        as.data.frame(t(x), stringsAsFactors = FALSE)))