require(tm)
require(RSQLite)
require(data.table)

sqlite_db <- 'ngrams.db'

uniTable <- 'Single'
biTable <- 'Double'
triTable <- 'Triple'

get_table <- function(dbName, tableName) {
    db <- dbConnect(SQLite(), dbName)
    query <- sprintf('SELECT * FROM %s', tableName)
    data <- data <- dbGetQuery(db, query)
    dbDisconnect(db)
    data.table(data)
}

predict_next <- function(line, unigram, bigram, trigram, defaultAnswer) {
    line <- paste('. .', line) # make sure line has at least three "words"
    wrds <- tail(unlist(strsplit(line, ' ')), n = 3)
    prediction <- c(trigram$pred[trigram$word == paste(wrds, collapse = ' ')],
                    #backoff if trigram didn't appear
                    bigram$pred[bigram$word == paste(wrds[2:3], collapse = ' ')],
                    unigram$pred[unigram$word == wrds[3]],
                    #skip-ahead if last word doesn't appear
                    bigram$skip[bigram$word == paste(wrds[1:2], collapse = ' ')],
                    unigram$skip[unigram$word == wrds[2]],
                    unigram$skip2[unigram$word == wrds[1]],
                    # if nothing shows up, use the default
                    defaultAnswer)
    # The first thing that showed up is the best prediction
    prediction[1]
}

shinyServer(
    function(input, output, session) {
        defaultStart = "I" # most common starting word in the sample
        defaultCont = "and" # most common continuation word in the sample
        # create the main result when the calculate button is clicked.
        unigram <- get_table(sqlite_db, uniTable)
        bigram <- get_table(sqlite_db, biTable)
        trigram <- get_table(sqlite_db, triTable)
        output$result <- renderUI({
            input$predAction
            isolate({
                line <- gsub("[[:punct:]]", "", input$txt)
                if (line == '') {defaultStart}
                else {predict_next(line, unigram, bigram, trigram, defaultCont)}
            })
        })
    }
)