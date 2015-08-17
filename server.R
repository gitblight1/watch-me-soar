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
    data.table(data, key = 'word')
}

predict_next <- function(line, unigram, bigram, trigram, defaultAnswer) {
    line <- paste('. .', line) # make sure line has at least three "words"
    wrds <- tail(unlist(strsplit(line, ' ')), n = 3)
    prediction <- c(trigram[J(paste(wrds, collapse = ' '))]$pred,
                    #backoff if trigram didn't appear
                    bigram[J(paste(wrds[2:3], collapse = ' '))]$pred,
                    unigram[J(wrds[3])]$pred,
                    #skip-ahead if last word doesn't appear
                    bigram[J(paste(wrds[1:2], collapse = ' '))]$skip,
                    unigram[J(wrds[2])]$skip,
                    unigram[J(wrds[1])]$skip2,
                    # if nothing shows up, use the default
                    defaultAnswer)
    # The first thing that showed up is the best prediction
    firstNonNA <-min(which(!is.na(prediction)))  
    prediction[firstNonNA]
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
                else {predict_next(tolower(line),
                                   unigram,
                                   bigram,
                                   trigram,
                                   defaultCont)}
            })
        })
    }
)