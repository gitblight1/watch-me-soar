require(tm)
require(RSQLite)
require(data.table)

sqlite_db <- 'ngrams.db'

uniTable <- 'Ngram1'
biTable <- 'Ngram2'
triTable <- 'Ngram3'
fourTable <- 'Ngram4'

get_table <- function(dbName, tableName) {
    db <- dbConnect(SQLite(), dbName)
    query <- sprintf('SELECT * FROM %s', tableName)
    data <- data <- dbGetQuery(db, query)
    dbDisconnect(db)
    data.table(data, key = 'word')
}

predict_next <- function(line, unigram, bigram, trigram, fourgram, defaultAnswer) {
    line <- paste('. . .', line) # make sure line has at least four "words"
    wrds <- tail(unlist(strsplit(line, ' ')), n = 3)
    prediction <- c(fourgram[J(paste(wrds, collapse = ' '))]$pred,
                    trigram[J(paste(wrds[2:4], collapse = ' '))]$pred,
                    #backoff if trigram didn't appear
                    bigram[J(paste(wrds[3:4], collapse = ' '))]$pred,
                    unigram[J(wrds[4])]$pred,
                    #skip-ahead if last word doesn't appear
                    trigram[J(paste(wrds[1:3], collapse = ' '))]$skip,
                    bigram[J(paste(wrds[2:3], collapse = ' '))]$skip,
                    unigram[J(wrds[3])]$skip,
                    bigram[J(paste(wrds[1:2], collapse = ' '))]$skip2,
                    unigram[J(wrds[2])]$skip2,
                    unigram[J(wrds[1])]$skip3,
                    # if nothing shows up, use the default
                    defaultAnswer)
    # The first thing that showed up is the best prediction
    firstGood <-min(which(prediction != '.')) #NA is automatically false
    result <- prediction[firstGood]
    if (result == 'i')
        toupper(result) # Proper nouns are capitalized, amirite?
    else
        result
}

shinyServer(
    function(input, output, session) {
        defaultStart = "I" # most common starting word in the sample
        defaultCont = "and" # most common continuation word in the sample
        # create the main result when the calculate button is clicked.
        unigram <- get_table(sqlite_db, uniTable)
        bigram <- get_table(sqlite_db, biTable)
        trigram <- get_table(sqlite_db, triTable)
        fourgram <- get_table(sqlite_db, fourTable)
        output$result <- renderUI({
            input$predAction
            isolate({
                line <- gsub("[[:punct:]]", "", input$txt)
                if (line == '') {defaultStart}
                else {result <- predict_next(tolower(line),
                                   unigram,
                                   bigram,
                                   trigram,
                                   fourgram,
                                   defaultCont)}
            })
        })
    }
)