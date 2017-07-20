library(shiny)
library(dplyr)
library(ggplot2)
library(ggthemes)

theme_set(theme_fivethirtyeight())

ui <- fluidPage(
  titlePanel("indivisualize"),
  sidebarLayout(
    sidebarPanel(
      selectInput("subject", label = "Visualization Subject", 
                  choices = list("Aida Ylanan" = "Aida-Ylanan",
                                 "Zack Chauvin" = "zack_chauvin",
                                 "Fernanda Palacios" = "fernanda_palacios",
                                 "Kush Tekriwal" = "kush_tekriwal"), 
                  selected = "Aida-Ylanan"),
      checkboxGroupInput("sources", label = "Input Sources", 
                         choices = list("Facebook" = "facebook", "Gmail" = "gmail", "Youtube" = "youtube"),
                         selected = "facebook")
    ),
    mainPanel(
      textOutput("book"),
      hr(),
      plotOutput("common_words"),
      hr(),
      plotOutput("sentiment"),
      hr(),
      plotOutput("tfidf"),
      hr()
    )
  )
)

server <- function(input, output) {
  get_data_source <- reactive({
    sources <- lapply(input$sources, function(data_source) {
      readRDS(paste0("data/", input$subject, "-", data_source, ".RData"))
    })
    do.call(rbind, sources)
  })
  
  output$book <- renderText({
    data_source <- get_data_source()
    if (!is.null(data_source)) {
      num_words <- nrow(get_data_source())
      paste("Woah! For these sources, you've written",
            num_words,
            "words. That's the equivalent of",
            round(num_words / 40000, digits = 2),
            "novels!")
    } else {
      "Please select an input source."
    }
    
  })
  
  output$common_words <- renderPlot({
    data_source <- get_data_source()
    if (!is.null(data_source)) {
      get_data_source() %>%
        count(word, sort = TRUE) %>%
        top_n(20) %>%
        #
        # Change the order of factor levels.
        #
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(word, n)) +
        geom_col() +
        coord_flip() +
        labs(x = NULL, y = "Count", title = "Your Most Common Words")
      
    }
  })
  output$sentiment <- renderPlot({
    data_source <- get_data_source()
    if (!is.null(data_source)) {
      data_source %>% inner_join(get_sentiments("bing")) %>%
        count(year, sentiment) %>%
        spread(sentiment, n, fill = 0) %>%
        mutate(sentiment = positive - negative, valence = sentiment > 0) %>% 
        ggplot(aes(x = factor(year), y = sentiment, fill = valence)) +
        geom_bar(stat = "identity") +
        labs(x = "Year", y = "Sentiment", title = "Your Sentiment Through the Years") +
        theme(legend.position = "none")
    }
  })
  output$tfidf <- renderPlot({
    data_source <- get_data_source()
    if (!is.null(data_source)) {
      data_source.counts <- data_source %>% count(year, word, sort = TRUE)
      data_source.year_counts <- data_source.counts %>% group_by(year) %>% summarise(total = sum(n)) %>% ungroup()
      data_source.total <- left_join(data_source.counts, data_source.year_counts)
      
      # Add in TF, IDF and TFIDF.
      #
      data_source.total <- data_source.total %>% bind_tf_idf(word, year, n)
      
      data_source.total %>% arrange(desc(tf_idf)) %>% 
        group_by(year) %>% 
        top_n(4) %>% 
        ungroup %>% 
        mutate(word = reorder(word, tf_idf)) %>% 
        ggplot(aes(x = word, y = tf_idf, fill = factor(year))) +
        geom_col() +
        facet_wrap(~factor(year), scales = "free") +
        coord_flip() + 
        labs(title = "Your Vocabulary Over Time") +
        theme(legend.position = "none",
              axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank())
    }
  })
}

shinyApp(ui = ui, server = server)
