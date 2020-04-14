#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(magrittr)
source("trends.R")
n_iterations <- 75

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Bayesian ACNH turnip estimator"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     sidebarPanel(
       fluidRow(
         radioButtons("prev_pattern",
                      "Last week's pattern",
                      choices = list("Fluctuating" = "fluct",
                                     "Large spike" = "spikelg",
                                     "Small spike" = "spikesm",
                                     "Decreasing" = "dec",
                                     "Don't know" = "none"),
                      selected = "none")
       ),
       fluidRow(
         textInput("buy_price",
                      "Buy price on Sunday",
                      value = 90)
       ),
       fluidRow(
         column(6,
                textInput("sell_price_hd1",
                             "Sell price Monday morning",
                             value = NA)
         ),
         column(6,
                textInput("sell_price_hd2",
                             "Sell price Monday afternoon",
                             value = NA)
         ),
         column(6,
                textInput("sell_price_hd3",
                             "Sell price Tuesday morning",
                             value = NA)
         ),
         column(6,
                textInput("sell_price_hd4",
                             "Sell price Tuesday afternoon",
                             value = NA)
         ),
         column(6,
                textInput("sell_price_hd5",
                             "Sell price Wednesday morning",
                             value = NA)
         ),
         column(6,
                textInput("sell_price_hd6",
                             "Sell price Wednesday afternoon",
                             value = NA)
         )
       ),
       fluidRow(
         column(6,
                textInput("sell_price_hd7",
                             "Sell price Thursday morning",
                             value = NA)
         ),
         column(6,
                textInput("sell_price_hd8",
                             "Sell price Thursday afternoon",
                             value = NA)
         ),
         column(6,
                textInput("sell_price_hd9",
                             "Sell price Friday morning",
                             value = NA)
         ),
         column(6,
                textInput("sell_price_hd10",
                             "Sell price Friday afternoon",
                             value = NA)
         ),
         column(6,
                textInput("sell_price_hd11",
                             "Sell price Saturday morning",
                             value = NA)
         ),
         column(6,
                textInput("sell_price_hd12",
                             "Sell price Saturday afternoon",
                             value = NA)
         )
       )
     ),
     
     # Show a plot of the generated distribution
     mainPanel(
       p("It ain't cute but it does the job, I hope.",
         "Please be mindful that it takes a bit of time to re-estimate the probabilities when you update the data."),
       h2("What's the probability you're in a particular pattern overall?"),
       p("These probabilities are a bit noisy because they're generated from random draws of possible turnip prices."),
       tableOutput("tib"),
       h2("Heatmap plot of possible future turnip prices"),
       p("This plot takes into account overall uncertainty about which pattern you're in."),
       p("For example, if many plausible patterns predict turnip prices in a particular range,",
         "the heatmap will be brighter there, because over all the patterns it could be,",
         "most of them predict the turnip prices will be in that range."),
       plotOutput("plot"),
       h3("Credits"),
       p("Built using R, the",
         a(href ="https://tidyverse.org/", "tidyverse"), "and",
         a(href = "https://shiny.rstudio.com/", "Shiny"),
         "by",
         a(href = "https://github.com/monicathieu", "Monica Thieu.")),
       p("Thanks to",
         a(href = "https://gist.github.com/Treeki/85be14d297c80c8b3c0a76375743325b",
                        "Ninji"),
         "for reverse-engineering the turnip price trend generation algorithm out of the ACNH source code."),
       p("Thanks also to",
         a(href = "https://turnipprophet.io/index.html", "Mike Bryant,"),
         a(href = "https://elxris.github.io/Turnip-Calculator/", "Christian Ceciliano,"),
         "and all contributors to those for their open-source JavaScript turnip trend calculators.",
         "I roughly eyeball-translated their C++/JavaScript code into R to extend it with R's ability to",
         "numerically estimate the posterior distribution of turnip prices using random draws.",
         "We stand on the shoulders of giants. :)"),
       h3("Suggestions?"),
       p("Please submit an",
         a(href = "https://github.com/monicathieu/shiny-acnh-turnips/issues",
           "issue or pull request!"))
     )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  get_givens <- reactive({
    given_prices <- tibble(halfday = 1:12,
                           price = c(input$sell_price_hd1,
                                     input$sell_price_hd2,
                                     input$sell_price_hd3,
                                     input$sell_price_hd4,
                                     input$sell_price_hd5,
                                     input$sell_price_hd6,
                                     input$sell_price_hd7,
                                     input$sell_price_hd8,
                                     input$sell_price_hd9,
                                     input$sell_price_hd10,
                                     input$sell_price_hd11,
                                     input$sell_price_hd12)) %>%
      mutate(price = as.integer(price))
  })
  
  get_buy_price <- reactive({
    as.integer(input$buy_price)
  })
  
  get_draws <- reactive({
    ## construct de whole prior distribution ----
    
    given_prices <- get_givens()
    
    buy_price <- get_buy_price()
    
    trends_fluct <- crossing(high1_len = 0:6,
                             dec1_len = 2:3) %>%
      mutate(high2_len = map(high1_len, ~0:(6 - .x)),
             dec2_len = 5 - dec1_len) %>%
      unchop(high2_len) %>%
      mutate(id = 1:n(),
             posterior = pmap(list(high1_len, dec1_len, high2_len, dec2_len),
                              function (a, b, c, d) {
                                get_trend_fluct(given_prices, buy_price,
                                                a, b, c, d)
                              }),
             prior = pmap(list(high1_len, dec1_len, high2_len, dec2_len),
                          function (a, b, c, d) {
                            get_trend_fluct(given_prices %>% mutate(price = NA), buy_price,
                                            a, b, c, d)
                          }))
    
    trends_spikelg <- tibble(peak_start = 2:8) %>%
      mutate(id = 1:n(), sep = "_",
             posterior = map(peak_start,
                             ~get_trend_spikelg(given_prices,
                                                buy_price,
                                                .x)),
             prior = map(peak_start,
                         ~get_trend_spikelg(given_prices %>% mutate(price = NA),
                                            buy_price,
                                            .x)))
    
    trends_spikesm <- tibble(peak_start = 1:8) %>%
      mutate(id = 1:n(),
             posterior = map(peak_start,
                             ~get_trend_spikesm(given_prices,
                                                buy_price,
                                                .x)),
             prior = map(peak_start,
                         ~get_trend_spikesm(given_prices %>% mutate(price = NA),
                                            buy_price,
                                            .x)))
    
    
    trends_dec <- tibble(id = 1) %>%
      mutate(posterior = map(id, ~given_prices %>%
                               get_trend_dec(buy_price)),
             prior = map(id, ~given_prices %>%
                           mutate(price = NA) %>% 
                           get_trend_dec(buy_price)))
    
    trends_all <- bind_rows(fluct = trends_fluct,
                            spikelg = trends_spikelg,
                            spikesm = trends_spikesm,
                            dec = trends_dec,
                            .id = "pattern") %>%
      select(pattern, id, posterior, prior) %>%
      nest(trends = -pattern) %>% 
      mutate(prob_pattern = get_pattern_probs(input$prev_pattern))

    
    posterior <- trends_all %>%
      unnest(trends) %>%
      # calculate normalized conditional probability of each trend
      group_by(pattern) %>%
      mutate(prob_cond_prior = 1 / n()) %>%
      ungroup() %>%
      # remove P=0 trend iterations (observed data outside of prior distribution)
      mutate(pred_below_min = map2_lgl(posterior, prior, ~any(.x$min_pred < .y$min_pred)),
             pred_above_max = map2_lgl(posterior, prior, ~any(.x$max_pred > .y$max_pred)),
             price_below_min = map_lgl(posterior, ~any(.x$price < .x$min_pred & !is.na(.x$price))),
             price_above_max = map_lgl(posterior, ~any(.x$price > .x$max_pred & !is.na(.x$price)))) %>%
      filter(!pred_below_min, !pred_above_max, !price_below_min, !price_above_max) %>%
      # mash posterior and prior stuff together
      unnest(c(posterior, prior), names_sep = "_") %>%
      rename(halfday = "posterior_halfday", price = "posterior_price") %>%
      select(-prior_halfday, -prior_price) %>%
      # calculate normalized overall probability of each plausible trend
      nest(trends = -c(pattern, id, prob_cond_prior)) %>%
      mutate(prob_trend = prob_cond_prior / sum(prob_cond_prior)) %>%
      unnest(trends)
    
    draws <- posterior %>%
      select(pattern, id, prob_trend, halfday, price, starts_with("posterior")) %>%
      nest(trends = -c(pattern, id, prob_trend))
    
    draw_probs = draws %>% pull(prob_trend)
    
    draws %<>%
      sample_n(n_iterations, replace = TRUE, weight = draw_probs) %>%
      unnest(trends) %>%
      mutate(prices_draw = map2(posterior_min_pred, posterior_max_pred,
                                ~tibble(halfday_id = 1:n_iterations) %>%
                                  mutate(price_draw = runif(1:nrow(.), .x, .y) %>%
                                           round() %>%
                                           as.integer()))) %>%
      unnest(prices_draw) %>%
      mutate(price = coalesce(price, price_draw))
    
  })
  
  output$tib <- renderTable({
    get_draws() %>%
      count(pattern) %>%
      ungroup() %>%
      mutate(pattern = recode(pattern,
                              fluct = "fluctuating",
                              spikelg = "large spike",
                              spikesm = "small spike",
                              dec = "decreasing"),
             prob = n/sum(n),
             probability = glue::glue("{round(prob * 100, 0)}%")) %>%
      select(pattern, probability)
  })
  
  output$plot <- renderPlot({
    get_draws() %>%
      count(halfday, price) %>%
      group_by(halfday) %>%
      mutate(n = n / sum(n)) %>%
      ungroup() %>%
      # boost the density of the plot up
      # should NA out data from halfdays where the price was given
      # to avoid them skewing the color scale
      mutate(price = if_else(halfday %in% (get_givens() %>% filter(is.na(price)) %>% pull(halfday)),
                             price,
                             NA_integer_),
             n = na_if(n, 1)) %>%
      slice(rep(1:n(), 15)) %>%
      arrange(n) %>%
      ggplot(aes(x = factor(halfday, levels = 1:12), y = price)) +
      geom_jitter(aes(color = n), alpha = 0.3) +
      geom_line(aes(x = halfday), data = get_givens(), color = "white") +
      geom_point(data = get_givens(), color = "white") +
      geom_hline(yintercept = get_buy_price(), color = "white", linetype = 3) +
      annotate("label",
               label = glue::glue("Buy price: {get_buy_price()} Bells"),
               x = 12,
               y = get_buy_price() + 5, hjust = 1, vjust = 0, color = "white", fill = "black",
               size = rel(5)) +
      scale_x_discrete(guide = guide_axis(n.dodge = 2),
                       breaks = 1:12,
                       labels = c("Mon morning", "Mon afternoon",
                                  "Tues morning", "Tues afternoon",
                                  "Wed morning", "Wed afternoon",
                                  "Thu morning", "Thu afternoon",
                                  "Fri morning", "Fri afternoon",
                                  "Sat morning", "Sat afternoon")) +
      scale_color_viridis_c() +
      labs(x = "Day-ish",
           y = "Possible turnip price (Bells)",
           color = "probability") +
      theme_dark() +
      theme(axis.title = element_text(size = rel(1.5)),
            axis.text = element_text(size = rel(1)),
            panel.background = element_rect(fill = "black"))
  })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

