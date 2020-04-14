
GLOBAL_MIN_RATE = .85
GLOBAL_MAX_RATE = .9

GLOBAL_MIN_STEP = -.05
GLOBAL_MAX_STEP = -.03

PATTERNS  <- c("fluct", "spikelg", "spikesm", "dec")

# if unspecified, prior distribution on buy price: runif(1, 90, 110)
# this doesn't get taken into account in this code, though,
# this code just fails to run if buy price isn't specified

# first, draw a hyperpattern
# then, once hyperpattern is drawn, if multiple possibilities
# draw parameter(s) for that hyperpattern

## fluct ----

get_trend_fluct <- function (given_prices,
                             buy_price,
                             high1_len,
                             dec1_len,
                             high2_len,
                             dec2_len) {
  
  # Crizazy logic to get the halfday indices of all "high" phases
  if (high1_len > 0) {
    high_days <- 1:(high1_len)
  } else {
    high_days <- NULL
  }
  
  if (high2_len > 0) {
    high_days <- c(high_days, (high1_len+dec1_len+1):(high1_len+dec1_len+high2_len))
  }
  
  high_days <- c(high_days, (high1_len+dec1_len+high2_len+dec2_len+1):12)
  
  # all high phases drawn from the same rates
  predicted_highs <- given_prices %>%
    filter(halfday %in% high_days) %>%
    mutate(min_rate = 0.9,
           max_rate = 1.4) %>%
    get_preds(buy_price)
  
  # both decreasing phases drawn with the same rates,
  # but drawn separately because the rate decrement restarts for dec2
  predicted_dec1 <- given_prices %>%
    filter(halfday > high1_len, halfday <= high1_len + dec1_len) %>%
    get_trend_dec(buy_price, duration = n(), minr = .6, maxr = .8, mins = -.1, maxs = -.04)
  
  predicted_dec2 <- given_prices %>%
    filter(halfday > high1_len + dec1_len + high2_len,
           halfday <= high1_len + dec1_len + high2_len + dec2_len) %>%
    get_trend_dec(buy_price, duration = n(), minr = .6, maxr = .8, mins = -.1, maxs = -.04)
  
  # bind together
  predicted_highs %>%
    bind_rows(predicted_dec1, predicted_dec2) %>%
    arrange(halfday) %>%
    select(-ends_with("rate")) %>%
    return()
  
}

## spikelg ----

get_trend_spikelg <- function (given_prices, buy_price, peak_start) {
  
  # use decreasing trend function for days up to peak start
  predicted_pre <- given_prices %>%
    get_trend_dec(buy_price, duration = peak_start - 1)
  
  # second part of week: first half-day that the peak starts
  # each half-day is independent here
  # taken from a truncated set of possible rates
  predicted_post <- tibble(min_rate = c(0.9, 1.4, 2.0, 1.4, 0.9, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4),
                           max_rate = c(1.4, 2.0, 6.0, 2.0, 1.4, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9)) %>%
    get_preds(buy_price) %>%
    mutate(halfday = 1:n() + peak_start - 1) %>%
    filter(halfday <= 12) %>%
    left_join(given_prices, by = "halfday")
  
  # bind together
  predicted_pre %>%
    bind_rows(predicted_post) %>%
    select(-ends_with("rate")) %>%
    return()
  
}

## spikesm ----

get_trend_spikesm <- function (given_prices, buy_price, peak_start) {
  
  # before peak: decreasing trend
  predicted_pre <- given_prices %>%
    get_trend_dec(buy_price, duration = peak_start - 1, minr = .4)
  
  # peak is 5 half-days long apparently
  # halfdays 1-2 of peak: min_rate = 0.9, max_rate = 1.4
  # halfday 3 of peak: 1.4, 2.0, then both preds - 1 bell
  # halfday 4 of peak: not on rates, but min_pred same as hd3, max_pred hd3 + 1
  # hd5 of peak: rate 1.4 - 1, max_pred hd4 - 1
  
  # may as well set these all independently
  predicted_peak <- given_prices %>%
    filter(halfday %in% peak_start:(peak_start + 4)) %>%
    mutate(min_pred = NA,
           max_pred = NA)
  
  pluck(predicted_peak, "min_pred", 1) <- 0.9 * buy_price
  pluck(predicted_peak, "max_pred", 1) <- 1.4 * buy_price
  pluck(predicted_peak, "min_pred", 2) <- 0.9 * buy_price
  pluck(predicted_peak, "max_pred", 2) <- 1.4 * buy_price
  pluck(predicted_peak, "min_pred", 3) <- 1.4 * buy_price - 1
  pluck(predicted_peak, "max_pred", 3) <- 2 * buy_price - 1
  pluck(predicted_peak, "min_pred", 4) <- pluck(predicted_peak, "min_pred", 3)
  pluck(predicted_peak, "max_pred", 4) <- pluck(predicted_peak, "max_pred", 3)
  pluck(predicted_peak, "min_pred", 5) <- 1.4 * buy_price - 1
  pluck(predicted_peak, "max_pred", 5) <- pluck(predicted_peak, "max_pred", 4) - 1
  
  # after peak: decreasing again
  predicted_post <- given_prices %>%
    filter(halfday >= peak_start + 5) %>%
    get_trend_dec(buy_price, duration = n(), minr = .4)
  
  predicted_pre %>%
    bind_rows(predicted_peak %>%
                mutate_at(vars(ends_with("pred")), as.integer),
              predicted_post) %>%
    select(-ends_with("rate")) %>%
    return()
}

## dec ----

get_trend_dec <- function (given_prices,
                           buy_price,
                           duration = 12,
                           minr = GLOBAL_MIN_RATE,
                           maxr = GLOBAL_MAX_RATE,
                           mins = GLOBAL_MIN_STEP,
                           maxs = GLOBAL_MAX_STEP) {
  # note that mins is always more negative than maxs
  # so the bottom of the range goes down more than the top does
  
  predicted <- given_prices
  
  # if no data given, start with raw priors
  if (all(is.na(predicted$price))) {
    predicted %<>%
      # these are the start points and imputed later data
      mutate(min_rate = seq(minr, by = mins, length.out = n()),
             max_rate = seq(maxr, by = maxs, length.out = n()))
  } else {
    # if any data given, seed those rates first
    predicted %<>%
      get_rates(buy_price)
    
    # check through every row
    for (i in 1:(nrow(predicted) - 1)) {
      
      # if data exist for this day and not for the next day,
      # impute the next day's rate
      # as written, this skips any days with missing data where a later day has data
      if (!is.na(pluck(predicted, "min_rate", i)) & is.na(pluck(predicted, "min_rate", i+1))) {
        pluck(predicted, "min_rate", i+1) <- pluck(predicted, "min_rate", i) - mins
        pluck(predicted, "max_rate", i+1) <- pluck(predicted, "max_rate", i) - maxs
      }
    }
  }
  
  predicted %>%
    get_preds(buy_price) %>%
    select(-ends_with("rate")) %>%
    filter(halfday <= min(halfday) + duration - 1) %>%
    return()
}

## helpers ----

get_preds <- function (prices, buy_price) {
  prices %>%
    mutate(min_pred = floor(min_rate * buy_price),
           max_pred = ceiling(max_rate * buy_price)) %>%
   mutate_at(vars(ends_with("pred")), as.integer)
}

get_rates <- function (prices, buy_price) {
  prices %>%
    mutate(min_rate = get_min_rate(price, buy_price),
           max_rate = get_max_rate(price, buy_price))
}

get_min_rate <- function (given_price, buy_price) {
  return ((given_price - 1) / buy_price)
}

get_max_rate <- function (given_price, buy_price) {
  return (given_price / buy_price)
}

rpattern <- function (pattern, patterns = PATTERNS) {
  
  if (pattern == "fluct") {
    sample(patterns, 1, prob = c(.2, .3, .35, .15))
  } else if (pattern == "spikelg") {
    sample(patterns, 1, prob = c(.5, .05, .25, .2))
  } else if (pattern == "spikesm") {
    sample(patterns, 1, prob = c(.45, .25, .15, .15))
  } else if (pattern == "dec") {
    sample(patterns, 1, prob = c(.25, .45, .25, .05))
  }
}

get_pattern_probs <- function (pattern, patterns = PATTERNS) {
  if (pattern == "fluct") {
    return(c(.2, .3, .35, .15))
  } else if (pattern == "spikelg") {
    return(c(.5, .05, .25, .2))
  } else if (pattern == "spikesm") {
    return(c(.45, .25, .15, .15))
  } else if (pattern == "dec") {
    return(c(.25, .45, .25, .05))
  } else if (pattern == "none") {
    # the markov probability matrix appears to be, just from simulating:
    return(c(.35, .25, .25, .15))
  }
}
