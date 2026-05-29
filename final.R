library(tidyverse)
library(dplyr)
library(purrr)
library(furrr)
library(ggplot2)
library(yardstick)


MAX_GUESSES <- 6
OPTIMAL_FIRST_GUESS <- "slate"

clean_word_list <- function(words) {
  words <- tolower(trimws(words))
  words <- words[nchar(words) == 5]
  words <- words[grepl("^[a-z]{5}$", words)]
  unique(words)
}


get_feedback_chars <- function(guess_chars, sol_chars) {
  feedback <- integer(5)
  used <- logical(5)
  
  for (i in 1:5) {
    if (guess_chars[i] == sol_chars[i]) {
      feedback[i] <- 2
      used[i] <- TRUE
    }
  }
  
  for (i in 1:5) {
    if (feedback[i] == 0) {
      for (j in 1:5) {
        if (!used[j] && guess_chars[i] == sol_chars[j]) {
          feedback[i] <- 1
          used[j] <- TRUE
          break
        }
      }
    }
  }
  
  feedback
}


# Vectorized version: one guess vs many candidates at once
# Returns n x 5 matrix of feedback values
get_feedback_matrix <- function(guess_chars, candidates_matrix) {
  n <- nrow(candidates_matrix)
  feedback <- matrix(0L, nrow = n, ncol = 5)
  used <- matrix(FALSE, nrow = n, ncol = 5)
  
  # Pass 1: mark greens
  for (i in 1:5) {
    match <- candidates_matrix[, i] == guess_chars[i]
    feedback[match, i] <- 2L
    used[match, i] <- TRUE
  }
  
  # Pass 2: mark yellows
  for (i in 1:5) {
    need_yellow <- feedback[, i] == 0L
    if (!any(need_yellow)) next
    
    for (j in 1:5) {
      check <- need_yellow & !used[, j] & candidates_matrix[, j] == guess_chars[i]
      if (any(check)) {
        feedback[check, i] <- 1L
        used[check, j] <- TRUE
        need_yellow <- need_yellow & !check
      }
    }
  }
  
  feedback
}


filter_candidates <- function(candidates_idx, guess_idx, feedback, guesses_split, candidates_matrix = NULL) {
  guess_chars <- guesses_split[[guess_idx]]
  
  # Use vectorized version if matrix provided
  if (!is.null(candidates_matrix)) {
    fb_matrix <- get_feedback_matrix(guess_chars, candidates_matrix)
    keep <- apply(fb_matrix, 1, function(row) all(row == feedback))
    return(candidates_idx[keep])
  }
  
  # Fallback to scalar version
  keep <- vapply(
    candidates_idx,
    function(i) {
      all(get_feedback_chars(guess_chars, guesses_split[[i]]) == feedback)
    },
    logical(1)
  )
  
  candidates_idx[keep]
}


init_state <- function() {
  list(
    greens = rep(NA_character_, 5),
    yellows = vector("list", 5),
    grays = character(0),
    used_letters = character(0)
  )
}

update_state <- function(state, guess_chars, feedback) {
  for (i in 1:5) {
    l <- guess_chars[i]
    state$used_letters <- union(state$used_letters, l)
    
    if (feedback[i] == 2) {
      state$greens[i] <- l
    } else if (feedback[i] == 1) {
      state$yellows[[i]] <- union(state$yellows[[i]], l)
    } else {
      state$grays <- union(state$grays, l)
    }
  }
  state
}

letter_freq <- function(candidates_idx, guesses_split) {
  letters <- unlist(guesses_split[candidates_idx])
  tab <- table(letters)
  as.numeric(tab / sum(tab)) |> setNames(names(tab))
}


score_valid_guess <- function(word_chars, state, lf) {
  novelty <- sum(!word_chars %in% state$used_letters)
  freq_score <- sum(lf[word_chars], na.rm = TRUE)
  green_score <- sum(!is.na(state$greens) & word_chars == state$greens)
  
  novelty * 0.6 + freq_score * 0.3 + green_score * 0.1
}


expected_entropy <- function(guess_idx, candidates_idx, guesses_split, candidates_matrix) {
  guess_chars <- guesses_split[[guess_idx]]
  fb_matrix <- get_feedback_matrix(guess_chars, candidates_matrix)
  
  # Convert rows to string keys for grouping
  fb_keys <- apply(fb_matrix, 1, paste, collapse = "")
  probs <- table(fb_keys) / length(fb_keys)
  -sum(probs * log2(probs))
}


# Fast batch entropy computation for all candidates
compute_all_entropies <- function(candidates_idx, candidates_matrix, guesses_split) {
  n <- length(candidates_idx)
  entropies <- numeric(n)
  
  for (k in seq_len(n)) {
    guess_chars <- guesses_split[[candidates_idx[k]]]
    fb_matrix <- get_feedback_matrix(guess_chars, candidates_matrix)
    # Pack feedback into single integer (0-242): faster than paste
    fb_key <- fb_matrix[,1] * 81L + fb_matrix[,2] * 27L + fb_matrix[,3] * 9L + 
      fb_matrix[,4] * 3L + fb_matrix[,5]
    probs <- tabulate(fb_key + 1L, nbins = 243) / n
    probs <- probs[probs > 0]
    entropies[k] <- -sum(probs * log2(probs))
  }
  entropies
}


entropy <- function(n) log2(max(n, 1))

extract_features <- function(history) {
  
  entropies <- vapply(history, `[[`, numeric(1), "entropy_before")
  drops <- -diff(entropies)
  
  # Fix pathological values
  drops[!is.finite(drops)] <- 0
  
  tibble(
    n_guesses = length(history),
    entropy_start = entropies[1],
    entropy_end = tail(entropies, 1),
    mean_entropy_drop = ifelse(length(drops) > 0, mean(drops), 0),
    max_entropy_drop = ifelse(length(drops) > 0, max(drops), 0),
    early_entropy_drop = ifelse(length(drops) >= 2,
                                mean(drops[1:2]),
                                0),
    monotone_entropy = ifelse(length(drops) > 0,
                              all(drops >= -1e-6),
                              TRUE),
    green_fraction = mean(vapply(history, \(h) mean(h$feedback == 2), numeric(1))),
    yellow_fraction = mean(vapply(history, \(h) mean(h$feedback == 1), numeric(1)))
  )
}


simulate_game <- function(
    solution_idx,
    guesses_split,
    word_list,
    invalid = FALSE
) {
  candidates_idx <- seq_along(word_list)
  # Pre-build matrix of all candidate chars (n x 5)
  all_chars_matrix <- do.call(rbind, guesses_split)
  candidates_matrix <- all_chars_matrix[candidates_idx, , drop = FALSE]
  
  history <- list()
  state <- init_state()
  
  for (g in seq_len(MAX_GUESSES)) {
    if (length(candidates_idx) == 0) break
    
    entropy_before <- entropy(length(candidates_idx))
    
    # guess selection
    if (!invalid) {
      lf <- letter_freq(candidates_idx, guesses_split)
      scores <- vapply(
        candidates_idx,
        \(i) score_valid_guess(guesses_split[[i]], state, lf),
        numeric(1)
      )
      guess_idx <- candidates_idx[which.max(scores)]
      
      # occasional human mistake
      if (runif(1) < 0.15) {
        guess_idx <- sample(candidates_idx, 1)
      }
      
    } else {
      # Bot strategy: use known optimal first guess, then maximize entropy
      if (g == 1) {
        guess_idx <- match(OPTIMAL_FIRST_GUESS, word_list)
      } else {
        ent <- compute_all_entropies(candidates_idx, candidates_matrix, guesses_split)
        guess_idx <- candidates_idx[which.max(ent)]
      }
    }
    
    feedback <- get_feedback_chars(
      guesses_split[[guess_idx]],
      guesses_split[[solution_idx]]
    )
    
    state <- update_state(state, guesses_split[[guess_idx]], feedback)
    
    # Filter candidates and update matrix
    keep <- filter_candidates(
      candidates_idx, guess_idx, feedback, guesses_split, candidates_matrix
    )
    keep_mask <- candidates_idx %in% keep
    candidates_idx <- keep
    candidates_matrix <- candidates_matrix[keep_mask, , drop = FALSE]
    
    history[[g]] <- list(
      guess = word_list[guess_idx],
      feedback = feedback,
      entropy_before = entropy_before
    )
    
    if (guess_idx == solution_idx) break
  }
  
  extract_features(history)
}


simulate_valid_game <- function(solution, word_list, guesses_split) {
  simulate_game(
    solution_idx = match(solution, word_list),
    guesses_split = guesses_split,
    word_list = word_list,
    invalid = FALSE
  )
}

simulate_invalid_game <- function(solution, word_list, guesses_split) {
  simulate_game(
    solution_idx = match(solution, word_list),
    guesses_split = guesses_split,
    word_list = word_list,
    invalid = TRUE
  )
}


guesses <- clean_word_list(scan("combined_wordlist.txt", what = character()))
solutions <- clean_word_list(scan("shuffled_real_wordles.txt", what = character()))
guesses_split <- strsplit(guesses, "")

#synthetic games 
set.seed(1232026)

system.time(simulate_valid_game(sample(solutions, 1), guesses, guesses_split))
system.time(simulate_invalid_game(sample(solutions, 1), guesses, guesses_split))


plan(multisession)

synthetic_valid <- future_map_dfr(
  1:1000,
  ~ simulate_valid_game(sample(solutions, 1), guesses, guesses_split)
)

synthetic_valid$valid  <- 1
synthetic_valid$source <- "synthetic_valid"


synthetic_invalid <- future_map_dfr(
  1:1000,
  ~ simulate_invalid_game(sample(solutions, 1), guesses, guesses_split)
)
synthetic_invalid$valid  <- 0
synthetic_invalid$source <- "synthetic_invalid"


synthetic_data <- bind_rows(synthetic_valid, synthetic_invalid)

write.csv(synthetic_data, "synthetic_data.csv")


synthetic_data <- read.csv("synthetic_data.csv")

library(tidyverse)

synthetic_data <- synthetic_data %>%
  mutate(source = factor(source))

synthetic_data <- synthetic_data %>%
  mutate(valid = factor(valid, levels = c(1, 0)))


# EDA plots

ggplot(synthetic_data, aes(x = source, y = mean_entropy_drop, fill = source)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.15, outlier.shape = NA) +
  labs(
    title = "Mean entropy drop per guess",
    y = "Mean entropy drop (bits)",
    x = NULL
  ) +
  theme_minimal()


ggplot(synthetic_data, aes(x = source, y = max_entropy_drop, fill = source)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Maximum single-step entropy drop",
    y = "Max entropy drop (bits)",
    x = NULL
  ) +
  theme_minimal()

ggplot(synthetic_data, aes(x = source, y = early_entropy_drop, fill = source)) +
  geom_violin(trim = TRUE, alpha = 0.6) +
  geom_boxplot(width = 0.15, outlier.shape = NA) +
  labs(
    title = "Early-game entropy reduction",
    subtitle = "Mean entropy drop over first two guesses",
    y = "Early entropy drop (bits)",
    x = NULL
  ) +
  theme_minimal()


ggplot(synthetic_data, aes(x = entropy_start, y = entropy_end, color = source)) +
  geom_point(alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title = "Entropy reduction over a game",
    x = "Entropy at start",
    y = "Entropy at end"
  ) +
  theme_minimal()


ggplot(synthetic_data, aes(x = n_guesses, fill = source)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Number of guesses per game",
    x = "Guesses used",
    y = "Count"
  ) +
  theme_minimal()


ggplot(synthetic_data,
       aes(x = yellow_fraction, y = green_fraction, color = source)) +
  geom_point(alpha = 0.7) +
  labs(
    title = "Feedback composition by game",
    x = "Fraction yellow",
    y = "Fraction green"
  ) +
  theme_minimal()


#extracting same features from real data 

parse_processed_text <- function(txt) {
  txt <- gsub("\\[|\\]|'|\"", "", txt)
  rows <- unlist(strsplit(txt, "\\s+"))
  rows <- rows[nzchar(rows)]
  
  lapply(rows, function(row) {
    chars <- strsplit(row, "")[[1]]
    as.integer(dplyr::recode(chars,
                             "⬛" = 0L,
                             "⬜" = 0L,
                             "🟨" = 1L,
                             "🟩" = 2L
    ))
  })
}

set.seed(212026)
real_data <- wordle_games %>% sample_n(size = 1000, replace = FALSE)

build_real_history <- function(processed_text) {
  feedback_list <- parse_processed_text(processed_text)
  
  history <- lapply(feedback_list, function(fb) {
    list(
      entropy_before = NA,  # exact entropy cannot be computed without guesses
      feedback = fb
    )
  })
  
  history
}

extract_real_features <- function(processed_text) {
  history <- build_real_history(processed_text)
  
  if (is.null(history) || length(history) == 0) return(NULL)
  
  # Basic counts
  n_guesses <- length(history)
  feedback_sums <- sapply(history, function(h) sum(h$feedback))
  
  early_entropy_drop <- sum(feedback_sums[1:min(2, n_guesses)])
  mean_entropy_drop <- mean(feedback_sums)
  max_entropy_drop <- max(feedback_sums)
  
  # Additional derived features
  all_feedback <- do.call(rbind, lapply(history, function(h) h$feedback))
  green_fraction <- mean(all_feedback == 2)
  yellow_fraction <- mean(all_feedback == 1)
  
  # Monotone entropy: simple approximation — 1 if mean drops decrease
  monotone_entropy <- all(diff(feedback_sums) <= 0)  
  # Approximate start/end entropy
  entropy_start <- sum(all_feedback[1, ])    # first guess sum
  entropy_end <- sum(all_feedback[n_guesses, ])  # last guess sum
  
  tibble(
    n_guesses = n_guesses,
    entropy_start = entropy_start,
    entropy_end = entropy_end,
    mean_entropy_drop = mean_entropy_drop,
    max_entropy_drop = max_entropy_drop,
    early_entropy_drop = early_entropy_drop,
    monotone_entropy = monotone_entropy,
    green_fraction = green_fraction,
    yellow_fraction = yellow_fraction,
    valid = NA,
    source = "real"
  )
}

fb_matrix <- parse_processed_text(real_data$processed_text)

real_features <- real_data %>%
  mutate(features = purrr::map(processed_text, extract_real_features)) %>%
  tidyr::unnest(features)

real_features$valid  <- NA
real_features$source <- "real"

real_features <- real_features %>%
  select(all_of(intersect(names(real_features), names(synthetic_data))))


write_csv(real_features, "real_features.csv")

#real_features <- read.csv("real_features.csv")

#model time

library(tidymodels)

set.seed(2232026)

#factor-ing outcome variable
synthetic_data <- synthetic_data %>%
  mutate(valid = factor(valid)) %>% 
  select(-X)


#70/30 split, stratifying on outcome variable, k-fold CV
split <- initial_split(
  synthetic_data,
  prop = 0.7,
  strata = valid
)

train_data <- training(split)
train_data$valid <- factor(train_data$valid)
test_data  <- testing(split)
test_data$valid <- factor(test_data$valid)

folds <- vfold_cv(
  train_data,
  v = 5,
  strata = valid
)

#recipe
rec <- recipe(valid ~ ., data = train_data) %>%
  step_rm(monotone_entropy, source) %>%   # always true
  step_zv(all_predictors()) %>%   # removes constant columns
  step_dummy(all_nominal_predictors()) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_impute_mode(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors())

#engines
log_spec <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")

enet_spec <- logistic_reg(
  penalty = tune(),
  mixture = tune()
) %>%
  set_engine("glmnet") %>%
  set_mode("classification")

rf_spec <- rand_forest(
  mtry  = tune(),
  trees = 500,
  min_n = tune()
) %>%
  set_engine("ranger") %>%
  set_mode("classification")

xgb_spec <- boost_tree(
  trees = tune(),
  tree_depth = tune(),
  learn_rate = tune(),
  min_n = tune()
) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

#workflows
log_wf  <- workflow() %>% add_model(log_spec)  %>% add_recipe(rec)
enet_wf <- workflow() %>% add_model(enet_spec) %>% add_recipe(rec)
rf_wf   <- workflow() %>% add_model(rf_spec)   %>% add_recipe(rec)
xgb_wf  <- workflow() %>% add_model(xgb_spec)  %>% add_recipe(rec)

#tuning grids 
enet_grid <- grid_regular(
  penalty(range = c(-4, 1)),
  mixture(range = c(0, 1)),
  levels = 5
)

rf_grid <- grid_regular(
  mtry(range = c(1, ncol(train_data) - 1)),
  min_n(range = c(2, 10)),
  levels = 5
)

xgb_grid <- grid_regular(
  trees(range = c(300, 1000)),
  tree_depth(range = c(3, 8)),
  learn_rate(range = c(-3, -0.5)),
  min_n(range = c(2, 10)),
  levels = 3
)

#tuning
my_metrics <- yardstick::metric_set(
  yardstick::roc_auc,
  yardstick::accuracy
)

#log = no tuning
log_res <- fit_resamples(
  log_wf,
  resamples = folds,
  metrics = my_metrics
)

#the rest (yes tuning)
enet_res <- tune_grid(
  enet_wf,
  resamples = folds,
  grid = enet_grid,
  metrics = my_metrics
)

rf_res <- tune_grid(
  rf_wf,
  resamples = folds,
  grid = rf_grid,
  metrics = my_metrics
)


xgb_res <- tune_grid(
  xgb_wf,
  resamples = folds,
  grid = xgb_grid,
  metrics = my_metrics
)


best_enet <- select_best(enet_res, metric = "roc_auc")
best_rf   <- select_best(rf_res, metric = "roc_auc")
best_xgb  <- select_best(xgb_res, metric = "roc_auc")

final_enet <- finalize_workflow(enet_wf, best_enet)
final_rf   <- finalize_workflow(rf_wf, best_rf)
final_xgb  <- finalize_workflow(xgb_wf, best_xgb)

#fitting
log_fit  <- fit(log_wf,  data = train_data)
enet_fit <- fit(final_enet, data = train_data)
rf_fit   <- fit(final_rf,   data = train_data)
xgb_fit  <- fit(final_xgb,  data = train_data)


#evaluating on synthetic test set
test_preds_xgb <- augment(xgb_fit, test_data)

test_preds_log <- augment(log_fit, test_data)

test_preds_enet <- augment(enet_fit, test_data)

test_preds_rf <- augment(rf_fit, test_data)

metric_set(roc_auc, accuracy)(data = test_preds_log,truth = valid,estimate = .pred_class, .pred_1)
metric_set(roc_auc, accuracy)(test_preds_enet, truth = valid, estimate = .pred_class, .pred_1)
metric_set(roc_auc, accuracy)(test_preds_rf, truth = valid, estimate= .pred_class, .pred_1)
metric_set(roc_auc, accuracy)(test_preds_xgb, truth = valid, estimate = .pred_class, .pred_1) #best one!




#real test data
# Predict on real features using each fitted model
#pred_log <- predict(log_fit, real_features, type = "prob") %>% bind_cols(real_features)
#pred_enet <- predict(enet_fit, real_features, type = "prob") %>% bind_cols(real_features)
pred_rf <- predict(rf_fit, real_features, type = "prob") %>% bind_cols(real_features)
pred_xgb <- predict(xgb_fit, real_features, type = "prob") %>% bind_cols(real_features)


#alternate visualizations since can't do testing error 
ggplot(pred_xgb, aes(x = .pred_1)) +
  geom_histogram(bins = 30) +
  labs(title = "Predicted Probability of 'Valid' on Real Data")

ggplot(test_preds_xgb, aes(x = .pred_1, fill = valid)) +
  geom_density(alpha = 0.4) + 
  labs(title = "Comparing Feature Distributions")

ggplot(bind_rows(
  synthetic_data %>% mutate(dataset = "synthetic"),
  real_features %>% mutate(dataset = "real")
),
aes(x = mean_entropy_drop, fill = dataset)) +
  geom_density(alpha = 0.4)






