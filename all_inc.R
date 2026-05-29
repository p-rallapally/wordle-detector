get_feedback_matrix_single_guess <- function(guess_chars, all_chars_matrix) {
  n <- nrow(all_chars_matrix)
  feedback <- matrix(0L, n, 5)
  used <- matrix(FALSE, n, 5)
  
  # Greens
  for (i in 1:5) {
    hit <- all_chars_matrix[, i] == guess_chars[i]
    feedback[hit, i] <- 2L
    used[hit, i] <- TRUE
  }
  
  # Yellows
  for (i in 1:5) {
    need <- feedback[, i] == 0L
    if (!any(need)) next
    
    for (j in 1:5) {
      ok <- need & !used[, j] & all_chars_matrix[, j] == guess_chars[i]
      if (any(ok)) {
        feedback[ok, i] <- 1L
        used[ok, j] <- TRUE
        need <- need & !ok
      }
    }
  }
  
  feedback
}


precompute_feedback_tensor <- function(guesses_split) {
  n <- length(guesses_split)
  all_chars_matrix <- do.call(rbind, guesses_split)
  
  tensor <- array(0L, dim = c(n, n, 5))
  
  for (i in seq_len(n)) {
    tensor[i, , ] <- get_feedback_matrix_single_guess(
      guesses_split[[i]],
      all_chars_matrix
    )
  }
  
  list(
    tensor = tensor,
    char_matrix = all_chars_matrix
  )
}

filter_candidates_fast <- function(
    candidates_idx,
    guess_idx,
    observed_feedback,
    feedback_tensor
) {
  fb <- feedback_tensor[guess_idx, candidates_idx, , drop = FALSE]
  keep <- apply(fb, 1, function(row) all(row == observed_feedback))
  candidates_idx[keep]
}

init_state <- function() {
  list(
    greens = rep(NA_character_, 5),
    yellows = vector("list", 5),
    used_letters = character(0)
  )
}

update_state <- function(state, guess_chars, feedback) {
  for (i in 1:5) {
    if (feedback[i] > 0) {
      state$used_letters <- union(state$used_letters, guess_chars[i])
    }
    if (feedback[i] == 2) {
      state$greens[i] <- guess_chars[i]
    }
    if (feedback[i] == 1) {
      state$yellows[[i]] <- union(state$yellows[[i]], guess_chars[i])
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
  
  0.6 * novelty + 0.3 * freq_score + 0.1 * green_score
}

entropy_cache <- new.env(parent = emptyenv())

compute_entropies_cached <- function(candidates_idx, feedback_tensor) {
  key <- paste(candidates_idx, collapse = ",")
  
  if (exists(key, envir = entropy_cache)) {
    return(entropy_cache[[key]])
  }
  
  n <- length(candidates_idx)
  if (n <= 1) return(0)
  
  p3 <- 3^(4:0)
  ent <- numeric(n)
  
  for (k in seq_len(n)) {
    fb <- feedback_tensor[candidates_idx[k], candidates_idx, ]
    keys <- fb %*% p3
    counts <- tabulate(keys + 1L, nbins = 243)
    probs <- counts[counts > 0] / n
    ent[k] <- -sum(probs * log2(probs))
  }
  
  entropy_cache[[key]] <- ent
  ent
}

extract_features <- function(history) {
  ent <- vapply(history, `[[`, numeric(1), "entropy_before")
  drops <- -diff(ent)
  
  tibble(
    n_guesses = length(history),
    entropy_start = ent[1],
    entropy_end = tail(ent, 1),
    mean_entropy_drop = mean(drops, na.rm = TRUE),
    max_entropy_drop = max(drops, na.rm = TRUE),
    early_entropy_drop = ifelse(length(drops) >= 2, mean(drops[1:2]), NA),
    monotone_entropy = all(drops >= -1e-6),
    green_fraction = mean(vapply(history, \(h) mean(h$feedback == 2), 0)),
    yellow_fraction = mean(vapply(history, \(h) mean(h$feedback == 1), 0))
  )
}

simulate_game <- function(
    solution_idx,
    word_list,
    guesses_split,
    feedback_tensor,
    invalid = FALSE
) {
  candidates_idx <- seq_along(word_list)
  state <- init_state()
  history <- list()
  
  for (g in seq_len(MAX_GUESSES)) {
    if (length(candidates_idx) == 0) break
    
    entropy_before <- log2(length(candidates_idx))
    
    if (!invalid) {
      lf <- letter_freq(candidates_idx, guesses_split)
      scores <- vapply(
        candidates_idx,
        \(i) score_valid_guess(guesses_split[[i]], state, lf),
        numeric(1)
      )
      guess_idx <- candidates_idx[which.max(scores)]
      
      if (runif(1) < 0.15) {
        guess_idx <- sample(candidates_idx, 1)
      }
    } else {
      if (g == 1) {
        guess_idx <- match(OPTIMAL_FIRST_GUESS, word_list)
      } else {
        ent <- compute_entropies_cached(candidates_idx, feedback_tensor)
        guess_idx <- candidates_idx[which.max(ent)]
      }
    }
    
    feedback <- feedback_tensor[guess_idx, solution_idx, ]
    state <- update_state(state, guesses_split[[guess_idx]], feedback)
    
    candidates_idx <- filter_candidates_fast(
      candidates_idx,
      guess_idx,
      feedback,
      feedback_tensor
    )
    
    history[[g]] <- list(
      guess = word_list[guess_idx],
      feedback = feedback,
      entropy_before = entropy_before
    )
    
    if (guess_idx == solution_idx) break
  }
  
  extract_features(history)
}

simulate_valid_game <- function(solution, word_list, guesses_split, feedback_tensor) {
  simulate_game(
    match(solution, word_list),
    word_list,
    guesses_split,
    feedback_tensor,
    invalid = FALSE
  )
}

simulate_invalid_game <- function(solution, word_list, guesses_split, feedback_tensor) {
  simulate_game(
    match(solution, word_list),
    word_list,
    guesses_split,
    feedback_tensor,
    invalid = TRUE
  )
}

guesses <- clean_word_list(scan("combined_wordlist.txt", what = character()))
solutions <- clean_word_list(scan("shuffled_real_wordles.txt", what = character()))
guesses_split <- strsplit(guesses, "")

fb <- precompute_feedback_tensor(guesses_split)

set.seed(123)

system.time(
  simulate_valid_game(sample(solutions, 1), guesses, guesses_split, fb$tensor)
)










