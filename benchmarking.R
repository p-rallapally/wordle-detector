library(microbenchmark)

solution   <- sample(solutions, 1)
guess      <- sample(guesses, 1)
candidates <- sample(guesses, 2000)  # realistic candidate size
feedback   <- get_feedback(guess, solution)

set.seed(1302026)
microbenchmark(
  get_feedback_matrix(guess_chars, candidates_matrix),
  times = 500
)
#28 microsecs

microbenchmark(
  filter_candidates_faster(candidates, guess, feedback),
  times = 50
) #67 microsecs


microbenchmark(
  simulate_valid_game(solution, guesses, guesses_split),
  times = 5
) #553 microsecs

microbenchmark(
  simulate_invalid_game(solution, guesses, guesses_split),
  times = 5
) #530 microsecs


benchmark_iteration <- function(solution, word_list) {
  candidates <- word_list
  
  guess <- sample(candidates, 1)
  feedback <- get_feedback(guess, solution)
  
  microbenchmark(
    filter = filter_candidates(candidates, guess, feedback), #somehow, this is the bottleneck
    entropy = entropy(length(candidates)),
    times = 20
  )
}

benchmark_iteration(solution, guesses)













