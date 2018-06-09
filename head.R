library(readr)
library(stringr)
library(dplyr)
library(nleqslv)
library(tictoc)

# helper functions
`%+%` <- function(x, y){
  paste0(x, y)
}

## generate scores for different player's strength
get_scores <- function(strength1, strength2, stronger_always_win=F){
  out <- tibble("score1"=numeric(), "score2"=numeric())
  
  for(i in 1:length(strength1)){
    diff <- sigmoid(strength1[i] - strength2[i])
    if(stronger_always_win){
      score1 <- as.numeric(strength1[i] > strength2[i])
    }else{
      score1 <- sample(0:1, 1, prob=c(1 - diff, diff))
    }
    score2 <- abs(score1 - 1)
    out %>% 
      bind_rows(tibble("score1"=score1, "score2"=score2)) ->
      out
  }
  out
}

## comupte matches and results matrices
ranking_problem <- function(matches.f=matches.f){
  players <- unique(c(matches.f$team1, matches.f$team2))
  
  matches.f %>% 
    filter(score1 > score2) ->
    mtch1
  
  matches.f %>% 
    filter(score1 < score2) %>% 
    rename(team11 = team1, score11 = score1) %>% 
    transmute(team1 = team2, team2 = team11, score1 = score2, score2 = score11) ->
    mtch2
  
  mtch1 %>% 
    bind_rows(mtch2) ->
    matches.f
  
  results_matrix <- matrix(0, nrow=length(players), ncol=length(players), dimnames=list(players, players))
  
  for(player in players){
    matches.f %>% 
      filter(team1 == player) %>% 
      pull(team2) ->
      loosers
    if(length(loosers)!=0){
      results_matrix[player, unique(loosers)] <- sapply(unique(loosers), function(x) {sum(loosers==x)})
    }
  }
  
  matches_matrix <- results_matrix + t(results_matrix)
  
  number_of_matches_played <- apply(matches_matrix, 1, sum)  # m
  average_scores <- apply(results_matrix, 1, sum) / number_of_matches_played # s
  
  list(
    "matches_matrix"=matches_matrix,
    "results_matrix"=results_matrix,
    "number_of_matches_played"=number_of_matches_played,
    "average_scores"=average_scores,
    "players"=players,
    "matches_f"=matches.f)
}

# Maximum likelihood method
maximum_likelihood <- function(x){
  x <- x[-length(x)]
  y <- numeric(n_teams)
  denom <- numeric(n_teams)
  for(i in 1:n_teams){
    d <- numeric(n_teams)  # tworzy zera (na pewno będzie jedno zero - dla j==i)
    for(j in 1:n_teams){
      if(j != i){
        d[j] <- matches_matrix[i, j] / (x[i] + x[j])
      }
    }
    denom[i] <- sum(d)
  }
  
  y = number_of_matches_played * average_scores / denom
  
  return(c(y-x, sum(y)-1))
}

# Least squares method
least_squares <- function(x, penalty=0){
  # x <- x[-length(x)]
  out <- 0
  for(i in 1:length(x)){
    for(j in 1:length(x)){
      out <- out + matches_matrix[i, j] * (D[i, j] - (x[i] - x[j]))^2
    }
  }
  out <- out + abs(sum(x))*penalty
  return(out)
}


least_squares_wolfram <- function(x){
  # x <- x[-length(x)]
  out <- ""
  for(i in 1:length(x)){
    for(j in 1:length(x)){
      if(i != j & matches_matrix[i, j] != 0){
        out <- out %+% "+" %+% matches_matrix[i, j] %+% "*" %+% "(" %+% D[i, j] %+% "-" %+% x[i] %+% "+" %+% x[j] %+% ")^2"
      }
    }
  }
  # out <- out + abs(sum(x))*1e15
  return(out)
  
  # (w <- fnl_wolfram(players))
  # w <- str_replace_all(w, "Germany", "A")
  # w <- str_replace_all(w, "Argentina", "B")
  # w <- str_replace_all(w, "Netherlands", "C")
  # w <- str_replace_all(w, "Brazil", "D")
  # w <- str_replace_all(w, "Costa Rica", "E")
  # w <- str_replace_all(w, "Belgium", "F")
  # w <- str_replace_all(w, "Colombia", "G")
  # w <- str_replace_all(w, "France", "H")
  # w
  # w <- str_replace_all(w, "\\+1\\*\\(", "+(")
  # w <- str_replace_all(w, "0\\-", "\\-")
  # w
}


# Recursive Buchholz
# tournament - results matrix
recursive_buchholz <- function(tournament){ # tournament odpowiada macierzy A z artyułu
  n <- dim(tournament)[1]
  
  matches_matrix <- tournament + t(tournament) # macierz M
  matches_sums <- rowSums(matches_matrix) # wektor m
  scores <- rowSums(tournament, na.rm = TRUE)/matches_sums #wektor s
  
  matches_centered <- matches_matrix/matches_sums - diag(n) #Macierz (M z kreską) - I
  scores_moved <- scores - 0.5 #wektor (s z daszkiem)
  
  #Dodajemy poniżej warunek, że mają się sumować do zera w następujący sposób:
  # dodajemy wiersz jedynek na dole (będzie sumował wszystie wyrazy) i 0 na końcu wyników (żeby sumowłąo się do 0). Żeby macierz była kwadratowa dorzucamy wektor jedynek po prawej (będą mnożone przez to zero więc to może byc cokolwiek różnego od zera)
  matches_centered <- rbind(matches_centered, rep(1, n))
  matches_centered <- cbind(matches_centered, rep(1, n+1))
  scores_moved <- c(scores_moved, 0)
  
  solve(matches_centered, -scores_moved)[1:n]
}

























