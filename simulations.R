ranking <- function(tournament){ # tournament odpowiada macierzy A z artyułu
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

kendall <- function(x, y){ #cor(..., method = 'kendall') nie działa na factorach, etyiety chyba tak właśnie należy interpretować, więc napisałem funkcję sam
  x <- as.factor(x)
  y <- as.factor(y)
  
  n <- length(x)
  M <- matrix(integer(length(x)^2), nrow = n)
  
  for(i in 1:(n - 1)){
    for(j in (i + 1):n){
      pos.x <- match(levels(x)[i], x) - match(levels(x)[j], x)
      pos.y <- match(levels(y)[i], y) - match(levels(y)[j], y)
      
      M[i,j] <- sign(pos.x * pos.y)
    }
  }
  
  sum(M)/(n*(n-1)/2)
}

round_robin <- function(true_ranks, k){#symulcja k rund kazdy z każdym
  n <- length(true_ranks)
  A <- matrix(numeric(n*n), nrow = n)
  
  for(r in 1:k){
    for(i in 1:(n-1)){
      for(j in (i+1):n){
        tmp_prob <- e1071::sigmoid(true_ranks[i] - true_ranks[j])
        result <- sample(c(1, 0), size = 1, prob = c(tmp_prob, 1 - tmp_prob))
        
        A[i, j] <- A[i, j] + result
        A[j, i] <- A[j, i] + 1 - result
      }
    }
  }
  factor(order(ranking(A), decreasing = TRUE))
}

circle <- function(true_ranks, n){# Po kółeczku (1 gra z 28 i 2, 2 gra z 1 i 3 itd.) a potem losowo n meczy
  k <- length(true_ranks)
  A1 <- matrix(numeric(k*k), nrow = k)
  
  tmp_prob <- e1071::sigmoid(true_ranks[1] - true_ranks[k])
  A1[1, k] <- sample(c(1, 0), size = 1, prob = c(tmp_prob, 1 - tmp_prob))
  A1[k, 1] <- 1 - A1[1, k]
  
  for(i in 1:k-1){
    tmp_prob <- e1071::sigmoid(true_ranks[i] - true_ranks[i+1])
    A1[i, i+1] <- sample(c(1, 0), size = 1, prob = c(tmp_prob, 1 - tmp_prob))
    A1[i+1, i] <- 1 - A1[i, i+1] 
  }
  
  for(i in 1:n){
    teams <- sample(1:k, 2, replace = FALSE)
    tmp_prob <- e1071::sigmoid(true_ranks[teams[1]] - true_ranks[teams[2]])
    A1[teams[1], teams[2]] <- sample(c(1, 0), size = 1, prob = c(tmp_prob, 1 - tmp_prob))
    A1[teams[2], teams[1]] <- 1 - A1[teams[1], teams[2]] 
  }
  
  tournament_ranks <- ranking(A1)
  factor(order(tournament_ranks, decreasing = TRUE))
}



# 8 drużyn każdy z każdym
true8 <- runif(8, -1.2, 1.2)
order8 <- factor(order(true8, decreasing = TRUE))

simulation8 <- sapply(seq(1, 101, 10), function(x) replicate(50, kendall(order8, round_robin(true8, x))))
simulation8 <- as.data.frame(simulation)
long_simulation8 <- tidyr::gather(simulation8, rounds, kendall)

# 28 drużyn każdy z każdym
true28 <- runif(28, -1.2, 1.2)
order28 <- factor(order(true28, decreasing = TRUE))

simulationdf <- sapply(seq(1, 101, 10), function(x) replicate(50, kendall(order28, round_robin(true28, x))))
simulationdf <- as.data.frame(simulationdf)
long_simulation <- tidyr::gather(simulationdf, rounds, kendall)

ggplot2::qplot(x = rounds, y = kendall, data = long_simulation, 
               ylim = c(0, 1), main = 'Turnieje każdy z każdym', xlab = 'Liczba rund', geom = 'point')

# 28 drużyn po kółeczku i losowo
many_sim <- function(n){
  out <- numeric(500)
  for(i in 1:500) out[i] <-  kendall(true_order, cirlce(true_ranks, n))
  out
}

library(parallel)
clust <- makeCluster(3)
clusterExport(clust, c("true_order", "true_ranks", "simulation", "ranking", "kendall"))

big_simulation <- as.data.frame(parSapply(clust, seq(0, 1000, 20), many_sim))

big_summary <- data.frame(up = sapply(big_simulation, mean) + sapply(big_simulation, sd),
                          avg = sapply(big_simulation, mean),
                          bott = sapply(big_simulation, mean) - sapply(big_simulation, sd))

plot(big_summary$avg, type = 'l', ylim = c(0, 0.85))
lines(big_summary$up, col = 'red')
lines(big_summary$bott, col = 'red')

