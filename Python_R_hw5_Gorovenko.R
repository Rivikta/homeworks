motifs2 <- matrix(c(
  "a", "C", "g", "G", "T", "A", "A", "t", "t", "C", "a", "G",
  "t", "G", "G", "G", "C", "A", "A", "T", "t", "C", "C", "a",
  "A", "C", "G", "t", "t", "A", "A", "t", "t", "C", "G", "G",
  "T", "G", "C", "G", "G", "G", "A", "t", "t", "C", "C", "C",
  "t", "C", "G", "a", "A", "A", "A", "t", "t", "C", "a", "G",
  "A", "C", "G", "G", "C", "G", "A", "a", "t", "T", "C", "C",
  "T", "C", "G", "t", "G", "A", "A", "t", "t", "a", "C", "G",
  "t", "C", "G", "G", "G", "A", "A", "t", "t", "C", "a", "C",
  "A", "G", "G", "G", "T", "A", "A", "t", "t", "C", "C", "G",
  "t", "C", "G", "G", "A", "A", "A", "a", "t", "C", "a", "C"
), nrow = 10, byrow = TRUE)

motifs2 <- toupper(motifs2)

count_matrix <- apply(motifs2, 2, function(col) table(factor(col, levels = c("A", "C", "G", "T"))))
count_matrix <- t(count_matrix)

profile_matrix <- apply(count_matrix, 2, function(col) col / sum(col))
profile_matrix <- t(profile_matrix)

scoreMotifs <- function(motifs, profile) {
  score <- 0
  for (i in 1:nrow(motifs)) {
    for (pos in 1:ncol(motifs)) {
      nuc <- motifs[i, pos]
      score <- score + log2(profile[nuc, pos])
    }
  }
  return(score)
}

score <- scoreMotifs(motifs2, profile_matrix)

getConsensus <- function(count_matrix) {
  consensus <- ""
  for (j in 1:ncol(count_matrix)) {
    max_nuc <- names(which.max(count_matrix[, j]))
    consensus <- paste0(consensus, max_nuc)
  }
  return(consensus)
}

consensus_seq <- getConsensus(count_matrix)

col1_counts <- count_matrix[, 1]
barplot(col1_counts, 
        col = "skyblue", 
        main = "Частоты нуклеотидов в 1-м столбце",
        xlab = "Нуклеотиды",
        ylab = "Частота",
        ylim = c(0, max(col1_counts) * 1.1))
