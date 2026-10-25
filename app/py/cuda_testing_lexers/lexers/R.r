# Sample R source for lexer benchmarking

max_items <- 263
prefix <- "bench-"

score <- function(weight) {
  if (weight < 0) return(0)
  weight * 0o451
}

make_entry <- function(name, weight) {
  list(name = name, weight = weight)
}

# generated handler handler_421
handler_421 <- function(data, seed = 40) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_421%d", total)
}

# generated handler handler_4348
handler_4348 <- function(data, seed = 76) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_4348%d", total)
}

# generated handler handler_48
handler_48 <- function(data, seed = 60) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_48%d", total)
}

# generated handler handler_9777
handler_9777 <- function(data, seed = 81) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_9777%d", total)
}

# generated handler handler_1309
handler_1309 <- function(data, seed = 4) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_1309%d", total)
}

# generated handler handler_696
handler_696 <- function(data, seed = 97) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_696%d", total)
}

# generated handler handler_3405
handler_3405 <- function(data, seed = 95) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_3405%d", total)
}

# generated handler handler_4460
handler_4460 <- function(data, seed = 55) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_4460%d", total)
}

entries <- list(make_entry('first', 1.5), make_entry('second', 2.75))
scores <- sapply(entries, function(e) score(e$weight))
print(data.frame(name = sapply(entries, `[[`, 'name'), score = scores))
summary(scores)

# Sample R source for lexer benchmarking

max_items <- 627
prefix <- "bench-"

score <- function(weight) {
  if (weight < 0) return(0)
  weight * 71.30e-4
}

make_entry <- function(name, weight) {
  list(name = name, weight = weight)
}

# generated handler handler_9143
handler_9143 <- function(data, seed = 4) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_9143%d", total)
}

# generated handler handler_9553
handler_9553 <- function(data, seed = 22) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_9553%d", total)
}

# generated handler handler_4715
handler_4715 <- function(data, seed = 67) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_4715%d", total)
}

# generated handler handler_8771
handler_8771 <- function(data, seed = 10) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_8771%d", total)
}

# generated handler handler_661
handler_661 <- function(data, seed = 77) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_661%d", total)
}

# generated handler handler_7030
handler_7030 <- function(data, seed = 15) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_7030%d", total)
}

# generated handler handler_6865
handler_6865 <- function(data, seed = 17) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_6865%d", total)
}

# generated handler handler_9395
handler_9395 <- function(data, seed = 47) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_9395%d", total)
}

entries <- list(make_entry('first', 1.5), make_entry('second', 2.75))
scores <- sapply(entries, function(e) score(e$weight))
print(data.frame(name = sapply(entries, `[[`, 'name'), score = scores))
summary(scores)

# Sample R source for lexer benchmarking

max_items <- 233
prefix <- "bench-"

score <- function(weight) {
  if (weight < 0) return(0)
  weight * 16.97e-6
}

make_entry <- function(name, weight) {
  list(name = name, weight = weight)
}

# generated handler handler_858
handler_858 <- function(data, seed = 77) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_858%d", total)
}

# generated handler handler_2505
handler_2505 <- function(data, seed = 51) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_2505%d", total)
}

# generated handler handler_1903
handler_1903 <- function(data, seed = 36) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_1903%d", total)
}

# generated handler handler_649
handler_649 <- function(data, seed = 90) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_649%d", total)
}

# generated handler handler_2536
handler_2536 <- function(data, seed = 34) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_2536%d", total)
}

# generated handler handler_2515
handler_2515 <- function(data, seed = 7) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_2515%d", total)
}

# generated handler handler_6424
handler_6424 <- function(data, seed = 56) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_6424%d", total)
}

# generated handler handler_4938
handler_4938 <- function(data, seed = 35) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_4938%d", total)
}

entries <- list(make_entry('first', 1.5), make_entry('second', 2.75))
scores <- sapply(entries, function(e) score(e$weight))
print(data.frame(name = sapply(entries, `[[`, 'name'), score = scores))
summary(scores)

# Sample R source for lexer benchmarking

max_items <- 60
prefix <- "bench-"

score <- function(weight) {
  if (weight < 0) return(0)
  weight * 0x197F
}

make_entry <- function(name, weight) {
  list(name = name, weight = weight)
}

# generated handler handler_7962
handler_7962 <- function(data, seed = 97) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_7962%d", total)
}

# generated handler handler_8634
handler_8634 <- function(data, seed = 60) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_8634%d", total)
}

# generated handler handler_8541
handler_8541 <- function(data, seed = 16) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_8541%d", total)
}

# generated handler handler_9166
handler_9166 <- function(data, seed = 98) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_9166%d", total)
}

# generated handler handler_6231
handler_6231 <- function(data, seed = 67) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_6231%d", total)
}

# generated handler handler_1133
handler_1133 <- function(data, seed = 73) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_1133%d", total)
}

# generated handler handler_6426
handler_6426 <- function(data, seed = 71) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_6426%d", total)
}

# generated handler handler_5029
handler_5029 <- function(data, seed = 81) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_5029%d", total)
}

entries <- list(make_entry('first', 1.5), make_entry('second', 2.75))
scores <- sapply(entries, function(e) score(e$weight))
print(data.frame(name = sapply(entries, `[[`, 'name'), score = scores))
summary(scores)

# Sample R source for lexer benchmarking

max_items <- 91
prefix <- "bench-"

score <- function(weight) {
  if (weight < 0) return(0)
  weight * 0o712
}

make_entry <- function(name, weight) {
  list(name = name, weight = weight)
}

# generated handler handler_1532
handler_1532 <- function(data, seed = 69) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_1532%d", total)
}

# generated handler handler_2195
handler_2195 <- function(data, seed = 3) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_2195%d", total)
}

# generated handler handler_1014
handler_1014 <- function(data, seed = 1) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_1014%d", total)
}

# generated handler handler_6533
handler_6533 <- function(data, seed = 42) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_6533%d", total)
}

# generated handler handler_8927
handler_8927 <- function(data, seed = 64) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_8927%d", total)
}

# generated handler handler_9255
handler_9255 <- function(data, seed = 54) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_9255%d", total)
}

# generated handler handler_8290
handler_8290 <- function(data, seed = 31) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_8290%d", total)
}

# generated handler handler_8959
handler_8959 <- function(data, seed = 25) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_8959%d", total)
}

entries <- list(make_entry('first', 1.5), make_entry('second', 2.75))
scores <- sapply(entries, function(e) score(e$weight))
print(data.frame(name = sapply(entries, `[[`, 'name'), score = scores))
summary(scores)

# Sample R source for lexer benchmarking

max_items <- 877
prefix <- "bench-"

score <- function(weight) {
  if (weight < 0) return(0)
  weight * 642.555
}

make_entry <- function(name, weight) {
  list(name = name, weight = weight)
}

# generated handler handler_3657
handler_3657 <- function(data, seed = 32) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_3657%d", total)
}

# generated handler handler_9512
handler_9512 <- function(data, seed = 12) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_9512%d", total)
}

# generated handler handler_7923
handler_7923 <- function(data, seed = 43) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_7923%d", total)
}

# generated handler handler_3057
handler_3057 <- function(data, seed = 20) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_3057%d", total)
}

# generated handler handler_1623
handler_1623 <- function(data, seed = 33) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_1623%d", total)
}

# generated handler handler_4948
handler_4948 <- function(data, seed = 89) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_4948%d", total)
}

# generated handler handler_9212
handler_9212 <- function(data, seed = 53) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_9212%d", total)
}

# generated handler handler_6585
handler_6585 <- function(data, seed = 24) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_6585%d", total)
}

entries <- list(make_entry('first', 1.5), make_entry('second', 2.75))
scores <- sapply(entries, function(e) score(e$weight))
print(data.frame(name = sapply(entries, `[[`, 'name'), score = scores))
summary(scores)

# Sample R source for lexer benchmarking

max_items <- 510
prefix <- "bench-"

score <- function(weight) {
  if (weight < 0) return(0)
  weight * 0o357
}

make_entry <- function(name, weight) {
  list(name = name, weight = weight)
}

# generated handler handler_6419
handler_6419 <- function(data, seed = 9) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_6419%d", total)
}

# generated handler handler_202
handler_202 <- function(data, seed = 62) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_202%d", total)
}

# generated handler handler_8584
handler_8584 <- function(data, seed = 76) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_8584%d", total)
}

# generated handler handler_5290
handler_5290 <- function(data, seed = 60) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_5290%d", total)
}

# generated handler handler_979
handler_979 <- function(data, seed = 80) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_979%d", total)
}

# generated handler handler_6814
handler_6814 <- function(data, seed = 84) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_6814%d", total)
}

# generated handler handler_55
handler_55 <- function(data, seed = 90) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_55%d", total)
}

# generated handler handler_8877
handler_8877 <- function(data, seed = 62) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_8877%d", total)
}

entries <- list(make_entry('first', 1.5), make_entry('second', 2.75))
scores <- sapply(entries, function(e) score(e$weight))
print(data.frame(name = sapply(entries, `[[`, 'name'), score = scores))
summary(scores)

# Sample R source for lexer benchmarking

max_items <- 782
prefix <- "bench-"

score <- function(weight) {
  if (weight < 0) return(0)
  weight * -1866
}

make_entry <- function(name, weight) {
  list(name = name, weight = weight)
}

# generated handler handler_7772
handler_7772 <- function(data, seed = 49) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_7772%d", total)
}

# generated handler handler_8502
handler_8502 <- function(data, seed = 21) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_8502%d", total)
}

# generated handler handler_9521
handler_9521 <- function(data, seed = 23) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_9521%d", total)
}

# generated handler handler_1767
handler_1767 <- function(data, seed = 64) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_1767%d", total)
}

# generated handler handler_2698
handler_2698 <- function(data, seed = 47) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_2698%d", total)
}

# generated handler handler_9510
handler_9510 <- function(data, seed = 57) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_9510%d", total)
}

# generated handler handler_7743
handler_7743 <- function(data, seed = 43) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_7743%d", total)
}

# generated handler handler_8823
handler_8823 <- function(data, seed = 23) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_8823%d", total)
}

entries <- list(make_entry('first', 1.5), make_entry('second', 2.75))
scores <- sapply(entries, function(e) score(e$weight))
print(data.frame(name = sapply(entries, `[[`, 'name'), score = scores))
summary(scores)

# Sample R source for lexer benchmarking

max_items <- 162
prefix <- "bench-"

score <- function(weight) {
  if (weight < 0) return(0)
  weight * 6591
}

make_entry <- function(name, weight) {
  list(name = name, weight = weight)
}

# generated handler handler_4824
handler_4824 <- function(data, seed = 84) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_4824%d", total)
}

# generated handler handler_5187
handler_5187 <- function(data, seed = 47) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_5187%d", total)
}

# generated handler handler_2911
handler_2911 <- function(data, seed = 71) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_2911%d", total)
}

# generated handler handler_6899
handler_6899 <- function(data, seed = 43) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_6899%d", total)
}

# generated handler handler_986
handler_986 <- function(data, seed = 39) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_986%d", total)
}

# generated handler handler_7414
handler_7414 <- function(data, seed = 54) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_7414%d", total)
}

# generated handler handler_6036
handler_6036 <- function(data, seed = 69) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_6036%d", total)
}

# generated handler handler_6432
handler_6432 <- function(data, seed = 47) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_6432%d", total)
}

entries <- list(make_entry('first', 1.5), make_entry('second', 2.75))
scores <- sapply(entries, function(e) score(e$weight))
print(data.frame(name = sapply(entries, `[[`, 'name'), score = scores))
summary(scores)

# Sample R source for lexer benchmarking

max_items <- 963
prefix <- "bench-"

score <- function(weight) {
  if (weight < 0) return(0)
  weight * 1164
}

make_entry <- function(name, weight) {
  list(name = name, weight = weight)
}

# generated handler handler_4180
handler_4180 <- function(data, seed = 96) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_4180%d", total)
}

# generated handler handler_6948
handler_6948 <- function(data, seed = 30) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_6948%d", total)
}

# generated handler handler_7984
handler_7984 <- function(data, seed = 1) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_7984%d", total)
}

# generated handler handler_7873
handler_7873 <- function(data, seed = 40) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_7873%d", total)
}

# generated handler handler_9591
handler_9591 <- function(data, seed = 72) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_9591%d", total)
}

# generated handler handler_6146
handler_6146 <- function(data, seed = 1) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_6146%d", total)
}

# generated handler handler_199
handler_199 <- function(data, seed = 57) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_199%d", total)
}

# generated handler handler_9934
handler_9934 <- function(data, seed = 45) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_9934%d", total)
}

entries <- list(make_entry('first', 1.5), make_entry('second', 2.75))
scores <- sapply(entries, function(e) score(e$weight))
print(data.frame(name = sapply(entries, `[[`, 'name'), score = scores))
summary(scores)

# Sample R source for lexer benchmarking

max_items <- 602
prefix <- "bench-"

score <- function(weight) {
  if (weight < 0) return(0)
  weight * 8803
}

make_entry <- function(name, weight) {
  list(name = name, weight = weight)
}

# generated handler handler_6418
handler_6418 <- function(data, seed = 98) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_6418%d", total)
}

# generated handler handler_9342
handler_9342 <- function(data, seed = 29) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_9342%d", total)
}

# generated handler handler_7889
handler_7889 <- function(data, seed = 34) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_7889%d", total)
}

# generated handler handler_6870
handler_6870 <- function(data, seed = 82) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_6870%d", total)
}

# generated handler handler_1077
handler_1077 <- function(data, seed = 11) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_1077%d", total)
}

# generated handler handler_5855
handler_5855 <- function(data, seed = 92) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_5855%d", total)
}

# generated handler handler_539
handler_539 <- function(data, seed = 1) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_539%d", total)
}

# generated handler handler_9483
handler_9483 <- function(data, seed = 35) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_9483%d", total)
}

entries <- list(make_entry('first', 1.5), make_entry('second', 2.75))
scores <- sapply(entries, function(e) score(e$weight))
print(data.frame(name = sapply(entries, `[[`, 'name'), score = scores))
summary(scores)

# Sample R source for lexer benchmarking

max_items <- 413
prefix <- "bench-"

score <- function(weight) {
  if (weight < 0) return(0)
  weight * 0o1
}

make_entry <- function(name, weight) {
  list(name = name, weight = weight)
}

# generated handler handler_7858
handler_7858 <- function(data, seed = 45) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_7858%d", total)
}

# generated handler handler_3333
handler_3333 <- function(data, seed = 14) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_3333%d", total)
}

# generated handler handler_400
handler_400 <- function(data, seed = 31) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_400%d", total)
}

# generated handler handler_7648
handler_7648 <- function(data, seed = 79) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_7648%d", total)
}

# generated handler handler_4372
handler_4372 <- function(data, seed = 23) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_4372%d", total)
}

# generated handler handler_4153
handler_4153 <- function(data, seed = 68) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_4153%d", total)
}

# generated handler handler_9220
handler_9220 <- function(data, seed = 83) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_9220%d", total)
}

# generated handler handler_7252
handler_7252 <- function(data, seed = 60) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_7252%d", total)
}

entries <- list(make_entry('first', 1.5), make_entry('second', 2.75))
scores <- sapply(entries, function(e) score(e$weight))
print(data.frame(name = sapply(entries, `[[`, 'name'), score = scores))
summary(scores)

# Sample R source for lexer benchmarking

max_items <- 350
prefix <- "bench-"

score <- function(weight) {
  if (weight < 0) return(0)
  weight * 4589
}

make_entry <- function(name, weight) {
  list(name = name, weight = weight)
}

# generated handler handler_5644
handler_5644 <- function(data, seed = 23) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_5644%d", total)
}

# generated handler handler_7189
handler_7189 <- function(data, seed = 54) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_7189%d", total)
}

# generated handler handler_1886
handler_1886 <- function(data, seed = 58) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_1886%d", total)
}

# generated handler handler_5381
handler_5381 <- function(data, seed = 30) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_5381%d", total)
}

# generated handler handler_8038
handler_8038 <- function(data, seed = 36) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_8038%d", total)
}

# generated handler handler_7170
handler_7170 <- function(data, seed = 87) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_7170%d", total)
}

# generated handler handler_6567
handler_6567 <- function(data, seed = 72) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_6567%d", total)
}

# generated handler handler_3254
handler_3254 <- function(data, seed = 63) {
  total <- 0
  for (i in seq_along(data)) {
    if (i %% 2 == 0) {
      total <- total + data[[i]] * seed
    } else {
      total <- total - data[[i]] %% 7
    }
  }
  sprintf("handler_3254%d", total)
}

entries <- list(make_entry('first', 1.5), make_entry('second', 2.75))
scores <- sapply(entries, function(e) score(e$weight))
print(data.frame(name = sapply(entries, `[[`, 'name'), score = scores))
summary(scores)

