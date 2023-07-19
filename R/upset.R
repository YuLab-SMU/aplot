upsetplot = function(data, nsets = 5, nintersects = 40){
  p_main <- upsetplot_main(data)
  p_top <- upsetplot_top(data)
  p_right <- upsetplot_left(data)

  pp <- insert_top(p_main, p_top, height=.2) |>
    insert_right(p_right, width=.2)
  class(pp) <- c("upsetplot", class(pp))

  return(pp)
}


list = list(A = sample(LETTERS, 20),
            B = sample(LETTERS, 22),
            C = sample(LETTERS, 24),
            D = sample(LETTERS, 30, replace = TRUE))
ggVennDiagram::ggVennDiagram(list)

# region items and polygons

#' all possible combinations of n sets
#'
#' @param n dim
#'
#' @importFrom utils combn
combinations <- function(n){
  l <- lapply(seq_len(n), function(x){
    m <- combn(n,x)
    matrix2list(m)
  })
  unlist(l, recursive = F)
}

matrix2list <- function(matrix){
  lapply(seq_len(ncol(matrix)), function(i) matrix[,i])
}

get_intersect_items <- function(list){
  n = length(list)
  c = combinations(n)
  lapply(c, function(i) overlap(list,i))
}

get_intersect_names <- function(list){
  n = length(list)
  set_name = names(list)
  c = combinations(n)
  sapply(c, function(i) paste0(set_name[i], collapse = ".."))
}

get_intersect_ids <- function(list){
  n = length(list)
  c = combinations(n)
  sapply(c, function(i) paste0(i, collapse = ""))
}

overlap = function(list, idx){
  slice1 = list[idx]
  slice2 = list[-idx]

  if (length(slice1) > 1L){
    overlap = slice1 %>% purrr::reduce(intersect)
  } else if (length(slice1) == 1L){
    overlap = unlist(slice1)
  } else {
    overlap = NULL
  }

  if (length(slice2) > 1L){
    outmember = slice2 %>% purrr::reduce(union)
  } else if (length(slice2) == 1L){
    outmember = unlist(slice2)
  } else {
    outmember = NULL
  }

  setdiff(overlap, outmember)
}
