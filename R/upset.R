## (PART) Plot


#' Plot upsetplot
#'
#' @param data
#' @param nsets
#' @param nintersects
#'
#' @return
#' @export
#'
#' @examples
upsetplot = function(list, nsets = 5, nintersects = 40){
  primary_data = get_all_subsets(list)
  main_data = tidy_main_subsets(primary_data)
  p_main = upsetplot_main(main_data)
  top_data = tidy_top_subsets(primary_data)
  p_top = upsetplot_top(top_data)
  left_data = tidy_left_subsets(list)
  p_left = upsetplot_left(left_data)

  pp = insert_top(p_main, p_top, height=4)  %>%
    insert_left(p_left, width=.2)
  class(pp) <- c("upsetplot", class(pp))

  return(pp)
}

upsetplot_main = function(data){
  ggplot2::ggplot(data, aes(id, set)) +
    ggplot2::geom_point(size = 4, color = "grey30") +
    ggplot2::geom_path(aes(group = id), size = 1.5, color = "grey30") +
    theme_upsetplot()
}

upsetplot_top = function(data){
  ggplot2::ggplot(data, aes(id, item_count)) +
    ggplot2::geom_col() +
    theme_upsetplot()
}

upsetplot_left = function(data){
  ggplot2::ggplot(data, aes(y = set, x = item_count)) +
    ggplot2::geom_col(orientation = "y") +
    ggplot2::scale_y_discrete(position = "right") +
    theme_upsetplot()
}

theme_upsetplot = function(){
  ggplot2::theme_bw()
}

## (PART) debug

list = list(A = sample(LETTERS, 20),
            B = sample(LETTERS, 22),
            C = sample(LETTERS, 24),
            D = sample(LETTERS, 30, replace = TRUE))
ggVennDiagram::ggVennDiagram(list)


## (PART) retrieve tidy data from primary subset datasets

tidy_main_subsets = function(all_subsets, name_separator = "/"){
  all_subsets %>%
    dplyr::select(id, name) %>%
    dplyr::rename(set = "name") %>%
    tidyr::separate_longer_delim(set, delim = name_separator)
}

tidy_top_subsets = function(all_subsets, name_separator = "/"){
  all_subsets %>%
    dplyr::select(id, item) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(item_count = length(item))
}

tidy_left_subsets = function(list, set_name = names(list)){
  dplyr::tibble(
    set = set_name,
    item = list
  ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(item_count = length(item))
}

## (PART) build primary data of subsets

#' Get the items/names/ids of subsets from a named list
#'
#' @param list a named list
#' @param name_separator default is /
#'
#' @return
#' @export
#'
#' @examples
#' list = list(A = sample(LETTERS, 20),
#'             B = sample(LETTERS, 22),
#'             C = sample(LETTERS, 24),
#'             D = sample(LETTERS, 30, replace = TRUE))
#' get_intersect_names(list)
#' get_intersect_ids(list)
#' get_intersect_items(list)
get_all_subsets = function(list, name_separator = "/"){
  dplyr::tibble(
    id = get_all_subsets_ids(list, sep = name_separator),
    name = get_all_subsets_names(list, sep = name_separator),
    item = get_all_subsets_items(list)
  )
}

get_all_subsets_items <- function(list){
  n = length(list)
  c = combinations(n)
  lapply(c, function(i) overlap(list,i))
}

get_all_subsets_names <- function(list, sep = "/"){
  n = length(list)
  set_name = names(list)
  c = combinations(n)
  sapply(c, function(i) paste0(set_name[i], collapse = sep))
}

get_all_subsets_ids <- function(list, sep){
  n = length(list)
  c = combinations(n)
  sapply(c, function(i) paste0(i, collapse = sep))
}

## (PART) subset calculations: the basics

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
