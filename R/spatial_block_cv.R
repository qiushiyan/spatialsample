#' @references
#' Valavi, R, Elith, J, Lahoz-Monfort, JJ, Guillera-Arroita, G. blockCV:
#' An r package for generating spatially or environmentally separated folds for k-fold cross-validation of species distribution models.
#'  Methods Ecol Evol. 2019; 10: 225– 232.
#'  https://doi.org/10.1111/2041-210X.13107
#' @rdname spatial_clustering_cv
#' @export
spatial_block_cv <- function(data,
                             coords,
                             v = 10,
                             nrow = 6,
                             ncol = 5,
                             crs = 4326,
                             outcome = "",
                             allocation = c("random", "systematic", "checkboard"),
                             save_blocks = FALSE,
                             verbose = FALSE,
                             progress = FALSE,
                             show_blocks = FALSE,
                             ...) {
  coords <- tidyselect::eval_select(rlang::enquo(coords), data = data)
  if (is_empty(coords)) {
    rlang::abort("`coords` are required and must be variables in `data`.")
  }
  allocation <- match.arg(allocation)

  out <- spatial_block_splits(
    data = data,
    coords = coords,
    v = v,
    crs = crs,
    nrow = nrow,
    ncol = ncol,
    allocation = allocation,
    outcome = outcome,
    save_blocks = save_blocks,
    verbose = verbose,
    progress = progress,
    show_blocks = show_blocks,
    ...
  )

  split_objs <- out$split_objs

  split_objs$splits <- map(split_objs$splits, rm_out)

  cv_att <- list(v = v, repeats = 1)
  if (save_blocks) {
    cv_att$blocks <- out$blocks
  }

  new_rset(
    splits = split_objs$splits,
    ids = split_objs[, grepl("^id", names(split_objs))],
    attrib = cv_att,
    subclass = c("spatial_block_cv", "rset")
  )
}

spatial_block_splits <- function(data, coords, v, crs, nrow, ncol,
                                 allocation, outcome, save_blocks,
                                 verbose, progress, show_blocks, ...) {
  if (!inherits(data, "sf")) {
    data_sf <- sf::st_as_sf(data, coords = coords, crs = crs)
  }

  if (outcome != "") {
    blocks <- suppressWarnings(
        blockCV::spatialBlock(data_sf, outcome,
                              k = v, rows = nrow,
                              cols = ncol, selection = allocation, verbose = verbose,
                              progress = progress, showBlocks = show_blocks, ...
        )
    )
  } else {
    blocks <- suppressWarnings(
        blockCV::spatialBlock(data_sf,
                              k = v, rows = nrow, cols = ncol, selection = allocation,
                              verbose = verbose, progress = progress, showBlocks = show_blocks, ...
        )
    )
  }
  split_objs <- lapply(blocks$folds, setNames, c("analysis", "assesment"))
  split_objs <- purrr::map(split_objs, make_splits,
    data = data,
    class = "spatial_block_split"
  )
  split_objs <- tibble::tibble(
    splits = split_objs,
    id = names0(length(split_objs), "Fold")
  )
  if (save_blocks) {
    blocks_df <- sf::st_as_sf(blocks$blocks)
    blocks_df$layer <- NULL
    blocks_df$folds <- factor(blocks_df$folds)
    list(
      split_objs = split_objs,
      blocks = blocks_df
    )
  } else {
    list(
      split_objs = split_objs
    )
  }
}

#' @export
print.spatial_block_cv <- function(x, ...) {
  cat("# ", pretty(x), "\n")
  class(x) <- class(x)[!(class(x) %in% c("spatial_block_cv", "rset"))]
  print(x, ...)
}

