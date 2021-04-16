#' Estimate an EFAST model
#'
#' This function estimates efa models with residual covariance.
#'
#' @param data <data.frame> the dataset or <matrix> covariance matrix
#' @param M <numeric> How many factors, minimum 2
#' @param rstruct <list> residual structure (see details)
#' @param sample.nobs <numeric> sample size (if data = covmat, see lavaan)
#' @param auto.fix.first <bool> see lavaan
#' @param auto.var <bool> see lavaan
#' @param auto.efa <bool> see lavaan
#' @param information <character> see lavaan
#' @param std.ov <bool> see lavaan
#' @param ... other arguments passed to lavaan
#'
#' @details Residual structure (in the form of residual covariances) can be
#' added to the EFA through a list of pairs of variable names. See the example
#' for more information.
#'
#' @examples
#' \dontrun{
#' # Use a lavaan test dataset
#' test_data <- lavaan::HolzingerSwineford1939[,7:15]
#'
#' # create an EFA model
#' test_efa <- efast(test_data, 3)
#'
#' # create a (simple) residual structure
#' res_struct <- list(
#'   c("x4", "x7"),
#'   c("x5", "x9")
#' )
#'
#' # create an efast model
#' test_efast <- efast(test_data, 3, res_struct)
#'
#' compare the models
#' lavaan::lavTestLRT(test_efa, test_efast)
#' }
#'
#' @importFrom lavaan lavaan summary
#'
#' @export
efast <- function(data, M, rstruct, sample.nobs = NULL, auto.fix.first = FALSE,
                  auto.var = TRUE, auto.efa = TRUE, information = "observed",
                  std.ov = TRUE, ...) {
  is_df <- check_data_argument(data)
  if (!is.numeric(M) || M < 1 || M %% 1 != 0)
    stop("Input a positive integer for M (the number of factors)")
  
  if (!is_df) {
    S <- data
    data <- NULL
  } else {
    S <- NULL
  }
  if (missing(rstruct) || is.null(rstruct) || length(rstruct) == 0) {
    message("No residual structure specified, performing an EFA.")
    res <- efast_efa(data, M, sample.nobs = sample.nobs,
                     auto.fix.first = auto.fix.first,
                     auto.var = auto.var, auto.efa = auto.efa,
                     information = information, std.ov = std.ov, ...)
    return(res)
  }
  
  # generate the model syntax
  efa_mod <- if (is_df) efa_model(data, M) else efa_model(S, M)
  str_mod <- if (is_df) str_model(data, rstruct) else str_model(S, rstruct)
  model   <- paste(efa_mod, str_mod, sep = "\n")
  
  # fit the model
  res <- lavaan(
    model          = model,
    data           = data,
    auto.fix.first = auto.fix.first,
    auto.var       = auto.var,
    auto.efa       = if (M > 1) auto.efa else FALSE,
    information    = information,
    std.ov         = if (is_df) std.ov else FALSE,
    sample.cov     = S,
    sample.nobs    = sample.nobs,
    ...
  )
  res@external$type   <- "EFAST"
  res@external$syntax <- model
  res
}

#' Estimate an EFA model in lavaan
#'
#' This function estimates efa models in the same way that efast models are
#' estimated: using lavaan.
#'
#' @param data <data.frame> the dataset or <matrix> covariance matrix
#' @param M <numeric> How many factors, minimum 2
#' @param sample.nobs <numeric> sample size (if data = covmat, see lavaan)
#' @param auto.fix.first <bool> see lavaan
#' @param auto.var <bool> see lavaan
#' @param auto.efa <bool> see lavaan
#' @param information <character> see lavaan
#' @param std.ov <bool> see lavaan
#' @param ... other arguments passed to lavaan
#'
#' @details The constrained model constrains the residual covariance to be
#'   equal across the different ROIs.
#'
#' @examples
#' \dontrun{
#' # create a test dataset
#' test_data <- simulate_efast()
#' fit_efa <- efast_efa(simdat, M = 4)
#' summary(fit_efa)
#' }
#'
#' @importFrom lavaan lavaan
#'
#' @export
efast_efa <- function(data, M, sample.nobs = NULL, auto.fix.first = FALSE,
                      auto.var = TRUE, auto.efa = TRUE,
                      information = "observed", std.ov = TRUE,
                      ...) {
  # check arguments
  is_df <- check_data_argument(data)
  if (!is.numeric(M) || M < 1 || M %% 1 != 0)
    stop("Input a positive integer for M (the number of factors)")
  
  # generate efa model syntax
  if (is_df) {
    S <- NULL
    efa_mod  <- efa_model(data, M)
  } else {
    S <- data
    data <- NULL
    efa_mod  <- efa_model(S, M)
  }
  
  # fit the model
  res <- lavaan(
    model          = efa_mod,
    data           = data,
    auto.fix.first = auto.fix.first,
    auto.var       = auto.var,
    auto.efa       = if (M > 1) auto.efa else FALSE,
    information    = information,
    std.ov         = std.ov,
    sample.cov     = S,
    sample.nobs    = sample.nobs,
    ...
  )
  res@external$type   <- "EFAST_EFA"
  res@external$syntax <- efa_mod
  res
}

#' Estimate an EFAST-hemi model
#'
#' This function estimates efast models with covariance due to hemispheric
#' symmetry.
#'
#' @param data <data.frame> the dataset or <matrix> covariance matrix
#' @param M <numeric> How many factors, minimum 2
#' @param lh_idx <numeric> column numbers of left hemisphere variables
#' @param rh_idx <numeric> column numbers of right hemisphere variables
#' @param roi_names <character> optional names of rois
#' @param constrain <bool> whether to constrain the symmetry (see details)
#' @param sample.nobs <numeric> sample size (if data = covmat, see lavaan)
#' @param auto.fix.first <bool> see lavaan
#' @param auto.var <bool> see lavaan
#' @param auto.efa <bool> see lavaan
#' @param information <character> see lavaan
#' @param std.ov <bool> see lavaan
#' @param ... other arguments passed to lavaan
#'
#' @details The constrained model constrains the residual covariance to be
#'   equal across the different ROIs.
#'
#' @examples
#' \dontrun{
#' # create a test dataset
#' test_data <- simulate_efast()
#' fit_efast <- efast_hemi(test_data, M = 4, 1:17, 18:34)
#' summary(fit_efast)
#' }
#'
#' @importFrom lavaan lavaan summary
#'
#' @export
efast_hemi <- function(data, M, lh_idx, rh_idx, roi_names, constrain = FALSE,
                       sample.nobs = NULL, auto.fix.first = FALSE,
                       auto.var = TRUE, auto.efa = TRUE,
                       information = "observed", std.ov = TRUE, ...) {
  
  # Check the arguments
  is_df <- check_data_argument(data)
  if (!is.numeric(M) || M < 1 || M %% 1 != 0)
    stop("Input a positive integer for M (the number of factors)")
  if (!is.numeric(lh_idx) || !is.numeric(rh_idx))
    stop("lh_idx and rh_idx should be numeric vectors.")
  if (length(lh_idx) != length(rh_idx))
    stop("RH and LH should have an equal number of variables.")
  if (length(intersect(lh_idx, rh_idx)) != 0)
    stop("There should be no overlap between lh_idx and rh_idx.")
  
  # Preprocess data
  if (missing(roi_names)) {
    roi_names <- paste0("ROI", seq_along(lh_idx))
  } else {
    if (length(roi_names) != length(lh_idx))
      stop("roi_names should be the same length as idx.")
  }
  var_names <- c(paste0("lh_", roi_names), paste0("rh_", roi_names))
  if (is_df) {
    df <- data[, c(lh_idx, rh_idx)]
    S  <- NULL
    colnames(df) <- var_names
  } else {
    df <- NULL
    S  <- data
    colnames(S)[c(lh_idx, rh_idx)] <- var_names
    rownames(S)[c(lh_idx, rh_idx)] <- var_names
  }
  
  # Create model syntax
  efa_mod  <- if (is_df) efa_model(df, M) else efa_model(S, M)
  hemi_mod <- hemi_model(roi_names, var_const = constrain)
  lat_idx  <- ifelse(constrain, "", lat_index(roi_names))
  model    <- paste(efa_mod, hemi_mod, lat_idx, sep = "\n")
  
  # Fit the model
  res <- lavaan(
    model          = model,
    data           = df,
    auto.fix.first = auto.fix.first,
    auto.var       = auto.var,
    auto.efa       = if (M > 1) auto.efa else FALSE,
    information    = information,
    std.ov         = if (is_df) std.ov else FALSE,
    sample.cov     = S,
    sample.nobs    = sample.nobs,
    ...
  )
  res@external$type   <- "EFAST_HEMI"
  res@external$syntax <- model
  res
}


#' Synthesised volume data for DKT-atlas ROIs.
#'
#' This data has been synthesised on the basis of real-world data from the
#' Cam-CAN cohort. It contains 68 grey matter volume measurements (34 LH and 34
#' RH ROIs) for 647 participants.
#'
#' @name roi_volume
#' @usage data(roi_volume)
#' @docType data
#' @references \url{cam-can.org}
#' @keywords datasets
#' @format A data frame with 647 rows and 68 columns
NULL


#' Check data argument type
#'
#' @keywords internal
check_data_argument <- function(data) {
  is_df  <- is.data.frame(data)
  is_mat <- is.matrix(data)
  
  if (!is_df && !is_mat)
    stop("Data is not a data frame or a covariance matrix.", call. = FALSE)
  
  if (is_df) {
    # data frame check
    if (!all(apply(data, 2, is.numeric)))
      stop("Data should contain only numeric variables.")
    
    return(TRUE)
  }
  
  if (is_mat) {
    # covariance matrix check
    is_sym_pd <- tryCatch(is.matrix(chol(data)), error = function(err) FALSE)
    
    if (!is_sym_pd)
      stop("Covariance matrix must be symmetric and positive-definite.")
    
    return(FALSE)
  }
}