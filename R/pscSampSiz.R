#' Sample size search for PSC study design
#'
#' Searches for a sample size that achieves a target study power using an
#' iterative simulation strategy followed by logistic regression.
#'
#' @param CFM a Counter-Factual model
#' @param beta the (log) HR used in the study design
#' @param fuTime follow-up time for the study design
#' @param recTime recruitment time for the study design
#' @param rec a study recruitment estimate obtained using [recForcast()].
#'   If specified, \code{recTime} is ignored.
#' @param n0 number of patients allocated to the control arm (defaults to 0)
#' @param n_start starting experimental-arm sample size (default 50)
#' @param n_min minimum experimental-arm sample size (default 10)
#' @param power target study power (default 0.8)
#' @param alpha one-sided significance level used to define a positive trial
#'   (default 0.05, i.e. require \eqn{1 - \alpha} of the posterior on the
#'   superiority side of the bound)
#' @param bound bound for the efficacy parameter on the log scale (default 0,
#'   i.e. HR = 1)
#' @param direction direction defining superiority: \code{"less"} tests
#'   \eqn{\beta <} \code{bound} (e.g. HR \eqn{<} 1); \code{"greater"} tests
#'   \eqn{\beta >} \code{bound}
#' @param n_iter number of adaptive search iterations (default 20)
#' @param step fractional change in sample size per iteration (default 0.1)
#' @param nsim.psc number of MCMC iterations for PSC estimation (default 500)
#' @param burn.psc burn-in for PSC estimation (default 100)
#' @param cen.prop proportion of administrative censoring (default 0.05)
#' @param stable_iter number of consecutive identical sample-size predictions
#'   required before the model-based search is considered stable (default 5)
#' @param max_refine maximum number of model-based refinement iterations
#'   (default 50)
#' @param max_adaptive_extend maximum number of times the initial adaptive
#'   search is repeated when only one trial outcome is observed (default 10)
#' @param trace if \code{TRUE}, print one line per iteration showing the
#'   current sample size, the proportion of \code{beta_1} draws greater than
#'   \code{bound}, whether the trial is a success, and the sample size for the
#'   next iteration. MCMC progress output is suppressed when tracing.
#'
#' @details
#' The search proceeds in three stages:
#' \enumerate{
#'   \item Starting at \code{n_start}, run \code{n_iter} adaptive iterations.
#'     At each step a single simulated trial is evaluated. If the trial is
#'     positive (at least \eqn{1 - \alpha} of the posterior for \code{beta_1}
#'     lies on the superiority side of \code{bound}), \code{n1} is reduced by
#'     \code{step}; otherwise it is increased. \code{n1} is not reduced below
#'     \code{n_min}.
#'   \item If only positive or only negative trials are observed, the initial
#'     adaptive search is repeated from \code{n_start} until both outcomes
#'     have been observed.
#'   \item A logistic regression model is fitted with trial positivity as the
#'     response and \code{n1} as the predictor. Trials are simulated at the
#'     predicted sample size, the results are added to the search data, and the
#'     model is refitted. This continues until the predicted sample size is
#'     unchanged for \code{stable_iter} consecutive refinements.
#' }
#'
#' @return A list with components:
#' \describe{
#'   \item{search}{data frame of all adaptive and refinement iterations}
#'   \item{glm_fit}{final fitted logistic regression model}
#'   \item{n_predicted}{stable sample size predicted for the target power}
#'   \item{predictions}{sample-size predictions from each refinement iteration}
#'   \item{final}{list with posterior draws and summary from the last
#'     refinement run at \code{n_predicted}}
#' }
#' @seealso [pscDesign()], [trialSamp()], [trialPositive()]
#' @export
#' @examples
#' \dontrun{
#' gemCFM <- pscDesign::gemCFM
#' pscSampSiz(gemCFM, beta = log(0.7), fuTime = 12, recTime = 12,
#'            n_iter = 5, nsim.psc = 300, burn.psc = 100, trace = TRUE)
#' }
pscSampSiz <- function(CFM, beta, fuTime, recTime = NULL, rec = NULL,
                       n0 = 0, n_start = 50, n_min = 10, power = 0.8,
                       alpha = 0.05, bound = 0,
                       direction = c("less", "greater"),
                       n_iter = 20, step = 0.1, nsim.psc = 500,
                       burn.psc = 100, cen.prop = 0.05, stable_iter = 5,
                       max_refine = 50, max_adaptive_extend = 10,
                       trace = FALSE) {

  direction <- match.arg(direction)

  if (is.null(rec) && is.null(recTime)) {
    stop("One of recTime or rec must be specified.")
  }

  # Shared arguments passed to each simulated trial.
  trial_args <- list(
    CFM = CFM, n0 = n0, beta = beta, fuTime = fuTime, recTime = recTime,
    rec = rec, nsim.psc = nsim.psc, burn.psc = burn.psc, cen.prop = cen.prop,
    alpha = alpha, bound = bound, direction = direction, trace = trace
  )

  # Stage 1: initial adaptive search from n_start.
  search <- runAdaptiveBlock(
    n_start = n_start, n_min = n_min, step = step, n_iter = n_iter,
    phase = "adaptive", batch = 1L, start_index = 0L, trial_args = trial_args
  )

  # Stage 2: repeat the adaptive search until both positive and negative trials
  # have been observed (required for logistic regression).
  adaptive_batches <- 1L
  while (length(unique(search$positive)) < 2L &&
         adaptive_batches < max_adaptive_extend) {
    adaptive_batches <- adaptive_batches + 1L
    if (isTRUE(trace)) {
      pscSampSizTrace(sprintf(
        "Repeating initial adaptive search (batch %d) to obtain both outcomes",
        adaptive_batches
      ))
    }
    search <- rbind(
      search,
      runAdaptiveBlock(
        n_start = n_start, n_min = n_min, step = step, n_iter = n_iter,
        phase = "adaptive", batch = adaptive_batches,
        start_index = nrow(search), trial_args = trial_args
      )
    )
  }

  if (length(unique(search$positive)) < 2L) {
    stop(
      "Adaptive search did not produce both positive and negative trials after ",
      max_adaptive_extend, " batches; try a different n_start or design.",
      call. = FALSE
    )
  }

  # Stage 3: model-based refinement until the predicted sample size stabilises.
  refine <- refineSampleSize(
    search = search,
    power = power,
    n_min = n_min,
    stable_iter = stable_iter,
    max_refine = max_refine,
    trial_args = trial_args
  )

  list(
    search = refine$search,
    glm_fit = refine$glm_fit,
    n_predicted = refine$n_predicted,
    predictions = refine$predictions,
    final = refine$final
  )
}

# Run one block of adaptive iterations, adjusting n1 up or down by step after
# each simulated trial depending on whether the trial is positive.
runAdaptiveBlock <- function(n_start, n_min, step, n_iter, phase, batch,
                             start_index, trial_args) {
  n1 <- n_start
  rows <- vector("list", n_iter)

  for (i in seq_len(n_iter)) {
    n1 <- max(n_min, round(n1))

    # Simulate a trial at the current sample size.
    trial <- evaluateTrial(
      n1 = n1, phase = phase, batch = batch, iteration = start_index + i,
      n1_next = NA_integer_, trial_args = trial_args
    )

    # Reduce n1 after a positive trial; increase after a negative trial.
    n1_next <- if (trial$row$positive) {
      max(n_min, floor(n1 * (1 - step)))
    } else {
      ceiling(n1 * (1 + step))
    }
    trial$row$n1_next <- n1_next

    if (isTRUE(trial_args$trace)) {
      pscSampSizTrace(sprintf(
        "[%s %d/%d] n1 = %d, P(beta > bound) = %.3f, success = %s, next_n1 = %d",
        phase, i, n_iter, trial$row$n1, trial$row$prop_gt_bound,
        if (trial$row$positive) "TRUE" else "FALSE",
        n1_next
      ))
    }

    rows[[i]] <- trial$row
    n1 <- n1_next
  }

  do.call(rbind, rows)
}

# Fit logistic regression to search data, simulate at the predicted sample size,
# append the result, and repeat until predictions stabilise.
refineSampleSize <- function(search, power, n_min, stable_iter, max_refine,
                             trial_args) {
  predictions <- integer()
  glm_fit <- NULL
  final <- NULL
  refine_count <- 0L

  repeat {
    refine_count <- refine_count + 1L
    if (refine_count > max_refine) {
      warning(
        "Model-based refinement did not stabilise within ", max_refine,
        " iterations; returning the latest prediction.",
        call. = FALSE
      )
      break
    }

    # Fit model and predict the sample size required for target power.
    glm_fit <- stats::glm(
      positive ~ n1,
      family = stats::binomial(),
      data = search
    )
    n_predicted <- predictSampleSize(glm_fit, power = power, n_min = n_min)
    predictions <- c(predictions, n_predicted)

    if (isTRUE(trial_args$trace)) {
      pscSampSizTrace(sprintf(
        "[refine %d] predicted n1 for %.0f%% power = %d",
        refine_count, 100 * power, n_predicted
      ))
    }

    # Stop when the predicted sample size is unchanged for stable_iter runs.
    if (length(predictions) >= stable_iter &&
        length(unique(tail(predictions, stable_iter))) == 1L) {
      if (isTRUE(trial_args$trace)) {
        pscSampSizTrace(sprintf(
          "sample size stable at n1 = %d for %d consecutive predictions",
          n_predicted, stable_iter
        ))
      }
      break
    }

    # Simulate at the predicted sample size and update the search data.
    trial <- evaluateTrial(
      n1 = n_predicted, phase = "refine", batch = refine_count,
      iteration = nrow(search) + 1L, n1_next = NA_integer_,
      trial_args = trial_args
    )
    search <- rbind(search, trial$row)
    final <- trialResult(trial, n_predicted)

    if (isTRUE(trial_args$trace)) {
      pscSampSizTrace(sprintf(
        "[refine %d] n1 = %d, P(beta > bound) = %.3f, success = %s",
        refine_count, trial$row$n1, trial$row$prop_gt_bound,
        if (trial$row$positive) "TRUE" else "FALSE"
      ))
    }
  }

  n_predicted <- tail(predictions, 1L)

  # Ensure a final trial result exists at the stable predicted sample size.
  if (is.null(final) || final$n1 != n_predicted) {
    trial <- evaluateTrial(
      n1 = n_predicted, phase = "refine", batch = refine_count,
      iteration = nrow(search) + 1L, n1_next = NA_integer_,
      trial_args = trial_args
    )
    search <- rbind(search, trial$row)
    final <- trialResult(trial, n_predicted)
  }

  list(
    search = search,
    glm_fit = glm_fit,
    n_predicted = n_predicted,
    predictions = predictions,
    final = final
  )
}

trialResult <- function(trial, n1) {
  list(
    n1 = n1,
    draws = trial$draws,
    prop_gt_bound = trial$row$prop_gt_bound,
    positive = as.logical(trial$row$positive),
    power = trial$row$power
  )
}

# Simulate one trial and summarise the posterior for beta_1.
evaluateTrial <- function(n1, phase, batch, iteration, n1_next, trial_args) {
  rec_i <- scaleRecruitment(
    rec = trial_args$rec, recTime = trial_args$recTime,
    n0 = trial_args$n0, n1 = n1
  )

  draws <- runTrialSamp(
    trace = trial_args$trace,
    CFM = trial_args$CFM, n0 = trial_args$n0, n1 = n1,
    beta = trial_args$beta, fuTime = trial_args$fuTime,
    recTime = trial_args$recTime, rec = rec_i,
    nsim.psc = trial_args$nsim.psc, burn.psc = trial_args$burn.psc,
    cen.prop = trial_args$cen.prop
  )

  prop_gt_bound <- postEval(draws, bound = trial_args$bound)
  positive <- trialPositive(
    draws, bound = trial_args$bound, alpha = trial_args$alpha,
    direction = trial_args$direction
  )
  pwr <- trialPower(
    draws, bound = trial_args$bound, direction = trial_args$direction
  )

  list(
    row = data.frame(
      iteration = iteration,
      phase = phase,
      batch = batch,
      n1 = n1,
      prop_gt_bound = prop_gt_bound,
      positive = as.integer(positive),
      n1_next = n1_next,
      power = pwr,
      post_mean = mean(draws),
      post_sd = stats::sd(draws),
      stringsAsFactors = FALSE
    ),
    draws = draws
  )
}

pscSampSizTrace <- function(...) {
  msg <- paste(unlist(list(...)), collapse = "\n")
  cat(msg, "\n", sep = "")
  flush.console()
  invisible(msg)
}

runTrialSamp <- function(trace = FALSE, ...) {
  if (isTRUE(trace)) {
    utils::capture.output(
      res <- trialSamp(...),
      type = c("output", "message")
    )
    res
  } else {
    trialSamp(...)
  }
}

scaleRecruitment <- function(rec, recTime, n0, n1) {
  if (!is.null(rec)) {
    n_target <- n0 + n1
    n_rec <- max(rec$Cumualtive.Rec.)
    if (n_rec == 0) {
      stop("recruitment forecast must include at least one patient.")
    }
    scale <- n_target / n_rec
    scaled <- rec
    scaled$Monthly.Rec <- pmax(0, round(rec$Monthly.Rec * scale))
    scaled$Cumualtive.Rec. <- cumsum(scaled$Monthly.Rec)
    return(scaled)
  }

  n <- n0 + n1
  cumRec <- round(seq(0, n, length = recTime))
  mnthRec <- c(0, diff(cumRec))
  data.frame(
    SitesOpen = 1,
    Monthly.Rec = mnthRec,
    Cumualtive.Rec. = cumRec,
    stringsAsFactors = FALSE
  )
}

# Solve the logistic regression equation for n1 at the target power.
predictSampleSize <- function(glm_fit, power, n_min) {
  b <- stats::coef(glm_fit)
  if (length(b) != 2L || abs(b[2L]) < .Machine$double.eps) {
    stop(
      "Logistic regression did not identify a sample-size effect; ",
      "try a wider search range or more iterations.",
      call. = FALSE
    )
  }

  target <- stats::qlogis(power)
  n_pred <- (target - b[1L]) / b[2L]

  if (!is.finite(n_pred)) {
    stop("Predicted sample size is not finite.", call. = FALSE)
  }

  max(n_min, round(n_pred))
}
