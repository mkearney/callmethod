

env_with_dots <- function(env = parent.frame()) {
	if ("..." %in% ls(envir = env, all.names = TRUE)) {
		dots <- local(list(...), envir = env)
	} else {
		dots <- list()
	}
	ognames <- c(names(as.list(env)), names(dots))
	ognames <- grep("^[[:alpha:]]", ognames, value = TRUE)
	if (length(dots) == 0 || is.null(names(dots))) return(env)
	if ("" %in% names(dots)) {
		stop("All value passed to `...` must be named.")
	}
	for (i in seq_along(dots)) {
		assign(names(dots)[i], dots[[i]], envir = env)
	}
	nms <- names(env)
	nms <- grep("^[[:alpha:]]", nms, value = TRUE)
	pos <- match(nms, ognames)
	assign(".order", setNames(pos, nms), envir = env)
	env
}

get_args <- function(env) {
	if (".order" %in% ls(envir = env, all.names = TRUE)) {
		pos <- get(".order", envir = env)
		as.list(env)[pos]
	} else {
		as.list(env)
	}
}

capture_dots <- function(env = env_with_dots()) {
	get_args(env)
}




do_method <- function(method, args, env) {
	callmethods <- ls(envir = env)
	methods <- grep(sprintf("^%s\\.", method), callmethods, value = TRUE)
	if (any(grepl("\\.default$", methods))) {
		def <- grep("\\.default$", methods)
		if (length(args[[1]]) == 0) {
			m <- methods[def]
			return(do.call(m, args))
		}
		methods <- c(methods[-def], methods[def])
	}
	cls <- sub("^[^.]+\\.", "", methods)
	for (i in seq_along(cls)) {
		if (inherits(args[[1]], cls[i]) ||
				(i == length(cls) && identical(cls, "default"))) {
			return(do.call(methods[i], args))
		}
	}
	stop("No method found")
}

#' Call method
#'
#' Call a particular set of methods (functions)
#'
#' @param method String, naming method to call
#' @param env Environment with method arguments.
#' @return Returns output from method.
#' @examples
#'
#' ## define method
#' r_norm <- function(n, m = 0, sd = 1) call_method("r_norm")
#'
#' ## set default
#' r_norm.default <- function(...) rnorm(...)
#'
#' ## call method
#' r_norm(10, 3)
#'
#' ## define method
#' rstring <- function(n, collapse = "") call_method("random_string")
#'
#' ## define method default
#' random_string.default <- function(...) {
#'   list(...)[[1]]
#' }
#'
#' ##  define method for numeric
#' random_string.numeric <- function(...) {
#'   dots <- list(...)
#'   n <- dots[[1]]
#'   collapse <- dots[[2]]
#'   fl <- sample(letters, 2, replace = TRUE)
#'   paste(c(fl[1],
#'     sample(c(rep(0:9, 3), letters, toupper(letters)), n - 2, replace = TRUE),
#' 	   fl[2]), collapse = collapse)
#' }
#'
#' ## call method
#' rstring(20)
#'
#' @export
call_method <- function(method, env = parent.frame()) {
	env <- env_with_dots(env)
	args <- get_args(env)
	do_method(method, args, parent.frame(2))
}






