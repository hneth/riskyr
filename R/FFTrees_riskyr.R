## FFTRees_riskyr.R | riskyr
## 2022 08 06
## A conversion function / API from FFTrees to riskyr objects
## -----------------------------------------------

## fft_riskyr: Conversion from FFTrees to riskyr objects: ------

# Goal: Convert an FFTrees object x into a riskyr object.

fft_riskyr <- function(x, data = "train", tree = 1){

  # Prepare: ----

  # Verify inputs:

  # x:
  if (class(x) != "FFTrees"){
    stop("Argument x is no FFTrees object")
  }

  # Parameters:
  main <- x$params$main
  crit <- x$criterion_name
  n_trees <- x$trees$n
  tree_options <- 1:n_trees


  # tree:
  if (is.null(tree) == FALSE){

    if (is.numeric(tree) == FALSE){
      stop("Argument tree is not numeric")
    }

    if (length(tree) != 1){
      stop("Argument tree must be a scalar (i.e., of length 1)")
    }

    if (is_integer(tree) == FALSE){
      stop("Argument tree is not an integer")
    }

    if (tree < 0 | tree > n_trees){
      stop(paste0("Argument tree must be an integer in range 1:", n_trees))
    }

  } else { # is.null(tree):

    stop(paste0("Argument tree must be an integer in range 1:", n_trees))

  } # if (is.null(tree)).


  # data:
  no_test_data <- is.null(x$trees$stats$test)

  if (is.null(data) == FALSE){

    data <- tolower(data)  # for robustness

    if (no_test_data){

      if ((data %in% c("train")) == FALSE){
        stop("Argument data must be 'train' (as no 'test' data available).")
      }

    } else { # test data is available:

      if ((data %in% c("train", "test")) == FALSE){
        stop("Argument data must be 'train' or 'test'.")
      }

    }

  } else { # is.null(data):

    if (no_test_data){
      stop("Argument data must be 'train' (as no 'test' data available).")
    } else { # test data is available:
      stop("Argument data must be 'train' or 'test'.")
    }

  } # if (is.null(data)).


  # Main: Get info from x ----

  # Tree definition and description:
  tree_def <- x$trees$definitions[tree, ]
  tree_def_names <- names(tree_def)
  tree_def_with_names <- paste(tree_def_names, tree_def, sep = ": ")  # combine names with descriptions
  tree_def_string <- paste(tree_def_with_names, collapse = " | ")  # collapse into 1 string
  # print(tree_def_string)  # 4debugging

  tree_in_words <- x$trees$inwords[[tree]]
  # print(tree_in_words)

  if (data == "test"){
    tree_stats <- x$trees$stats$test[tree, ]
  } else { # data == "train":
    tree_stats <- x$trees$stats$train[tree, ]
  }
  # print(tree_stats)  # 4debugging


  # 4 essential frequencies:
  hi <- tree_stats[["hi"]]
  mi <- tree_stats[["mi"]]
  fa <- tree_stats[["fa"]]
  cr <- tree_stats[["cr"]]


  # Labels:
  cond_lbl <- x$criterion_name

  cond_false_lbl <- "FALSE" # x$params$decision.labels[1]  # exit 0: FALSE
  cond_true_lbl  <- "TRUE"  # x$params$decision.labels[2]  # exit 1: TRUE
  # ToDo: Consider using capitalise_1st(x$params$decision.labels[1])

  if (data == "train"){ # data fitting:
    dec_lbl <- "Decision"
  } else { # data == "test" / predicting:
    dec_lbl <- "Prediction"
  }

  # dec_neg_lbl <- x$params$decision.labels[1]  # exit 0: FALSE
  # dec_pos_lbl <- x$params$decision.labels[2]  # exit 1: TRUE

  dec_neg_lbl <- paste0("'", x$params$decision.labels[1], "'")  # exit 0: FALSE
  dec_pos_lbl <- paste0("'", x$params$decision.labels[2], "'")  # exit 1: TRUE

  # Create riskyr object: ----

  out <- riskyr(scen_lbl = main,
                hi = hi, mi = mi, fa = fa, cr = cr,
                cond_lbl = cond_lbl, cond_true_lbl = cond_true_lbl, cond_false_lbl = cond_false_lbl,
                dec_lbl = dec_lbl, dec_pos_lbl = dec_pos_lbl, dec_neg_lbl = dec_neg_lbl,
                scen_txt = tree_in_words,
                scen_src = tree_def_string
  )


  # Output: ----

  return(out)

} # fft_riskyr().


## Check: ------
#
# library(FFTrees)
#
# # Create FFTs (as FFTrees objects):
# FFT <- FFTrees::FFTrees(survived ~., data = FFTrees::titanic, main = "FFT fitting Titanic survival")  # train(ing) data only
# FFT_t <- FFTrees::FFTrees(survived ~., data = FFTrees::titanic, train.p = .50, main = "FFT predicting Titanic survival")  # with test data
#
# plot(FFT)
#
# fft_riskyr(FFT)
# fft_riskyr(FFT, data = "train", tree = 3)
# fft_riskyr(FFT_t, data = "test", tree = 3)
#
#
# # Explore functionality:
#
# library(magrittr)  # for pipe
#
# fft_riskyr(FFT_t, data = "test") %>% plot(f_lbl = "namnum")
# fft_riskyr(FFT_t, data = "test") %>% plot(type = "tree", by = "dc", area = "sq", col_pal = pal_rgb)
# fft_riskyr(FFT_t, data = "test") %>% plot(type = "fnet", by = "cddc", area = "sq", col_pal = pal_bwp)
#
# fft_riskyr(FFT) %>% plot(type = "table", by = "cddc", col_pal = pal_bw)
# fft_riskyr(FFT) %>% plot(type = "area", by = "cdac", p_split = "v", f_lbl = "namnum", p_lbl = TRUE, col_pal = pal_rgb)
# fft_riskyr(FFT_t, data = "test") %>% plot(type = "bars", by = "all", dir = 2, f_lbl = "nam", col_pal = pal_mod)
#
# fft_riskyr(FFT) %>% plot(type = "curve")


## (+) ToDo: --------

# - Use condition label (cond_lbl) & decision label (dec_lbl) in plot_bars(),
#   rather than generic "True condition" and "Outcome" labels!

## eof. ------------------------------------------
