
# Graded IRT from s v miller:
# USA %>%
#   select(uid, havedemr, strongleader, armyrule) %>%
#   filter_all(all_vars(!is.na(.))) -> Index
#
# IndexM <- mirt(Index[ ,  2:ncol(Index)], model = 1,
#                itemtype = "graded", SE = TRUE, verbose = FALSE)
#
# fscores(IndexM, full.scores = TRUE, full.scores.SE = TRUE) %>%
#   tbl_df() %>%
#   rename(lindex = F1,
#          se_lindex = SE_F1) %>%
#   bind_cols(Index, .) %>%
#   select(uid, lindex, se_lindex) %>%
#   left_join(USA, .) -> USA


# from steve miller
get_var_info <- function(x) {
 require(labelled)
 require(tidyverse)
 var_label(x) -> b
 data.frame(r = unique(data.frame(val_labels(x)))) -> c
 rownames_to_column(c, "label") -> c
 names(c) <- c("label", "numeric")
 tribble(~label, ~numeric,
         b, NA,
         NA, NA) -> tribs
 tribs[is.na(tribs)] <- ""
 tribs$numeric <- as.numeric(tribs$numeric)
 as.data.frame(rbind(tribs, c)) -> info
 info[is.na(info)] <- ""
 print(info, row.names=F)
}
