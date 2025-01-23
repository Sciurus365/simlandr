# those commands are used to check the package and to submit it to CRAN
# a special argument is used to compact the vignettes

devtools::submit_cran(args = c('--compact-vignettes=both'))
devtools::check_win_devel(args = c('--compact-vignettes=both'))
devtools::check_win_release(args = c('--compact-vignettes=both'))
devtools::check_mac_release(args = c('--compact-vignettes=both'))
devtools::check_rhub(build_args = c('--compact-vignettes=both'))
devtools::build(args = c('--compact-vignettes=both'))
devtools::check(remote = TRUE, build_args = c('--compact-vignettes=both'))
devtools::release(args = c('--compact-vignettes=both'))
devtools::install(args = c('--compact-vignettes=both'), build_vignettes = TRUE)
