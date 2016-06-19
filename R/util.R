# Copyright 2016 Kim. All right reserved.

#------------------------------------------------------------------------------
#
# Author: kim.seonghyun@scipi.net (Kim Seonghyun)

# ------------------------------------------------------------------------------
impactObject <- function(.data, plans,
                         verbose = F) {
  plan <- plans[1, ]
  checked <- checkData(.data, plan)
  res <- ZDBayes::ZDSAD(checked$target.data,
                        checked$ref.idxs,
                        checked$mea.idxs)
  return(res)
}
