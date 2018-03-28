#' Generates a root density depth function for the soil nodes
#'
#' @param soilnodes vector of lower soil layer limits (cm, positive downward.),
#' for which the relative root distribution will be calculated. Values in  Topsoil
#' (humus) layers are given as negative values.
#' @param maxrootdepth the maximum rooting depth below  which relative root
#' density length will be set to zero
#' @param method root distribution model name. Choose "betamodel" to use the model after Gale & Grigal, "table" to supply a table,
#' or "constant" to get a uniform root distribution with depth
#' @param param parameter(s) of the root distribution function
#' @param relrootden vector with relative root length
#' @param rootdepths vector with lower depths limit, corresponding to relative
#' rootlength-vector
#' @param humusroots Should there be roots in top soil layers? If TRUE, the relative
#' rootlength of the topsoil layers will be the same as in the upper mineral soil layer.
#'
#' @return vector of relative rootlength, corresponding to soilnodes
#' @export
MakeRelRootDens <- function(soilnodes, #cm lower layer limits, positive downward
                            maxrootdepth, #cm, positive downward
                            method, #Depth-Table, Beta
                            param, #
                            relrootden=NULL, # supply when method is "table"
                            rootdepths=NULL,
                            humusroots = FALSE
                            #cum_RLenDmax= 0.95 # maximum cumulative rootlength, at which maximim
) {

  #snap max rootdepthto nearest soilnode
  maxrootdepth <- soilnodes[which(abs(soilnodes - maxrootdepth) == min(abs(soilnodes-maxrootdepth)))]
  method <- match.arg(method, choices = c("betamodel", "table", "constant"))

  if (length(maxrootdepth) > 1) {
    maxrootdepth <- maxrootdepth[length(maxrootdepth)]}

  if (method == "betamodel") {

    RLenD <- 1 - (param ^ seq(1,maxrootdepth))
    RLenD <- c(RLenD[1], diff(RLenD))
    if (humusroots) {
      RelDenFun <- approxfun(x = seq(1,length(RLenD)),y = RLenD, method = "linear",rule = 2:1, yright = 0)
    } else {
      RelDenFun <- approxfun(x = seq(1,length(RLenD)),y = RLenD, method = "linear",rule = 1:1, yleft = 0, yright = 0)
    }
  }

  if (method == "table") {
    if (humusroots ) {
      RelDenFun <- approxfun(x = rootdepths, y = relrootden, method = "linear",rule = 2:1, yright = 0)
    } else {
      RelDenFun <- approxfun(x = rootdepths ,y = relrootden,  method = "linear",rule = 1:1, yleft = 0, yright = 0)
    }

  }

  if (method == "constant") {
    if (humusroots ) {
      RelDenFun <- approxfun(x = soilnodes, y =c(rep(1,sum(soilnodes <= maxrootdepth)),rep(0,sum(soilnodes > maxrootdepth))) ,
                             method = "linear",rule = 2:1, yright = 0)
    } else {
      RelDenFun <- approxfun(x = soilnodes ,y = c(rep(1,sum(soilnodes <= maxrootdepth)),rep(0, sum(soilnodes > maxrootdepth))),
                                                  method = "linear",rule = 1:1, yleft = 0, yright = 0 )
    }

  }

  RelDenFun(soilnodes)

}


# beta <- optimize(function(beta,z,maxroot.pct){abs(maxroot.pct- (1 - (beta^z)))},
#                  c(0,1), z=85, maxroot.pct=0.95 )$minimum
# beta <- 0.99
# RLenD <- 1 - (beta^seq(1,120))
# #wurzeln anf?gen
# RLenD <- c(RLenD[1], diff(RLenD))
#
# soil$relrootlength <- round(approx(x = seq(1,length(RLenD)),y = RLenD, xout=soil$Lower*-100,method = "linear",rule = 2:1, yright = 0)$y,4)

