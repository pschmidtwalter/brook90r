#' Generates a root density depth function for the soil nodes
#'
#' @param soilnodes vector of lower soil layer limits,
#' for which the relative root distribution will be calculated. Topsoil (humus) layers
#' corespond to negative values.
#' @param maxrootdepth the maximum rooting depth (m, negative downwards) below which
#' relative root length density will be set to zero
#' @param method method name for the root depth distribution. "betamodel" use the model after Gale & Grigal, "table" to supply a table,
#' "constant" to get a uniform root distribution with depth. 'table' redistributes layers of root density to the soilnodes
#' @param beta parameter(s) of the root distribution function
#' @param relrootden vector with relative root density
#' @param rootdepths vector with lower depths limit, corresponding to relative
#' rootlength-vector
#' @param humusroots Should there be roots in top soil layers? If TRUE, the relative
#' rootlength of the topsoil layers will be the same as in the upper mineral soil layer.
#'
#' @return vector of relative rootlength, corresponding to soilnodes
#' @export
MakeRelRootDens <- function(soilnodes, #cm lower layer limits, positive downward
                            maxrootdepth = min(soilnodes), #cm, positive downward
                            method ="betamodel", #Depth-Table, Beta
                            beta = 0.97, #
                            relrootden=NULL, # supply when method is "table"
                            rootdepths=NULL,
                            humusroots = FALSE
                            #cum_RLenDmax= 0.95 # maximum cumulative rootlength, at which maximim
) {

  #snap max rootdepth to nearest soilnode
  method <- match.arg(method, choices = c("betamodel", "table", "constant"))
  maxrootdepth <- soilnodes[which(abs(soilnodes - maxrootdepth) == min(abs(soilnodes-maxrootdepth)))]

  if (method == "betamodel") {

    maxrootdepth <- maxrootdepth * (-100)

    soilnodes <- soilnodes * (-100)

    RLenD <- 1 - (beta ^ seq(1,maxrootdepth))
    RLenD <- c(RLenD[1], diff(RLenD))
    rootden <- data.table(rootden = RLenD, lower = 1:maxrootdepth)


    if (humusroots) {
      RelDenFun <- approxfun(x = seq(1,length(RLenD)),y = RLenD, method = "linear",rule = 2:1, yright = 0)
    } else {
      RelDenFun <- approxfun(x = seq(1,length(RLenD)),y = RLenD, method = "linear",rule = 1:1, yleft = 0, yright = 0)
    }

    # get values at soillayer midpoint:
    midpoints <- c(0, soilnodes[1:length(soilnodes)-1]) + (diff(c(0,soilnodes))/2)
    rootden <- RelDenFun(midpoints) * (1/sum(RelDenFun(midpoints)))

  }

  if (method == "table") {
    if (humusroots ) {
      RelDenFun <- approxfun(x = rootdepths ,y = relrootden,  method = "const",rule = 2:1, yleft = 0, yright = 0)
      #RelDenFun <- approxfun(x = rootdepths, y = relrootden, method = "linear",rule = 2:1, yright = 0)
    } else {
      RelDenFun <- approxfun(x = rootdepths ,y = relrootden,  method = "linear",rule = 1:1, yleft = 0, yright = 0)
    }
    rootden <- RelDenFun(soilnodes) * (1/sum(RelDenFun(soilnodes)))
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

  return(rootden)

}


# beta <- optimize(function(beta,z,maxroot.pct){abs(maxroot.pct- (1 - (beta^z)))},
#                  c(0,1), z=85, maxroot.pct=0.95 )$minimum
# beta <- 0.99
# RLenD <- 1 - (beta^seq(1,120))
# #wurzeln anf?gen
# RLenD <- c(RLenD[1], diff(RLenD))
#
# soil$relrootlength <- round(approx(x = seq(1,length(RLenD)),y = RLenD, xout=soil$Lower*-100,method = "linear",rule = 2:1, yright = 0)$y,4)

