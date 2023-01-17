#' SDA unit normalization
#'
#' @param X matrix with state variables created in build_x
#' @param method FALSE = forward, TRUE = reverse
#' @param Y obs.mean
#' @param R obs.cov
#' @param Pa analysis mean
#' @param mu.a analysis cov
#'
#' @return
#' @export
#'
#' @examples
sda_unitnormalization <- function(X, Y = NULL, R = NULL, Pa = NULL, mu.a = NULL, method = FALSE){
  
  if(method){
    #reverse mode, after analysis step to revert unit normalization before states are updated
    #use vectors of var.units/var.names to match to names in X
    #use matrix multiplication
    
    if("AbvGrndWood" %in% colnames(X)){
      AGB <- X[,"AbvGrndWood"]
      AGB <- AGB/10
    }
    if("LAI" %in% colnames(X)){
      LAI <- X[,"LAI"]
      LAI <- round(LAI, 1)/1
    }
    if("TotSoilCarb" %in% colnames(X)){
      SoilC <- X[,"TotSoilCarb"]
      SoilC <- SoilC/0.1
    }
    if("litter_carbon_content" %in% colnames(X)){
      litterC <- X[,"litter_carbon_content"]
      litterC <- litterC/30
    }
    if("NEE" %in% colnames(X)){
      NEE <- X[,"NEE"]
      NEE <- NEE/10
    }
    if("Qle" %in% colnames(X)){
      Qle <- X[,"Qle"]
      Qle <- Qle/1
    }
    X.norm <- as.matrix(cbind(AGB, LAI, SoilC, litterC, NEE, Qle))
    if(exists("Pa") & exists("mu.a")){
      if("Xall[1]" %in% colnames(Pa)){
        AGB <- Pa[,"Xall[1]"]
        AGB.var <- mu.a["Xall[1]"]
        AGB <- AGB/10
        AGB.var <- AGB.var/10
      }
      if("Xall[2]" %in% colnames(Pa)){
        LAI <- Pa[,"Xall[2]"]
        LAI.var <- mu.a["Xall[2]"]
        LAI <- LAI/1
        LAI.var <- LAI.var/1
      }
      if("Xall[3]" %in% colnames(Pa)){
        SoilC <- Pa[,"Xall[3]"]
        SoilC.var <- mu.a["Xall[3]"]
        SoilC <- SoilC/0.1
        SoilC.var <- Soil.C.var/0.1
      }
      if("Xall[4]" %in% colnames(Pa)){
        litterC <- Pa[,"Xall[4]"]
        litterC.var <- mu.a["Xall[4]"]
        litterC <- litterC/30
        litterC.var <- litterC.var/30
      }
      if("Xall[5]" %in% colnames(Pa)){
        NEE <- Pa[,"Xall[5]"]
        NEE.var <- mu.a["Xall[5]"]
        NEE <- NEE/10
        NEE.var <- NEE.var/10
      }
      if("Xall[6]" %in% colnames(Pa)){
        Qle <- Pa[,"Xall[6]"]
        Qle.var <- mu.a["Xall[6]"]
        Qle <- Qle/1
        Qle.var <- Qle.var/1
      }
      Pa.norm <- Pa
      Pa.norm[,"Xall[1]"] <- AGB
      Pa.norm[,"Xall[2]"] <- LAI
      Pa.norm[,"Xall[3]"] <- SoilC
      Pa.norm[,"Xall[4]"] <- litterC
      Pa.norm[,"Xall[5]"] <- NEE
      Pa.norm[,"Xall[6]"] <- Qle
      mu.a.norm <- mu.a
      mu.a.norm["Xall[1]"] <- AGB.var
      mu.a.norm["Xall[2]"] <- LAI.var
      mu.a.norm["Xall[3]"] <- SoilC.var
      mu.a.norm["Xall[4]"] <- litterC.var
      mu.a.norm["Xall[5]"] <- NEE.var
      mu.a.norm["Xall[6]"] <- Qle.var
    }
    norm.unit = list(X=X.norm, Pa=Pa.norm, mu.a=mu.a.norm)
  }else{
    #forward mode, before analysis step to normalize unit range
    if("AbvGrndWood" %in% colnames(X)){
      AGB <- X[,"AbvGrndWood"]
      AGB <- AGB*10
    }
    if("LAI" %in% colnames(X)){
      LAI <- X[,"LAI"]
      LAI <- round(LAI, 1)*1
    }
    if("TotSoilCarb" %in% colnames(X)){
      SoilC <- X[,"TotSoilCarb"]
      SoilC <- SoilC*0.1
    }
    if("litter_carbon_content" %in% colnames(X)){
      litterC <- X[,"litter_carbon_content"]
      litterC <- litterC*30
    }
    if("NEE" %in% colnames(X)){
      NEE <- X[,"NEE"]
      NEE <- NEE*10
    }
    if("Qle" %in% colnames(X)){
      Qle <- X[,"Qle"]
      Qle <- Qle*1
    }
    X.norm <- as.matrix(cbind(AGB, LAI, SoilC, litterC, NEE, Qle))
    if(exists("Y") & exists("R")){
      if("NEE" %in% names(Y)){
        NEE.obs <- Y["NEE"]
        NEE.obs <- NEE.obs*10
        NEE.cov <- R[,1]
        NEE.cov <- NEE.cov*10
      }
      if("Qle" %in% names(Y)){
        Qle.obs <- Y["Qle"]
        Qle.obs <- Qle.obs*1
        Qle.cov <- R[,2]
        Qle.cov <- Qle.cov*1
      }
      Y.norm <- Y
      Y.norm["NEE"] <- NEE.obs
      Y.norm["Qle"] <- Qle.obs
      R.norm <- R
      R.norm[,1] <- NEE.cov
      R.norm[,2] <- Qle.cov
    }
   norm.unit = list(X=X.norm, Y=Y.norm, R=R.norm) 
  }
  return(norm.unit)
}