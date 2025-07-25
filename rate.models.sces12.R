# univariate and multivariable analysis (forward selection by LRT)
#   Adjusted in fixed effect: dage, dageXXSNR, dageXXage, dageXXOCT
# Select in fixed effect:
#   baseline age, gender, ethnicity, se, iop, anti_ht, dm4, dm5, GC/RNFL, discarea (for RNFL)
#   signal-to-noise ratio (SNR for unadjusted OCT values)
# Select in random effect:
#   dage

rm(list=ls())

library(dplyr)
library(nlme)
library(lme4)
library(sjPlot)
library(myfunc)
library(xlsx)
# source('D:/wb/Proj/Rproj/workingfunc/outlier.detection.R')

load('../20221201/SCES12_OCT.RData')
# outlier.remove <- TRUE
outlier.remove <- FALSE

# lmer.step init
{
  xintercept <- 0
  lmer.step.dir <- c('forward','forward')
}

models <- NULL

# xs <- c('ethnicM','gender','anti_ht','final_se_cen','iop_cen','dm5')
# xs <- c('gender','anti_ht','final_se_cen','avgal_cen','iop_cen','dm5')
xs <- c('gender','anti_ht','final_se_cen','iop_cen','dm5')
# lmer.step: analysis GC
{
  # init
  {
    filter <- (!filterAMD)&(!filterDR)
    filter <- filter & (!filterGlaucoma)
    filter <- filter & (!filterSuspect)
    filter <- filter & (!filterAMD2)&(!filterDR2)
    filter <- filter & (!filterGlaucoma2)
    filter <- filter & (!filterSuspect2)
    filter <- filter & filterMAC.GC
    d <- ds[filter,]
    
    # ys <- c('GCavg','GCts','GCs','GCns','GCni','GCi','GCti','GCsh','GCih')
    # ys <- c('GCavg','GCs','GCi')
    ys <- c('GCavg')
    y <- ys[1]
    x <- xs[1]
  }
  for (y in ys) {
    # multi risk factor
    {
      cat('**************************************************\n')
      cat('multivariable risk factor model\n')
      d <- ds[filter,]
      if (sum(grepl('discarea',xs))>0) d <- ds[filter & filterONHod & filterSNRonh,]
      # remove ESD outlier
      if (outlier.remove) {
        d[univariate.outlier(d[,y])$esd.outlier==1,y] <- NA
        d[univariate.outlier(d[,paste(y,'_2',sep='')])$esd.outlier==1,paste(y,'_2',sep='')] <- NA
        d[,paste('d',y,sep='')] <- d[,paste(y,'_2',sep='')] - d[,y]
      }
      m <- NULL
      m2 <- NULL
      m <- lmer.Step(d,group='sno',y=paste('d',y,sep=''),
                     xintercept=xintercept,
                     x=paste('dageXX',xs,sep=''),
                     # xadj=c('dage','dSNRmac'),
                     xadj=c('dage','dSNRmac','dageXXage_cen',paste('dageXX',y,'_cen',sep='')),
                     # z=c('dage'),
                     # z=c('dageXXethnicM'),
                     zadj=c('dage'),zintercept=0,
                     direction=lmer.step.dir)
      print(m$log)
      cat('\n')
      print(summary(m$model))
      m2 <- m$model
      cat('\n\n\n')
    }
    coef <- as.data.frame(t(c(summary(m2)$coef[,1],sqrt(as.numeric(VarCorr(m2)$sno)))))
    colnames(coef)[ncol(coef)] <- 'sd.dage'
    rownames(coef) <- y
    models <- bind_rows(models,coef)
  }
}

# xs <- c('ethnicM','gender','anti_ht','final_se_cen','iop_cen','dm5','discarea_cen')
# xs <- c('gender','anti_ht','final_se_cen','avgal_cen','iop_cen','dm5','discarea_cen')
xs <- c('gender','anti_ht','final_se_cen','iop_cen','dm5','discarea_cen')
# lmer.step: analysis RNFL
{
  # init
  {
    filter <- (!filterAMD)&(!filterDR)
    filter <- filter & (!filterGlaucoma)
    filter <- filter & (!filterSuspect)
    filter <- filter & (!filterAMD2)&(!filterDR2)
    filter <- filter & (!filterGlaucoma2)
    filter <- filter & (!filterSuspect2)
    filter <- filter & filterONH.RNFL
    d <- ds[filter,]
    
    # ys <- c('RNFLavg','RNFLt','RNFLs','RNFLn','RNFLi')
    ys <- c('RNFLs','RNFLi')
    y <- ys[1]
    x <- xs[1]
  }
  for (y in ys) {
    # multi risk factor
    {
      cat('**************************************************\n')
      cat('multivariable risk factor model\n')
      d <- ds[filter,]
      if (sum(grepl('discarea',xs))>0) d <- ds[filter & filterONHod & filterSNRonh,]
      # remove ESD outlier
      if (outlier.remove) {
        d[univariate.outlier(d[,y])$esd.outlier==1,y] <- NA
        d[univariate.outlier(d[,paste(y,'_2',sep='')])$esd.outlier==1,paste(y,'_2',sep='')] <- NA
        d[,paste('d',y,sep='')] <- d[,paste(y,'_2',sep='')] - d[,y]
      }
      m <- NULL
      m2 <- NULL
      m <- lmer.Step(d,group='sno',y=paste('d',y,sep=''),
                     xintercept=xintercept,
                     x=paste('dageXX',xs,sep=''),
                     # xadj=c('dage','dSNRonh'),
                     xadj=c('dage','dSNRonh','dageXXage_cen',paste('dageXX',y,'_cen',sep='')),
                     # z=c('dage'),
                     # z=c('dageXXethnicM'),
                     zadj=c('dage'),zintercept=0,
                     direction=lmer.step.dir)
      print(m$log)
      cat('\n')
      print(summary(m$model))
      m2 <- m$model
      cat('\n\n\n')
    }
    coef <- as.data.frame(t(c(summary(m2)$coef[,1],sqrt(as.numeric(VarCorr(m2)$sno)))))
    colnames(coef)[ncol(coef)] <- 'sd.dage'
    rownames(coef) <- y
    models <- bind_rows(models,coef)
  }
}

models <- cbind(models[,!colnames(models) %in% 'sd.dage'],
                models$sd.dage)
colnames(models)[ncol(models)] <- 'sd.dage'
# models[is.na(models)] <- 0

rate.models <- models[,grep('dage',colnames(models))]
colnames(rate.models) <- gsub('dageXX','',colnames(rate.models))
colnames(rate.models) <- gsub('_cen','',colnames(rate.models))
colnames(rate.models)[colnames(rate.models)=='dage'] <- 'Constant'
colnames(rate.models)[colnames(rate.models)=='gender'] <- 'Female'
colnames(rate.models)[colnames(rate.models)=='age'] <- 'Age'
colnames(rate.models)[colnames(rate.models)=='final_se'] <- 'SE'
rate.models <- rate.models[,c('Constant','Age','Female','SE','GCavg','RNFLs','RNFLi','sd.dage')]

data.template <- rate.models[0,!colnames(rate.models) %in% c('Constant','sd.dage')]

xs.cen.cn <- gsub('^dageXX|_cen$','',colnames(models)[grep('_cen$',colnames(models))])
xs.cen.cn <- cen$Variable[cen$Variable %in% xs.cen.cn]
xs.cen <- as.data.frame(t(cen$mean[cen$Variable %in% xs.cen.cn]))
colnames(xs.cen) <- xs.cen.cn
colnames(xs.cen) <- c('Age','SE','GCavg','RNFLs','RNFLi')

xs.cen.sces12 <- xs.cen
rate.models.sces12 <- rate.models
save(xs.cen.sces12,rate.models.sces12,data.template,file='rate_models_sces12.RData')
