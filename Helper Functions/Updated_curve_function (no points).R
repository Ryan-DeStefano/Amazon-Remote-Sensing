# Works the same as the lsat_fit_phenological_curves() function from the LandsatTS package except graphs 
# the curves with no points to make it easier to read the graph

fit_phenological_curve_no_points <- function(dt, si, window.yrs = 7, window.min.obs = 20, si.min = 0.15, 
                                spar = 0.78, pcnt.dif.thresh = c(-30, 30), weight = TRUE, 
                                spl.fit.outfile = FALSE, progress = TRUE, test.run = FALSE) {
  
  dt <- data.table::data.table(dt)
  dt[, `:=`(obs.id, 1:nrow(dt))]
  obs.filtered <- c()
  n.sites <- length(unique(dt$sample.id))
  
  if (test.run == TRUE) {
    if (n.sites < 9) {
      stop(paste0("Your data set only has ", n.sites, " sample sites, so do not run in test mode"))
    }
    else {
      dt <- dt[sample.id %in% sample(unique(dt$sample.id), 9)]
    }
  }
  
  dt <- dt[, eval(c("sample.id", "obs.id", "latitude", "longitude", 
                    "year", "doy", si)), with = FALSE]
  dt <- setnames(dt, si, "si")
  dt <- dt[order(sample.id, doy)]
  dt <- dt[si >= si.min]
  dt[, `:=`(n.obs, .N), by = "sample.id"]
  dt <- dt[n.obs > window.min.obs * 2]
  dt <- dt[, `:=`(n.obs, NULL)]
  
  rough.splines.dt <- dt[, .(spl.fit = list(stats::smooth.spline(doy, 
                                                                 si, spar = spar))), by = "sample.id"]
  
  doy.rng <- min(dt$doy):max(dt$doy)
  rough.spline.fits.dt <- rough.splines.dt[, .(spl.fit = unlist(Map(function(mod, 
                                                                             doy) {
    stats::predict(mod, data.frame(doy = doy.rng))$y
  }, spl.fit))), by = "sample.id"]
  
  rough.spline.fits.dt <- rough.spline.fits.dt[, `:=`(doy, 
                                                      doy.rng), by = "sample.id"]
  dt <- rough.spline.fits.dt[dt, on = c("sample.id", "doy")]
  dt <- dt[, `:=`(pcnt.dif, (si - spl.fit)/((si + spl.fit)/2) * 
                    100)]
  dt <- dt[pcnt.dif > -100 & pcnt.dif < 100]
  dt <- dt[, `:=`(c("spl.fit", "pcnt.dif"), NULL)]
  
  all.yrs <- sort(unique(dt$year))
  rng.yrs <- min(all.yrs):max(all.yrs)
  focal.yrs <- rng.yrs[-c(1:(round(window.yrs/2)), (length(rng.yrs) - 
                                                      round(window.yrs/2) + 1):length(rng.yrs))]
  n.focal.yrs <- length(focal.yrs)
  data.list <- list()
  splines.list <- list()
  all.yrs <- min(dt$year):max(dt$year)
  
  for (i in all.yrs) {
    focal.yr <- i
    half.win.yrs <- round(window.yrs/2)
    
    if (i - half.win.yrs < min(all.yrs)) {
      focal.win <- min(all.yrs):(min(all.yrs) + window.yrs - 
                                   1)
    } else if (i + half.win.yrs > max(all.yrs)) {
      focal.win <- (max(all.yrs) - window.yrs + 1):max(all.yrs)
    } else {
      focal.win <- (i - half.win.yrs + 1):(i + half.win.yrs - 
                                             1)
    }
    
    focal.dt <- dt[year %in% focal.win]
    focal.dt <- focal.dt[order(sample.id, doy)]
    focal.dt <- focal.dt[, `:=`(n.obs.focal.win, .N), by = "sample.id"]
    focal.dt <- focal.dt[n.obs.focal.win >= window.min.obs]
    
    if (nrow(focal.dt) == 0) {
      if (progress == TRUE) {
        print(paste0("skipping ", focal.yr, " because too little data"))
      }
      (next)()
    }
    
    if (weight == TRUE) {
      focal.dt[, `:=`(n.yrs.from.focal, abs(year - focal.yr))]
      focal.dt[, `:=`(weight, exp(-0.25 * n.yrs.from.focal))]
      splines.dt <- focal.dt[, .(spl.fit = list(stats::smooth.spline(doy, 
                                                                     si, w = weight, spar = spar))), by = "sample.id"]
    } else {
      splines.dt <- focal.dt[, .(spl.fit = list(stats::smooth.spline(doy, 
                                                                     si, spar = spar))), by = "sample.id"]
    }
    
    doy.rng <- min(focal.dt$doy):max(focal.dt$doy)
    spline.fits.dt <- splines.dt[, .(spl.fit = unlist(Map(function(mod, 
                                                                   doy) {
      stats::predict(mod, data.frame(doy = doy.rng))$y
    }, spl.fit))), by = "sample.id"]
    
    spline.fits.dt <- spline.fits.dt[, `:=`(doy, doy.rng), 
                                     by = "sample.id"]
    
    refitting = 1
    
    while (refitting == 1) {
      focal.dt <- spline.fits.dt[focal.dt, on = c("sample.id", 
                                                  "doy")]
      focal.dt <- focal.dt[, `:=`(pcnt.dif, (si - spl.fit)/((si + 
                                                               spl.fit)/2) * 100)]
      refit.sites <- unique(focal.dt[pcnt.dif < pcnt.dif.thresh[1] | 
                                       pcnt.dif > pcnt.dif.thresh[2]]$sample.id)
      obs.filtered <- unique(c(obs.filtered, unique(focal.dt[pcnt.dif < 
                                                               pcnt.dif.thresh[1] | pcnt.dif > pcnt.dif.thresh[2]]$obs.id)))
      focal.dt <- focal.dt[pcnt.dif > pcnt.dif.thresh[1] & 
                             pcnt.dif < pcnt.dif.thresh[2]]
      focal.dt <- focal.dt[, `:=`(c("spl.fit", "pcnt.dif"), 
                                  NULL)]
      refit.dt <- focal.dt[sample.id %in% refit.sites]
      
      if (length(refit.sites) > 0) {
        spline.fits.dt <- spline.fits.dt[sample.id %in% 
                                           refit.sites == FALSE]
        refit.dt <- refit.dt[, `:=`(n.obs.focal.win, 
                                    .N), by = "sample.id"]
        refit.dt <- refit.dt[n.obs.focal.win >= window.min.obs]
        
        if (nrow(refit.dt) == 0) {
          refitting = 0
        } else {
          if (weight == TRUE) {
            spline.refits.dt <- refit.dt[, .(spl.fit = list(stats::smooth.spline(doy, 
                                                                                 si, w = weight, spar = spar))), by = "sample.id"]
          } else {
            spline.refits.dt <- refit.dt[, .(spl.fit = list(stats::smooth.spline(doy, 
                                                                                 si, spar = spar))), by = "sample.id"]
          }
          
          spline.refits.dt <- spline.refits.dt[, .(spl.fit = unlist(Map(function(mod, 
                                                                                 doy) {
            stats::predict(mod, data.frame(doy = doy.rng))$y
          }, spl.fit))), by = "sample.id"]
          
          spline.refits.dt <- spline.refits.dt[, `:=`(doy, 
                                                      doy.rng), by = "sample.id"]
          
          spline.fits.dt <- rbind(spline.fits.dt, spline.refits.dt)
        }
      } else {
        refitting = 0
      }
    }
    
    if (weight == TRUE) {
      focal.dt <- focal.dt[, `:=`(c("n.yrs.from.focal", 
                                    "weight"), NULL)]
    }
    
    sample.doy.smry <- focal.dt[, .(min.doy = min(doy), max.doy = max(doy)), 
                                by = "sample.id"]
    
    spline.fits.dt <- spline.fits.dt[sample.doy.smry, on = "sample.id"]
    spline.fits.dt <- spline.fits.dt[doy >= min.doy][doy <= 
                                                       max.doy]
    spline.fits.dt <- spline.fits.dt[, `:=`(spl.fit.max, 
                                            max(spl.fit)), by = "sample.id"]
    spline.fits.dt <- spline.fits.dt[, `:=`(spl.fit.max.doy, 
                                            doy[which.max(spl.fit)]), by = "sample.id"]
    spline.fits.dt <- spline.fits.dt[, `:=`(spl.frac.max, 
                                            spl.fit/spl.fit.max)]
    spline.fits.dt <- spline.fits.dt[, `:=`(si.adjustment, 
                                            spl.fit.max - spl.fit)]
    spline.fits.dt <- spline.fits.dt[, `:=`(focal.yr, focal.yr)]
    spline.fits.dt <- spline.fits.dt[, `:=`(c("min.doy", 
                                              "max.doy"), NULL)]
    splines.list[[i]] <- spline.fits.dt
    focal.dt <- spline.fits.dt[focal.dt, on = c("sample.id", 
                                                "doy")]
    focal.dt <- focal.dt[, `:=`(si.max.pred, si + si.adjustment)]
    focal.dt <- focal.dt[, `:=`(c("focal.yr"), NULL)]
    setnames(focal.dt, c("n.obs.focal.win"), c("spl.n.obs"))
    data.list[[i]] <- focal.dt
    
    if (progress == TRUE) {
      print(paste0("finished ", focal.yr))
    }
  }
  
  spline.dt <- data.table::data.table(data.table::rbindlist(splines.list))
  
  if (spl.fit.outfile != FALSE) {
    data.table::fwrite(spline.dt, spl.fit.outfile)
  }
  
  dt <- data.table::data.table(rbindlist(data.list))
  dt <- dt[order(sample.id, year, doy)]
  data.table::setcolorder(dt, c("sample.id", "latitude", "longitude", 
                                "year", "doy", "si", "spl.n.obs", "spl.fit", "spl.frac.max", 
                                "spl.fit.max", "spl.fit.max.doy", "si.adjustment", "si.max.pred"))
  n.sites <- length(unique(dt$sample.id))
  
  if (n.sites > 9) {
    example.ids <- sample(unique(dt$sample.id), 9, replace = FALSE)
  } else {
    example.ids <- sample(unique(dt$sample.id), n.sites, 
                          replace = FALSE)
  }
  
  example.obs.dt <- dt[sample.id %in% example.ids]
  example.curves.dt <- spline.dt[sample.id %in% example.ids]
  
  fig <- ggplot(example.obs.dt, aes(doy, si)) + 
    labs(y = paste0("Landsat ", toupper(si)), x = "Day of Year") + 
    ggtitle("Nine random sample locations") + 
    facet_wrap(~sample.id, nrow = 3, ncol = 3, scales = "free_y") + 
    geom_line(data = example.curves.dt, 
              mapping = aes(doy, spl.fit, group = focal.yr, 
                            color = focal.yr), alpha = 0.75) + 
    scale_color_gradientn(name = "Curve", 
                          colours = c("blue", "red", "gold")) + 
    theme_bw() + 
    theme(legend.position = "right", 
          legend.text = element_text(size = 12), 
          legend.title = element_text(size = 12), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 14), 
          plot.title = element_text(hjust = 0.5), 
          strip.background = element_rect(fill = "black"), 
          strip.text = element_text(color = "white", 
                                    size = 12)) + 
    guides(colour = guide_colourbar(title.position = "top", 
                                    title.hjust = 0.5))
  
  print(fig)
  
  if (test.run == FALSE) {
    colnames(dt) <- gsub("si", si, colnames(dt))
    return(dt)
  }
}
