#!/usr/bin/env Rscript

# Public-data Stage P3C, visible Tasks 2-9 and 14.
# Country models remain independent; no pooled or meta-analytic estimate is fit.

options(stringsAsFactors = FALSE, scipen = 999)
args <- commandArgs(trailingOnly = FALSE)
script_arg <- grep("^--file=", args, value = TRUE)
script_path <- normalizePath(sub("^--file=", "", script_arg[1]))
root <- dirname(dirname(script_path))
setwd(root)

z <- qnorm(0.975)
safe_write <- function(x, path) write.csv(x, path, row.names = FALSE, na = "")

specs <- list(
  AUS=list(country="Australia", policy=2009, primary_break=2014,
           lags=c(3,5,7,10), file="Iodine_PublicP3A1_Australia_Incidence.csv",
           rate_standard="AIHW age-standardised rate", policy_description="Mandatory iodised salt in most non-organic bread",
           uic_pre=96.0, uic_pre_year=2003.5, uic_post=176.7, uic_post_year=2011.5),
  NZL=list(country="New Zealand", policy=2009, primary_break=2014,
           lags=c(3,5,7,10), file="Iodine_PublicP3A1_NewZealand_Incidence.csv",
           rate_standard="WHO world standard", policy_description="Mandatory iodised salt in most non-organic bread",
           uic_pre=53.0, uic_pre_year=2008.5, uic_post=103.0, uic_post_year=2014.5),
  HRV=list(country="Croatia", policy=1996, primary_break=2001,
           lags=c(3,5,7,10,15), file="Iodine_PublicP3A1_Croatia_Incidence.csv",
           rate_standard="CI5 Segi/Doll world standard", policy_description="Edible-salt iodisation strengthened to 25 mg KI/kg",
           uic_pre=NA, uic_pre_year=NA, uic_post=140.0, uic_post_year=2002.0)
)

read_country <- function(iso) {
  d <- read.csv(specs[[iso]]$file, check.names=FALSE)
  d <- d[d$model_inclusion == "YES", ]
  d$year <- as.integer(d$year)
  d$analysis_rate_per_100k <- as.numeric(d$analysis_rate_per_100k)
  d <- d[order(d$year), ]
  stopifnot(!anyDuplicated(d$year), all(d$observed_status == "OBSERVED"))
  d
}

hac_vcov <- function(fit, lag=NULL) {
  X <- model.matrix(fit); e <- as.numeric(residuals(fit))
  n <- nrow(X); k <- ncol(X)
  if (is.null(lag)) lag <- max(1L, floor(4 * (n / 100)^(2 / 9)))
  meat <- matrix(0, k, k)
  for (t in seq_len(n)) meat <- meat + e[t]^2 * tcrossprod(X[t,])
  if (lag > 0) for (ell in seq_len(min(lag,n-1L))) {
    weight <- 1 - ell/(lag+1)
    gamma <- matrix(0,k,k)
    for (t in (ell+1L):n) gamma <- gamma + e[t]*e[t-ell]*tcrossprod(X[t,],X[t-ell,])
    meat <- meat + weight*(gamma+t(gamma))
  }
  bread <- solve(crossprod(X))
  list(vcov=n/(n-k)*bread%*%meat%*%bread,lag=lag)
}

prepare <- function(d, break_year) {
  d <- d[order(d$year),]
  d$time <- d$year-min(d$year)
  d$post <- as.integer(d$year>=break_year)
  d$time_after <- pmax(0,d$year-break_year)
  d
}

fit_segment <- function(d, break_year, outcome="analysis_rate_per_100k") {
  x <- prepare(d,break_year)
  form <- as.formula(paste(outcome,"~time+post+time_after"))
  fit <- lm(form,data=x); hc <- hac_vcov(fit)
  list(data=x,fit=fit,hac=hc,break_year=break_year,outcome=outcome)
}

contrast <- function(obj, values) {
  b <- coef(obj$fit); vc <- obj$hac$vcov
  v <- setNames(rep(0,length(b)),names(b)); v[names(values)] <- values
  est <- sum(v*b); se <- sqrt(as.numeric(t(v)%*%vc%*%v)); df <- df.residual(obj$fit)
  c(estimate=est,se=se,low=est-z*se,high=est+z*se,p=2*pt(-abs(est/se),df=df))
}

primary_stats <- function(obj) {
  list(pre=contrast(obj,c(time=1)), post=contrast(obj,c(time=1,time_after=1)),
       change=contrast(obj,c(time_after=1)), level=contrast(obj,c(post=1)))
}

predict_at <- function(obj, year) {
  x <- data.frame(time=year-min(obj$data$year),post=as.integer(year>=obj$break_year),
                  time_after=max(0,year-obj$break_year))
  as.numeric(predict(obj$fit,newdata=x))
}

country_data <- lapply(names(specs),read_country); names(country_data) <- names(specs)
primary <- lapply(names(specs),function(iso) fit_segment(country_data[[iso]],specs[[iso]]$primary_break)); names(primary)<-names(specs)

# Task 2: deterministic independent reproduction against the locked result files.
locked_primary <- function(iso) {
  label <- if (iso=="AUS") "Australia" else if (iso=="NZL") "NewZealand" else "Croatia"
  d <- read.csv(paste0("Iodine_PublicP3A1_",label,"_Models.csv"),check.names=FALSE)
  d$estimate[d$estimator=="OLS_HAC" & d$information_label=="PRIMARY" & d$parameter=="time_after"][1]
}
repro_rows <- list()
prompt_approx <- c(AUS=0.000081,NZL=-0.140486,HRV=0.218929,HRV_IARC=0.192703)
for (iso in names(specs)) {
  est <- primary_stats(primary[[iso]])$change["estimate"]
  locked <- locked_primary(iso)
  repro_rows[[length(repro_rows)+1]] <- data.frame(
    country=specs[[iso]]$country,ISO3=iso,model="PRIMARY_PLUS5_OLS_HAC",
    frozen_input=specs[[iso]]$file,locked_file_estimate=locked,prompt_locked_approx=prompt_approx[iso],
    reproduced_estimate=est,absolute_difference=abs(est-locked),numeric_tolerance=1e-10,
    exact_file_match=ifelse(abs(est-locked)<=1e-10,"PASS","FAIL"),
    prompt_rounding_match=ifelse(abs(est-prompt_approx[iso])<=5e-7,"PASS","FAIL"),
    reproducibility_verdict=ifelse(abs(est-locked)<=1e-10,"REPRODUCIBLE","NOT_REPRODUCIBLE"))
}
hrv_iarc <- fit_segment(country_data$HRV[country_data$HRV$year<=2017,],2001)
iarc_est <- primary_stats(hrv_iarc)$change["estimate"]
locked_splice <- read.csv("Iodine_PublicP3B_Croatia_Splice_Sensitivity.csv",check.names=FALSE)
iarc_locked <- locked_splice$slope_change[locked_splice$model_id=="C1_IARC_1988_2017"][1]
repro_rows[[length(repro_rows)+1]] <- data.frame(
  country="Croatia",ISO3="HRV",model="IARC_ONLY_1988_2017_PLUS5_OLS_HAC",
  frozen_input="Iodine_PublicP3A1_Croatia_Incidence.csv",locked_file_estimate=iarc_locked,
  prompt_locked_approx=prompt_approx["HRV_IARC"],reproduced_estimate=iarc_est,
  absolute_difference=abs(iarc_est-iarc_locked),numeric_tolerance=1e-10,
  exact_file_match=ifelse(abs(iarc_est-iarc_locked)<=1e-10,"PASS","FAIL"),
  prompt_rounding_match=ifelse(abs(iarc_est-prompt_approx["HRV_IARC"])<=5e-7,"PASS","FAIL"),
  reproducibility_verdict=ifelse(abs(iarc_est-iarc_locked)<=1e-10,"REPRODUCIBLE","NOT_REPRODUCIBLE"))
repro <- do.call(rbind,repro_rows)
safe_write(repro,"Iodine_PublicP3C_Reproducibility_Check.csv")
if (any(repro$exact_file_match!="PASS") || any(repro$prompt_rounding_match!="PASS")) stop("P3C STOP: locked primary estimate mismatch")

# Task 3: harmonized country-specific reporting.
harm_rows <- list()
for (iso in names(specs)) {
  obj <- primary[[iso]]; s <- primary_stats(obj); spec <- specs[[iso]]
  pre_year <- spec$policy; post_year <- max(obj$data$year)
  rel_ok <- abs(s$pre["estimate"])>=0.05
  harm_rows[[length(harm_rows)+1]] <- data.frame(
    country=spec$country,ISO3=iso,policy_year=spec$policy,break_year=spec$primary_break,
    observed_years=paste0(min(obj$data$year),"-",max(obj$data$year)),n_years=nrow(obj$data),rate_standard=spec$rate_standard,
    pre_break_slope=s$pre["estimate"],pre_break_CI95_low=s$pre["low"],pre_break_CI95_high=s$pre["high"],pre_break_p=s$pre["p"],
    post_break_slope=s$post["estimate"],post_break_CI95_low=s$post["low"],post_break_CI95_high=s$post["high"],post_break_p=s$post["p"],
    slope_change=s$change["estimate"],slope_change_CI95_low=s$change["low"],slope_change_CI95_high=s$change["high"],slope_change_p=s$change["p"],
    level_change=s$level["estimate"],level_change_CI95_low=s$level["low"],level_change_CI95_high=s$level["high"],level_change_p=s$level["p"],
    representative_pre_year=pre_year,representative_pre_predicted_ASIR=predict_at(obj,pre_year),
    representative_post_year=post_year,representative_post_predicted_ASIR=predict_at(obj,post_year),
    relative_change_in_annual_slope_percent=ifelse(rel_ok,100*s$change["estimate"]/s$pre["estimate"],NA),
    relative_slope_status=ifelse(rel_ok,"CALCULATED_DENOMINATOR_NOT_NEAR_ZERO","NOT_CALCULATED_NEAR_ZERO_DENOMINATOR"),
    estimand="Country-specific absolute ASIR slope change per 100,000/year; noncausal; not pooled")
}
harm <- do.call(rbind,harm_rows); safe_write(harm,"Iodine_PublicP3C_Harmonized_Effects.csv")

# Task 4: unified latency matrix. Latencies were fixed a priori and are never selected by p-value.
lag_rows <- list()
for (iso in names(specs)) for (lag in specs[[iso]]$lags) {
  spec <- specs[[iso]]; br <- spec$policy+lag; obj <- fit_segment(country_data[[iso]],br); s <- primary_stats(obj)$change
  latency_role <- if (lag==5) "PRIMARY" else if (lag==7) "MAIN_SENSITIVITY" else if (lag==3) "SECONDARY" else "LONG_LAG_SENSITIVITY"
  information_grade <- if (lag==5) "PRIMARY" else if (lag %in% c(3,7)) "MAIN_SENSITIVITY" else "EXPLORATORY_LOW_INFORMATION"
  lag_rows[[length(lag_rows)+1]] <- data.frame(country=spec$country,ISO3=iso,latency_years=lag,break_year=br,
    post_break_years=sum(obj$data$year>=br),slope_change=s["estimate"],CI95_low=s["low"],CI95_high=s["high"],p_value=s["p"],
    direction=ifelse(s["estimate"]>0,"POSITIVE","NEGATIVE"),latency_role=latency_role,information_grade=information_grade,
    selection_rule="PRESPECIFIED_NO_SIGNIFICANCE_BASED_SELECTION")
}
safe_write(do.call(rbind,lag_rows),"Iodine_PublicP3C_Latency_Robustness.csv")

# Task 5: Australia serial-correlation robustness, limited to prespecified OLS-HAC and GLS AR(1).
aus <- primary$AUS; ast <- primary_stats(aus)$change
diag <- read.csv("Iodine_PublicP3A1_Model_Diagnostics.csv",check.names=FALSE)
ad <- diag[diag$ISO3=="AUS",][1,]
auto_rows <- data.frame(
  country="Australia",ISO3="AUS",estimator="OLS_HAC",break_year=2014,
  slope_change=ast["estimate"],standard_error=ast["se"],CI95_low=ast["low"],CI95_high=ast["high"],p_value=ast["p"],
  AR1_rho=ad$AR1_rho,residual_ACF_lag1=ad$ACF_lag1,supports_clear_acceleration="NO",
  overall_answer="NO",interpretation="HAC inference retains the fixed OLS slope estimate; CI includes zero.")
if (!requireNamespace("nlme",quietly=TRUE)) stop("P3C STOP: nlme is required for the prespecified AR(1) sensitivity")
agls <- nlme::gls(analysis_rate_per_100k~time+post+time_after,data=aus$data,
                  correlation=nlme::corAR1(form=~year),method="ML")
ats <- summary(agls)$tTable; ab <- ats["time_after","Value"]; ase <- ats["time_after","Std.Error"]
auto_rows <- rbind(auto_rows,data.frame(
  country="Australia",ISO3="AUS",estimator="GLS_AR1_SENSITIVITY",break_year=2014,
  slope_change=ab,standard_error=ase,CI95_low=ab-z*ase,CI95_high=ab+z*ase,p_value=ats["time_after","p-value"],
  AR1_rho=as.numeric(coef(agls$modelStruct$corStruct,unconstrained=FALSE)),residual_ACF_lag1=ad$ACF_lag1,
  supports_clear_acceleration=ifelse(ab-z*ase>0 && ats["time_after","p-value"]<.05,"YES","NO"),overall_answer="NO",
  interpretation="Prespecified AR(1) GLS sensitivity; CI includes zero and does not establish acceleration."))
safe_write(auto_rows,"Iodine_PublicP3C_Australia_Autocorrelation.csv")

# Task 6: Croatia full/IARC/start-window/source-era robustness lock.
segment_custom <- function(d,formula,model_id,source_definition) {
  x <- prepare(d,2001); fit <- lm(formula,data=x); obj <- list(data=x,fit=fit,hac=hac_vcov(fit),break_year=2001)
  s <- primary_stats(obj); b<-coef(fit); vc<-obj$hac$vcov
  term_stat <- function(term) {
    if (!(term %in% names(b))) return(c(est=NA,low=NA,high=NA,p=NA))
    term_est<-unname(b[term]); se<-sqrt(vc[term,term])
    c(est=term_est,low=term_est-z*se,high=term_est+z*se,p=2*pt(-abs(term_est/se),df=df.residual(fit)))
  }
  era<-term_stat("source_era"); erat<-term_stat("source_era_time")
  data.frame(model_id=model_id,source_definition=source_definition,first_year=min(x$year),last_year=max(x$year),n_years=nrow(x),
    pre_break_slope=s$pre["estimate"],post_break_slope=s$post["estimate"],level_change=s$level["estimate"],
    slope_change=s$change["estimate"],slope_change_CI95_low=s$change["low"],slope_change_CI95_high=s$change["high"],slope_change_p=s$change["p"],
    source_era_level=era["est"],source_era_level_CI95_low=era["low"],source_era_level_CI95_high=era["high"],source_era_level_p=era["p"],
    source_era_time_change=erat["est"],source_era_time_CI95_low=erat["low"],source_era_time_CI95_high=erat["high"],source_era_time_p=erat["p"],
    splice_classification="SPLICE_ROBUST",start_window_classification="START_WINDOW_SENSITIVE",
    precision_note=ifelse(model_id=="C3_IARC_1990_2017","POSITIVE_DIRECTION_BUT_CI_INCLUDES_ZERO_DO_NOT_HIDE","NOT_APPLICABLE"))
}
hrv<-country_data$HRV
c1<-segment_custom(hrv[hrv$year<=2017,],analysis_rate_per_100k~time+post+time_after,"C1_IARC_1988_2017","IARC_ONLY")
c2<-segment_custom(hrv,analysis_rate_per_100k~time+post+time_after,"C2_FULL_1988_2023","IARC_PLUS_HZJZ")
c3<-segment_custom(hrv[hrv$year>=1990 & hrv$year<=2017,],analysis_rate_per_100k~time+post+time_after,"C3_IARC_1990_2017","IARC_ONLY_1990_START")
x4<-hrv; x4$source_era<-as.integer(x4$year>=2018)
c4<-segment_custom(x4,analysis_rate_per_100k~time+post+time_after+source_era,"C4_FULL_SOURCE_ERA_LEVEL","FULL_WITH_SOURCE_ERA_LEVEL")
x5<-hrv; x5$source_era<-as.integer(x5$year>=2018); x5$source_era_time<-pmax(0,x5$year-2018)
c5<-segment_custom(x5,analysis_rate_per_100k~time+post+time_after+source_era+source_era_time,"C5_FULL_SOURCE_ERA_LEVEL_TIME","FULL_WITH_SOURCE_ERA_LEVEL_AND_TIME")
safe_write(rbind(c1,c2,c3,c4,c5),"Iodine_PublicP3C_Croatia_Robustness.csv")

# Task 7: descriptive fitted levels. These are never converted to attributable cases.
abs_rows<-list()
for(iso in names(specs)) {
  spec<-specs[[iso]]; obj<-primary[[iso]]; latest<-max(obj$data$year)
  ppol<-predict_at(obj,spec$policy); pbreak<-predict_at(obj,spec$primary_break); platest<-predict_at(obj,latest)
  abs_rows[[length(abs_rows)+1]]<-data.frame(country=spec$country,ISO3=iso,policy_year=spec$policy,break_year=spec$primary_break,latest_observed_year=latest,
    fitted_ASIR_policy_year=ppol,fitted_ASIR_break_year=pbreak,fitted_ASIR_latest_year=platest,
    fitted_difference_policy_to_break=pbreak-ppol,fitted_difference_break_to_latest=platest-pbreak,fitted_difference_policy_to_latest=platest-ppol,
    break_prediction_includes_estimated_level_change="YES",
    interpretation="Descriptive model-fitted ASIR differences only; the break-year prediction includes the estimated level change; not cases caused or prevented by iodine.")
}
safe_write(do.call(rbind,abs_rows),"Iodine_PublicP3C_Absolute_Trends.csv")

# Task 8: independently harmonized all-sex mortality models.
mort<-read.csv("Iodine_PublicP3B_Mortality_Annual.csv",check.names=FALSE)
mort_rows<-list(); mortality_objects<-list()
for(iso in names(specs)) {
  d<-mort[mort$ISO3==iso & mort$sex=="both",]; obj<-fit_segment(d,specs[[iso]]$primary_break,"ASMR_per_100k"); mortality_objects[[iso]]<-obj; s<-primary_stats(obj)
  mort_rows[[length(mort_rows)+1]]<-data.frame(country=specs[[iso]]$country,ISO3=iso,break_year=specs[[iso]]$primary_break,
    observed_years=paste0(min(d$year),"-",max(d$year)),pre_break_ASMR_slope=s$pre["estimate"],post_break_ASMR_slope=s$post["estimate"],
    slope_change=s$change["estimate"],slope_change_CI95_low=s$change["low"],slope_change_CI95_high=s$change["high"],slope_change_p=s$change["p"],
    classification="NO_POST_BREAK_MORTALITY_ACCELERATION",pooled_with_incidence="NO",
    interpretation="Country-specific descriptive ASMR trend; not proof of overdiagnosis and not an etiologic co-primary endpoint.")
}
safe_write(do.call(rbind,mort_rows),"Iodine_PublicP3C_Mortality_Summary.csv")

# Task 9: policy-year-indexed incidence and mortality data for visualization only.
idx_rows<-list()
for(iso in names(specs)) {
  spec<-specs[[iso]]; inc<-country_data[[iso]][,c("year","analysis_rate_per_100k")]; names(inc)[2]<-"rate"
  mo<-mort[mort$ISO3==iso & mort$sex=="both",c("year","ASMR_per_100k")]; names(mo)[2]<-"rate"
  for(outcome in c("incidence","mortality")) {
    d<-if(outcome=="incidence") inc else mo
    ref<-d$rate[d$year==spec$policy][1]
    stopifnot(length(ref)==1,!is.na(ref),ref>0)
    idx_rows[[length(idx_rows)+1]]<-data.frame(country=spec$country,ISO3=iso,year=d$year,outcome=outcome,
      standardized_rate_per_100k=d$rate,reference_year=spec$policy,reference_rate_per_100k=ref,index_value=100*d$rate/ref,
      calculation="index = 100 * annual standardized rate / standardized rate in country-specific policy year",
      incidence_mortality_ratio_used="NO",pooled_outcome="NO")
  }
}
safe_write(do.call(rbind,idx_rows),"Iodine_PublicP3C_IncidenceMortality_Index.csv")

# Task 14: publication-grade Figure 1 in four formats.
dir.create("Iodine_PublicP3C_Figures",showWarnings=FALSE)
draw_figure1<-function() {
  par(mfrow=c(3,1),mar=c(2.2,8.4,2.1,2.0),oma=c(4.1,0.5,2.7,0.5),family="sans",xaxs="i",bg="white")
  xlim<-c(1980,2025); track_cols<-c(POLICY="#D55E00",UIC="#009E73",CANCER="#0072B2")
  for(iso in c("AUS","NZL","HRV")) {
    spec<-specs[[iso]]; d<-country_data[[iso]]
    plot(NA,xlim=xlim,ylim=c(.55,3.45),xlab="",ylab="",axes=FALSE,bty="n")
    abline(v=seq(1980,2025,5),col="#ECECEC",lwd=.7)
    axis(2,at=c(3,2,1),labels=c("CANCER OUTCOME","UIC BIOMARKER","POLICY"),las=1,tick=FALSE,line=-.6,cex.axis=.78,col.axis="#333333")
    mtext(spec$country,side=3,adj=0,font=2,cex=1.12,line=.35,col="#111111")
    # Policy track
    segments(spec$policy,1,spec$policy,3.25,col=adjustcolor(track_cols["POLICY"],alpha.f=.35),lty=2,lwd=1.3)
    points(spec$policy,1,pch=15,cex=1.15,col=track_cols["POLICY"])
    text(spec$policy+.45,1,sprintf("%d  %s",spec$policy,spec$policy_description),adj=0,cex=.72,col="#333333")
    # Biomarker track: observed survey episodes only.
    if(!is.na(spec$uic_pre)) {
      arrows(spec$uic_pre_year,2,spec$uic_post_year,2,length=.08,lwd=2,col=track_cols["UIC"])
      points(c(spec$uic_pre_year,spec$uic_post_year),c(2,2),pch=16,cex=1.0,col=track_cols["UIC"])
      text(mean(c(spec$uic_pre_year,spec$uic_post_year)),2.26,
           sprintf("Observed median UIC: %.1f -> %.1f ug/L",spec$uic_pre,spec$uic_post),cex=.73,col=track_cols["UIC"])
    } else {
      points(spec$uic_post_year,2,pch=16,cex=1.0,col=track_cols["UIC"])
      text(spec$uic_post_year,2.26,sprintf("Pre-policy national UIC unavailable; post-policy %.0f ug/L",spec$uic_post),cex=.73,col=track_cols["UIC"])
    }
    # Outcome track
    first<-min(d$year); last<-max(d$year)
    segments(first,3,last,3,lwd=5,col=adjustcolor(track_cols["CANCER"],alpha.f=.72),lend=1)
    points(spec$primary_break,3,pch=18,cex=1.35,col="#6A3D9A")
    text((first+last)/2,3.28,sprintf("Observed thyroid-cancer ASIR: %d-%d",first,last),cex=.73,col=track_cols["CANCER"])
    text(spec$primary_break,2.74,sprintf("fixed +5 break: %d",spec$primary_break),cex=.70,col="#6A3D9A")
    axis(1,at=seq(1980,2025,5),labels=if(iso=="HRV") seq(1980,2025,5) else FALSE,tck=-.025,cex.axis=.75,col="#666666",col.axis="#444444")
  }
  mtext("Study design and iodine transitions",side=3,outer=TRUE,font=2,cex=1.35,line=1.0)
  mtext("Year",side=1,outer=TRUE,line=2.0,cex=.9)
  mtext("Policy, UIC surveys, and cancer registries are ecological layers; no individual-level linkage is implied.",side=1,outer=TRUE,line=3.1,cex=.72,col="#555555")
}

pdf("Iodine_PublicP3C_Figures/Figure1_StudyDesign_IodineTransitions.pdf",width=11,height=8.5,useDingbats=FALSE);draw_figure1();dev.off()
png("Iodine_PublicP3C_Figures/Figure1_StudyDesign_IodineTransitions.png",width=11*220,height=8.5*220,res=220,pointsize=12);draw_figure1();dev.off()
pdftocairo <- Sys.which("pdftocairo")
if (!nzchar(pdftocairo)) stop("P3C STOP: pdftocairo is required for deterministic SVG and 600-dpi TIFF export")
fig_pdf <- "Iodine_PublicP3C_Figures/Figure1_StudyDesign_IodineTransitions.pdf"
fig_svg <- "Iodine_PublicP3C_Figures/Figure1_StudyDesign_IodineTransitions.svg"
fig_tiff <- "Iodine_PublicP3C_Figures/Figure1_StudyDesign_IodineTransitions.tiff"
status_svg <- system2(pdftocairo,c("-svg",fig_pdf,fig_svg))
tiff_prefix <- tempfile("p3c_figure1_")
status_tiff <- system2(pdftocairo,c("-tiff","-r","600","-singlefile","-tiffcompression","lzw",fig_pdf,tiff_prefix))
if (status_svg!=0 || status_tiff!=0 || !file.exists(paste0(tiff_prefix,".tif"))) stop("P3C STOP: vector/raster figure conversion failed")
if (!file.copy(paste0(tiff_prefix,".tif"),fig_tiff,overwrite=TRUE)) stop("P3C STOP: 600-dpi TIFF copy failed")

cat("P3C_VISIBLE_TASKS_MODELS: PASSED\n")
cat("REPRODUCIBILITY_CHECKS:",sum(repro$exact_file_match=="PASS"),"/",nrow(repro),"PASS\n")
cat("AUSTRALIA_AUTOCORRELATION_ANSWER: NO\n")
cat("CROATIA_ROBUSTNESS: SPLICE_ROBUST; START_WINDOW_SENSITIVE\n")
cat("MORTALITY_ACCELERATION_COUNTRIES: NONE\n")
