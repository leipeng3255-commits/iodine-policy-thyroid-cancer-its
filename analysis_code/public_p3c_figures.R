#!/usr/bin/env Rscript

# Public-data Stage P3C Tasks 15-19.
# This script consumes verified P3C/P3B outputs as frozen inputs. It does not
# refit or alter any primary model and never calculates a pooled estimate.

options(stringsAsFactors=FALSE,scipen=999)
args<-commandArgs(trailingOnly=FALSE)
script_arg<-grep("^--file=",args,value=TRUE)
script_path<-normalizePath(sub("^--file=","",script_arg[1]))
root<-dirname(dirname(script_path));setwd(root)

stopifnot(requireNamespace("ggplot2",quietly=TRUE),requireNamespace("patchwork",quietly=TRUE))
library(ggplot2);library(patchwork)

COUNTRIES<-c(AUS="Australia",NZL="New Zealand",HRV="Croatia")
PANELS<-c(AUS="A  Australia",NZL="B  New Zealand",HRV="C  Croatia")
POLICY<-c(AUS=2009,NZL=2009,HRV=1996)
BREAKS<-c(AUS=2014,NZL=2014,HRV=2001)
INC_FILES<-c(AUS="Iodine_PublicP3A1_Australia_Incidence.csv",NZL="Iodine_PublicP3A1_NewZealand_Incidence.csv",HRV="Iodine_PublicP3A1_Croatia_Incidence.csv")

cb<-c(blue="#0072B2",orange="#D55E00",teal="#009E73",purple="#6A3D9A",grey="#666666",lightgrey="#B8B8B8",black="#222222")
theme_p3c<-theme_bw(base_size=9.5,base_family="Times")+
  theme(panel.grid.minor=element_blank(),panel.grid.major=element_line(color="#E8E8E8",linewidth=.28),
        panel.border=element_rect(color="#4D4D4D",linewidth=.4),strip.background=element_rect(fill="white",color="white"),
        strip.text=element_text(face="bold",size=10,hjust=0),axis.title=element_text(size=9.5),
        axis.text=element_text(size=8.2,color="#222222"),legend.position="bottom",legend.title=element_blank(),
        legend.text=element_text(size=8),plot.title=element_text(face="bold",size=11,hjust=0),
        plot.subtitle=element_text(size=8.5,color="#444444"),plot.caption=element_text(size=7.2,color="#555555",hjust=0))

save_multiformat<-function(plot,base,width,height) {
  pdf_path<-paste0(base,".pdf");png_path<-paste0(base,".png");svg_path<-paste0(base,".svg");tiff_path<-paste0(base,".tiff")
  pdf(pdf_path,width=width,height=height,family="Times",useDingbats=FALSE,bg="white");print(plot);dev.off()
  png_backend<-if(Sys.info()[["sysname"]]=="Darwin") "quartz" else if(capabilities("cairo")) "cairo" else getOption("bitmapType")
  png(png_path,width=width*220,height=height*220,res=220,type=png_backend,bg="white",pointsize=12);print(plot);dev.off()
  converter<-Sys.which("pdftocairo");if(!nzchar(converter))stop("pdftocairo required")
  if(system2(converter,c("-svg",pdf_path,svg_path))!=0)stop("SVG conversion failed")
  prefix<-tempfile("p3c_figure_")
  if(system2(converter,c("-tiff","-r","600","-singlefile","-tiffcompression","lzw",pdf_path,prefix))!=0)stop("TIFF conversion failed")
  if(!file.copy(paste0(prefix,".tif"),tiff_path,overwrite=TRUE))stop("TIFF copy failed")
}

safe_write<-function(x,path)write.csv(x,path,row.names=FALSE,na="")
fmt<-function(x,d=3)formatC(x,digits=d,format="f")

# ------------------------------- Figure 2 ---------------------------------
harm<-read.csv("Iodine_PublicP3C_Harmonized_Effects.csv",check.names=FALSE)
absfit<-read.csv("Iodine_PublicP3C_Absolute_Trends.csv",check.names=FALSE)
f2rows<-list()
for(iso in names(COUNTRIES)) {
  d<-read.csv(INC_FILES[iso],check.names=FALSE);d<-d[d$model_inclusion=="YES",]
  h<-harm[harm$ISO3==iso,][1,];a<-absfit[absfit$ISO3==iso,][1,]
  fitted<-ifelse(d$year<BREAKS[iso],
                 a$fitted_ASIR_policy_year+h$pre_break_slope*(d$year-POLICY[iso]),
                 a$fitted_ASIR_break_year+h$post_break_slope*(d$year-BREAKS[iso]))
  f2rows[[length(f2rows)+1]]<-data.frame(panel=unname(PANELS[iso]),country=unname(COUNTRIES[iso]),ISO3=iso,year=d$year,
    observed_ASIR_per_100k=d$analysis_rate_per_100k,fitted_segmented_ASIR_per_100k=fitted,
    fitted_segment=ifelse(d$year<BREAKS[iso],"Fitted pre-break trend","Fitted post-break trend"),
    policy_year=unname(POLICY[iso]),primary_break_year=unname(BREAKS[iso]),pre_break_slope=h$pre_break_slope,
    post_break_slope=h$post_break_slope,slope_change=h$slope_change,
    slope_change_CI95_low=h$slope_change_CI95_low,slope_change_CI95_high=h$slope_change_CI95_high,
    observed_status="OBSERVED",projected="NO",pooled_estimate="NO")
}
f2<-do.call(rbind,f2rows);f2$panel<-factor(f2$panel,levels=unname(PANELS))
safe_write(f2,"Iodine_PublicP3C_Figure2_SourceData.csv")
f2long<-rbind(
  data.frame(panel=f2$panel,year=f2$year,value=f2$observed_ASIR_per_100k,series="Observed annual ASIR"),
  data.frame(panel=f2$panel,year=f2$year,value=f2$fitted_segmented_ASIR_per_100k,series=f2$fitted_segment))
mark2<-do.call(rbind,lapply(names(COUNTRIES),function(iso){q<-f2[f2$ISO3==iso,];data.frame(panel=unname(PANELS[iso]),
  marker=c("Policy year","Fixed +5-year breakpoint"),year=c(POLICY[iso],BREAKS[iso]),y=max(q$observed_ASIR_per_100k)*c(1.035,1.035))}))
mark2$panel<-factor(mark2$panel,levels=unname(PANELS))
ann2<-do.call(rbind,lapply(names(COUNTRIES),function(iso){h<-harm[harm$ISO3==iso,][1,];q<-f2[f2$ISO3==iso,];data.frame(panel=unname(PANELS[iso]),
  x=min(q$year)+1,y=max(q$observed_ASIR_per_100k)*.88,
  label=paste0("Pre-break slope: ",fmt(h$pre_break_slope),"/year\nPost-break slope: ",fmt(h$post_break_slope),"/year\nSlope change: ",fmt(h$slope_change)," (95% CI ",fmt(h$slope_change_CI95_low)," to ",fmt(h$slope_change_CI95_high),")"))}))
ann2$panel<-factor(ann2$panel,levels=unname(PANELS))
p2<-ggplot(f2long,aes(year,value,color=series,linetype=series,group=series))+
  geom_line(data=subset(f2long,series=="Observed annual ASIR"),linewidth=.58)+
  geom_point(data=subset(f2long,series=="Observed annual ASIR"),size=.95,shape=21,fill="white",stroke=.45)+
  geom_line(data=subset(f2long,series!="Observed annual ASIR"),linewidth=1.0)+
  geom_vline(data=subset(mark2,marker=="Policy year"),aes(xintercept=year),color=unname(cb["grey"]),linetype="dotted",linewidth=.75)+
  geom_vline(data=subset(mark2,marker=="Fixed +5-year breakpoint"),aes(xintercept=year),color=unname(cb["purple"]),linetype="dotdash",linewidth=.82)+
  geom_point(data=mark2,aes(year,y,shape=marker),inherit.aes=FALSE,size=2.3,color=unname(c(cb["grey"],cb["purple"])[match(mark2$marker,c("Policy year","Fixed +5-year breakpoint"))]))+
  geom_label(data=ann2,aes(x,y,label=label),inherit.aes=FALSE,hjust=0,vjust=1,size=2.65,family="Times",linewidth=.18,label.padding=grid::unit(.12,"lines"),fill=adjustcolor("white",.88))+
  facet_wrap(~panel,ncol=1,scales="free_y")+
  scale_color_manual(values=c("Observed annual ASIR"=unname(cb["black"]),"Fitted pre-break trend"=unname(cb["blue"]),"Fitted post-break trend"=unname(cb["orange"])))+
  scale_linetype_manual(values=c("Observed annual ASIR"="solid","Fitted pre-break trend"="longdash","Fitted post-break trend"="solid"))+
  scale_shape_manual(values=c("Policy year"=17,"Fixed +5-year breakpoint"=18))+
  labs(x="Calendar year",y="Thyroid-cancer ASIR per 100,000",
       title="Country-specific thyroid-cancer incidence trajectories",
       subtitle="Observed national registry rates and fixed-break segmented trends; policy and latency markers are distinct",
       caption="Country-specific models only. UIC is not plotted on the cancer-incidence axis; no pooled estimate is shown.")+
  guides(color=guide_legend(order=1),linetype=guide_legend(order=1),shape=guide_legend(order=2))+theme_p3c
save_multiformat(p2,"Iodine_PublicP3C_Figure2",10.5,9.0)
writeLines(c("# Figure 2 caption draft","",
"Country-specific thyroid-cancer incidence trajectories in Australia, New Zealand, and Croatia. Points and thin dark lines show observed annual national age-standardised incidence rates; blue long-dashed and orange solid lines show the fitted pre- and post-break segments, respectively. Grey dotted vertical lines and triangles mark iodine-policy years; purple dot-dashed lines and diamonds mark the prespecified policy-plus-5-year cancer breakpoints. Annotations report absolute annual ASIR slopes and slope changes with 95% confidence intervals. Models are country-specific and noncausal; no pooled estimate is calculated."),"Iodine_PublicP3C_Figure2_CaptionDraft.md")

# ------------------------------- Figure 3 ---------------------------------
primary3<-data.frame(panel="A",display=harm$country,country=harm$country,ISO3=harm$ISO3,model="PRIMARY_PLUS5",
  latency_years=5,estimate=harm$slope_change,CI95_low=harm$slope_change_CI95_low,CI95_high=harm$slope_change_CI95_high,
  role="PRIMARY +5",pooled="NO")
lat<-read.csv("Iodine_PublicP3C_Latency_Robustness.csv",check.names=FALSE)
rolemap<-ifelse(lat$latency_years==5,"PRIMARY +5",ifelse(lat$latency_years==7,"MAIN SENSITIVITY +7","EXPLORATORY"))
lat3<-data.frame(panel="B",display=paste0(lat$country,"  +",lat$latency_years),country=lat$country,ISO3=lat$ISO3,
  model=paste0("LAG",lat$latency_years),latency_years=lat$latency_years,estimate=lat$slope_change,CI95_low=lat$CI95_low,CI95_high=lat$CI95_high,
  role=rolemap,pooled="NO")
cro<-read.csv("Iodine_PublicP3C_Croatia_Robustness.csv",check.names=FALSE)
clabel<-c(C1_IARC_1988_2017="IARC-only 1988-2017",C2_FULL_1988_2023="Full series",C3_IARC_1990_2017="IARC-only 1990-2017",
          C4_FULL_SOURCE_ERA_LEVEL="Source-era level",C5_FULL_SOURCE_ERA_LEVEL_TIME="Source-era level x time")
cro3<-data.frame(panel="C",display=unname(clabel[cro$model_id]),country="Croatia",ISO3="HRV",model=cro$model_id,latency_years=5,
  estimate=cro$slope_change,CI95_low=cro$slope_change_CI95_low,CI95_high=cro$slope_change_CI95_high,role="CROATIA ROBUSTNESS",pooled="NO")
f3<-rbind(primary3,lat3,cro3);safe_write(f3,"Iodine_PublicP3C_Figure3_SourceData.csv")
forest_panel<-function(d,title,subtitle=NULL,color_by_role=FALSE) {
  d$display<-factor(d$display,levels=rev(unique(d$display)))
  base<-ggplot(d,aes(estimate,display))+
    geom_vline(xintercept=0,linetype="dashed",color="#777777",linewidth=.55)+
    geom_segment(aes(x=CI95_low,xend=CI95_high,y=display,yend=display),linewidth=.75,color="#444444")+
    labs(x="ASIR slope change per 100,000/year",y=NULL,title=title,subtitle=subtitle)+theme_p3c+
    theme(legend.position="bottom",panel.grid.major.y=element_blank(),plot.title=element_text(size=10))
  if(color_by_role) base+geom_point(aes(shape=role,color=role),size=2.5,stroke=.8)+
      scale_shape_manual(values=c("PRIMARY +5"=15,"MAIN SENSITIVITY +7"=16,"EXPLORATORY"=4))+
      scale_color_manual(values=c("PRIMARY +5"=unname(cb["blue"]),"MAIN SENSITIVITY +7"=unname(cb["teal"]),"EXPLORATORY"=unname(cb["grey"])))
  else base+geom_point(shape=15,size=2.5,color=unname(cb["blue"]))
}
p3a<-forest_panel(primary3,"Primary +5-year estimates","No pooled diamond")
p3b<-forest_panel(lat3,"Prespecified latency robustness",NULL,TRUE)
p3c<-forest_panel(cro3,"Croatia source/start-window checks","All retain the fixed 2001 breakpoint")
p3<-((p3a|p3b|p3c)+plot_layout(widths=c(.82,1.52,1.12),guides="collect")+
  plot_annotation(title="Country-specific post-latency slope changes and robustness",
    subtitle="Points are country-specific absolute ASIR slope changes; horizontal lines are 95% confidence intervals",
    caption="No pooled estimate or pooled diamond is calculated. +5 is primary; +7 is the main sensitivity; +3, +10, and Croatia +15 are exploratory.",
    tag_levels="A",theme=theme(plot.title=element_text(family="Times",face="bold",size=12),plot.subtitle=element_text(family="Times",size=9),plot.caption=element_text(family="Times",size=7.5)))) & theme(legend.position="bottom")
save_multiformat(p3,"Iodine_PublicP3C_Figure3",13.0,7.5)
writeLines(c("# Figure 3 caption draft","",
"Country-specific post-latency thyroid-cancer incidence slope changes and robustness analyses. Panel A shows the primary policy-plus-5-year slope-change estimates and 95% confidence intervals for Australia, New Zealand, and Croatia. Panel B shows all prespecified latency analyses; marker shape and color distinguish the primary +5-year model, the main +7-year sensitivity, and exploratory +3-, +10-, and Croatia +15-year analyses. Panel C shows Croatia full-series, IARC-only, 1990-start, and source-era specifications. The zero line denotes no slope change. Estimates are not pooled, and no pooled diamond is shown."),"Iodine_PublicP3C_Figure3_CaptionDraft.md")

# ------------------------------- Figure 4 ---------------------------------
f4<-read.csv("Iodine_PublicP3C_IncidenceMortality_Index.csv",check.names=FALSE)
f4$panel<-factor(PANELS[f4$ISO3],levels=unname(PANELS));f4$series<-ifelse(f4$outcome=="incidence","Incidence index","Mortality index")
f4$observed_status<-"OBSERVED";f4$projected<-"NO";f4$pooled_outcome<-"NO"
safe_write(f4,"Iodine_PublicP3C_Figure4_SourceData.csv")
mark4<-do.call(rbind,lapply(names(COUNTRIES),function(iso){q<-f4[f4$ISO3==iso,];data.frame(panel=unname(PANELS[iso]),marker=c("Policy year","Fixed +5-year breakpoint"),
  year=c(POLICY[iso],BREAKS[iso]),y=max(q$index_value)*1.03)}));mark4$panel<-factor(mark4$panel,levels=unname(PANELS))
p4<-ggplot(f4,aes(year,index_value,color=series,linetype=series,group=series))+
  geom_hline(yintercept=100,color="#9A9A9A",linewidth=.35)+geom_line(linewidth=.78)+
  geom_vline(data=subset(mark4,marker=="Policy year"),aes(xintercept=year),color=unname(cb["grey"]),linetype="dotted",linewidth=.75)+
  geom_vline(data=subset(mark4,marker=="Fixed +5-year breakpoint"),aes(xintercept=year),color=unname(cb["purple"]),linetype="dotdash",linewidth=.82)+
  geom_point(data=mark4,aes(year,y,shape=marker),inherit.aes=FALSE,size=2.25,color=unname(c(cb["grey"],cb["purple"])[match(mark4$marker,c("Policy year","Fixed +5-year breakpoint"))]))+
  facet_wrap(~panel,ncol=1,scales="free_y")+
  scale_color_manual(values=c("Incidence index"=unname(cb["blue"]),"Mortality index"=unname(cb["orange"])))+
  scale_linetype_manual(values=c("Incidence index"="solid","Mortality index"="longdash"))+
  scale_shape_manual(values=c("Policy year"=17,"Fixed +5-year breakpoint"=18))+
  labs(x="Calendar year",y="Policy-year index (policy year = 100)",title="Incidence-mortality triangulation",
       subtitle="Incidence and mortality are indexed separately to each country's observed policy-year rate",
       caption="Index = 100 x annual standardized rate / observed standardized rate in the country-specific policy year. No incidence:mortality ratio is used. Divergence is not proof of overdiagnosis.")+
  guides(color=guide_legend(order=1),linetype=guide_legend(order=1),shape=guide_legend(order=2))+theme_p3c
save_multiformat(p4,"Iodine_PublicP3C_Figure4",10.5,9.0)
writeLines(c("# Figure 4 caption draft","",
"Country-specific thyroid-cancer incidence and mortality trajectories indexed to 100 in each country's observed iodine-policy year. Blue solid lines show incidence indices and orange long-dashed lines show mortality indices; the outcomes remain separate and are not combined into an effect estimate. Grey dotted policy markers and purple dot-dashed policy-plus-5-year breakpoint markers are visually distinct. Index values equal 100 times the annual standardized rate divided by the observed standardized rate in the country-specific policy year. Incidence-mortality divergence is not proof of overdiagnosis."),"Iodine_PublicP3C_Figure4_CaptionDraft.md")

# ------------------------------- Figure 5 ---------------------------------
domains<-c("Documented\niodine policy","Biological\nUIC response","Pre-policy\nincidence rise","Post-5-year\nslope change","Latency\nrobustness",
           "Mortality\nacceleration","Histology\nevidence","Diagnostic-era\nconcern","Source-continuity\nconcern")
matrix_labels<-list(
  AUS=c("Yes\nBread, 2009","Moderate","Yes","No clear change","No clear\nacceleration","No","Moderate\nSupplement","High","Low"),
  NZL=c("Yes\nBread, 2009","Strong","Yes","Decrease","Decrease in\n+3/+5/+7; +10\nimprecise","No","Moderate\nSupplement","Moderate","Low"),
  HRV=c("Yes\nSalt, 1996","Uncertain","Yes","Increase","Positive across\nlags; start-window\nsensitive","No","Weak\nText only","High","Moderate"))
groups<-c("Exposure","Exposure","Temporal incidence","Temporal incidence","Temporal incidence","Mortality","Histology","Bias/context","Bias/context")
f5<-do.call(rbind,lapply(names(COUNTRIES),function(iso)data.frame(country=unname(COUNTRIES[iso]),ISO3=iso,domain=domains,cell_label=matrix_labels[[iso]],domain_group=groups,
  evidence_type="QUALITATIVE_CATEGORY_NOT_NUMERIC_SCORE",pooled="NO")))
f5$country<-factor(f5$country,levels=rev(unname(COUNTRIES)));f5$domain<-factor(f5$domain,levels=domains)
safe_write(f5,"Iodine_PublicP3C_Figure5_SourceData.csv")
group_cols<-c("Exposure"="#E8F1F5","Temporal incidence"="#EDF3E7","Mortality"="#EEEEEE","Histology"="#F5EEE5","Bias/context"="#F5E7E9")
p5<-ggplot(f5,aes(domain,country,fill=domain_group))+
  geom_tile(color="white",linewidth=1.3)+geom_text(aes(label=cell_label),family="Times",size=3.05,lineheight=.92,color="#222222")+
  scale_fill_manual(values=group_cols,guide="none")+
  scale_x_discrete(position="top")+
  labs(x=NULL,y=NULL,title="Cross-country evidence synthesis",
       subtitle="Heterogeneous cancer-incidence responses despite documented population iodine policies",
       caption="Cells contain direct qualitative evidence labels, not arbitrary numeric scores. Histology remains supplementary/text only and does not drive the central claim.")+
  theme_p3c+theme(panel.grid=element_blank(),panel.border=element_blank(),axis.text.x=element_text(angle=33,hjust=0,vjust=0,size=8.2,face="bold"),
    axis.text.y=element_text(size=9.2,face="bold"),legend.position="none",plot.margin=margin(8,12,8,8))
save_multiformat(p5,"Iodine_PublicP3C_Figure5",12.0,5.2)
writeLines(c("# Figure 5 caption draft","",
"Cross-country qualitative evidence synthesis for Australia, New Zealand, and Croatia. Cells report documented policy, observed UIC validation, pre-policy incidence timing, country-specific post-latency slope direction, latency robustness, mortality acceleration, histology quality, and diagnostic/source-continuity concerns. Background colors identify evidence domains only and are not numeric scores. The temporal cancer response is heterogeneous despite population iodine repletion; the figure does not provide a pooled or causal estimate."),"Iodine_PublicP3C_Figure5_CaptionDraft.md")

# ----------------------- Supplementary histology ---------------------------
hist<-read.csv("Iodine_PublicP3B_Histology_Annual.csv",check.names=FALSE);hist<-hist[hist$sex=="both" & hist$ISO3 %in% c("AUS","NZL"),]
hist<-hist[(hist$ISO3=="AUS" & hist$year>=1993)|(hist$ISO3=="NZL" & hist$year>=2001),]
hs<-rbind(
  data.frame(panel=ifelse(hist$ISO3=="AUS","A  Australia","B  New Zealand"),country=hist$country,ISO3=hist$ISO3,year=hist$year,subtype="PTC",proportion_percent=hist$PTC_proportion_percent,unspecified_explicit="YES"),
  data.frame(panel=ifelse(hist$ISO3=="AUS","A  Australia","B  New Zealand"),country=hist$country,ISO3=hist$ISO3,year=hist$year,subtype="FTC",proportion_percent=hist$FTC_proportion_percent,unspecified_explicit="YES"),
  data.frame(panel=ifelse(hist$ISO3=="AUS","A  Australia","B  New Zealand"),country=hist$country,ISO3=hist$ISO3,year=hist$year,subtype="Unspecified morphology",proportion_percent=hist$unspecified_proportion_percent,unspecified_explicit="YES"))
hs$panel<-factor(hs$panel,levels=c("A  Australia","B  New Zealand"));safe_write(hs,"Iodine_PublicP3C_FigureS1_Histology_SourceData.csv")
shade<-data.frame(panel=factor(c("A  Australia","B  New Zealand"),levels=levels(hs$panel)),xmin=2014,xmax=2017,ymin=-Inf,ymax=Inf)
ph<-ggplot(hs,aes(year,proportion_percent,color=subtype,linetype=subtype))+
  geom_rect(data=shade,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),inherit.aes=FALSE,fill="#EDE7F4",alpha=.7)+
  geom_vline(xintercept=2014,color=unname(cb["purple"]),linetype="dotdash",linewidth=.75)+geom_line(linewidth=.8)+geom_point(size=.95)+
  facet_wrap(~panel,ncol=1)+scale_color_manual(values=c(PTC=unname(cb["blue"]),FTC=unname(cb["orange"]),"Unspecified morphology"=unname(cb["grey"])))+
  scale_linetype_manual(values=c(PTC="solid",FTC="longdash","Unspecified morphology"="dotted"))+
  labs(x="Calendar year",y="Percent of thyroid-cancer cases",title="Supplementary histology composition",
       subtitle="Stable later-quality eras; shaded region denotes the four observed years after the fixed 2014 breakpoint",
       caption="Subtype proportions are compositional and do not estimate population subtype incidence. Unspecified morphology is displayed explicitly. Croatia remains text only because its ICD-O-3 transition coincides with the fixed breakpoint.")+theme_p3c
save_multiformat(ph,"Iodine_PublicP3C_FigureS1_Histology",10.0,7.2)
writeLines(c("# Figure S1 caption draft","",
"Supplementary thyroid-cancer histology composition in the stable later-quality series for Australia and New Zealand. PTC, FTC, and unspecified morphology are shown explicitly. Purple dot-dashed lines mark the fixed 2014 breakpoints and shading marks the four available post-break years through 2017. These case-denominator proportions are descriptive and do not estimate population subtype incidence. Croatia is not plotted because the 2001 ICD-O-3 transition coincides with its fixed breakpoint."),"Iodine_PublicP3C_FigureS1_Histology_CaptionDraft.md")

cat("P3C_FIGURES_2_TO_5: COMPLETE\n")
cat("P3C_SUPPLEMENTARY_HISTOLOGY: AUS_NZL_COMPLETE; CROATIA_TEXT_ONLY\n")
cat("POOLED_ESTIMATE_CREATED: NO\n")
