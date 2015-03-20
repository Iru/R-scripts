require(RPostgreSQL)
require(data.table)
require(ggplot2)

if (!is.null(run.once)) {
  # connect to database
  con <- dbConnect(dbDriver("PostgreSQL"), dbname="analytics")
  
  # 
  # Pull claim data from database. Convert to data.table form for ease of 
  # manipulation. Convert strings to factors and shorten some names while we're at
  # it
  # 
  claim.dt<-data.table(dbReadTable(con,c("hci3_staging", "episode_claim")), key="episode")
  claim.dt[,c("dx_code"
              , "px_code"
              , "assignment_type"
              , "type_pos_cd"
              , "type",    "episode_acronym"
              , "pcp_npi", "pcp_provider_npi"
              , "doc_npi", "attributed_phys_npi"
              , "fac_npi", "fac_prvdr_npi_num"
              , "sup_npi", "spprt_prvdr_npi_num") :=
             list(factor(dx_code)
                  , factor(px_code)
                  , factor(assignment_type)
                  , factor(type_pos_cd)
                  , factor(episode_acronym), NULL
                  , factor(pcp_provider_npi), NULL
                  , factor(attributed_phys_npi), NULL
                  , factor(fac_prvdr_npi_num), NULL
                  , factor(spprt_prvdr_npi_num), NULL
             )]
  
  # 
  # Pull the code table from the database. Convert to data.table form for ease of 
  # manipulation. Convert strings to factors
  # 
  code.table.dt <- data.table(dbReadTable(con,c("hci3_staging", "code_type")))
  code.table.dt[,c("code_id"
                   , "type_id"
                   , "group_id"
                   , "code_name"
                   , "group_name"
                   , "specific_type_id") :=
                  list(factor(code_id)
                       , factor(type_id)
                       , factor(group_id)
                       , factor(code_name)
                       , factor(group_name)
                       , NULL
                  )]
  run.once<-NULL
}
setkey(code.table.dt, type_id)
dx.code.dt<-code.table.dt["DX"]
rx.code.dt<-code.table.dt["RX"]
px.code.dt<-code.table.dt[type_id!="DX" & type_id!="RX"]


# First build facility & support only tables
facility.dt<-claim.dt[!is.na(fac_npi),
                      .(type
                        , doc_npi
                        , pcp_npi
                        , total.fac=sum(tot_amt)
                        , covrd.fac=sum(cvrd_amt)                                 
                      ), by="episode,fac_npi"]

support.dt<-claim.dt[!is.na(sup_npi),
                     .(type
                       , doc_npi
                       , pcp_npi
                       , total.sup=sum(tot_amt)
                       , covrd.sup=sum(cvrd_amt)                                 
                     ), by="episode,sup_npi"]

# create episode level view - will need more of these that show dx/px groups later
episode.dt<-unique(claim.dt[,
                            .(type
                              , doc_npi
                              , pcp_npi
                              , total.all=sum(tot_amt)
                              , covrd.all=sum(cvrd_amt)                                 
                            ), by=episode])

episode.dt<-merge(x=episode.dt
                  , y=facility.dt[,
                               .(total.fac=sum(total.fac)
                                 , covrd.fac=sum(covrd.fac)
                               ), by=episode]
                  , all.x=TRUE)
episode.dt<-merge(x=episode.dt
                  , y=support.dt[,
                               .(total.sup=sum(total.sup)
                                 , covrd.sup=sum(covrd.sup)
                               ), by=episode]
                  , all.x=TRUE)

means.dt<-rbindlist(list(
  episode.dt[,
             .(type=NA  
               , total.all=mean(total.all, na.rm=TRUE)
               , covrd.all=mean(covrd.all, na.rm=TRUE)
               , total.fac=mean(total.fac, na.rm=TRUE)
               , covrd.fac=mean(covrd.fac, na.rm=TRUE)
               , total.sup=mean(total.sup, na.rm=TRUE)
               , covrd.sup=mean(covrd.sup, na.rm=TRUE)
               , cutoff=quantile(total.all, probs=0.8, type=8)
             )],
  episode.dt[,
             .(total.all=mean(total.all, na.rm=TRUE)
               , covrd.all=mean(covrd.all, na.rm=TRUE)
               , total.fac=mean(total.fac, na.rm=TRUE)
               , covrd.fac=mean(covrd.fac, na.rm=TRUE)
               , total.sup=mean(total.sup, na.rm=TRUE)
               , covrd.sup=mean(covrd.sup, na.rm=TRUE)
               , cutoff=quantile(total.all, probs=0.8, type=8)
             ), by=type]
))

quantiles.dt<--rbindlist(list(
    episode.dt[,
               .(type=NA  
                 , total.all=mean(total.all, na.rm=TRUE)
                 , covrd.all=mean(covrd.all, na.rm=TRUE)
                 , total.fac=mean(total.fac, na.rm=TRUE)
                 , covrd.fac=mean(covrd.fac, na.rm=TRUE)
                 , total.sup=mean(total.sup, na.rm=TRUE)
                 , covrd.sup=mean(covrd.sup, na.rm=TRUE)
               )],
    episode.dt[,
               .(total.all=mean(total.all, na.rm=TRUE)
                 , covrd.all=mean(covrd.all, na.rm=TRUE)
                 , total.fac=mean(total.fac, na.rm=TRUE)
                 , covrd.fac=mean(covrd.fac, na.rm=TRUE)
                 , total.sup=mean(total.sup, na.rm=TRUE)
                 , covrd.sup=mean(covrd.sup, na.rm=TRUE)
               ), by=type]
  ))

  #
# Distributions of ALL costs
#

# pcp distribution- ignore any episodes with missing npis
setkey(episode.dt,pcp_npi)
pcp.dt<-episode.dt[!is.na(pcp_npi),
                   .(num_episode=length(episode)
                     , avg_total=mean(total.all)
                     , avg_covrd=mean(covrd.all)
                     , tot_total=sum(total.all)
                     , tot_covrd=sum(covrd.all)
                     , rel_total=mean(total.all)/means.dt$total.all
                     , rel_covrd=mean(covrd.all)/means.dt$covrd.all
                   ), by=pcp_npi]

# attributed distribution- ignore any episodes with missing npis
setkey(episode.dt,doc_npi)
doc.dt<-episode.dt[!is.na(doc_npi),
                    list(num_episode=length(episode)
                         , avg_total=mean(total.all)
                         , avg_covrd=mean(covrd.all)
                         , tot_total=sum(total.all)
                         , tot_covrd=sum(covrd.all)
                         , rel_total=mean(total.all)/means.dt$total.all
                         , rel_covrd=mean(covrd.all)/means.dt$covrd.all
                    ), by=doc_npi]

# pcp vs attributed distribution - ignore any episodes with missing npis
setkey(episode.dt,pcp_npi,doc_npi)
pcp.doc.dt<-episode.dt[!is.na(pcp_npi) & !is.na(doc_npi),
                        list(num_episode=length(episode)
                             , avg_total=mean(total.all)
                             , avg_covrd=mean(covrd.all)
                             , tot_total=sum(total.all)
                             , tot_covrd=sum(covrd.all)
                             , rel_total=mean(total.all)/means.dt$total.all
                             , rel_covrd=mean(covrd.all)/means.dt$covrd.all
                        ), by="pcp_npi,doc_npi"]

#
# Per episode type distributions
#

setkey(episode.dt,type)
epi.cond.dt<-episode.dt[,
                        .(num_episode=length(episode)
                          , avg_total=mean(total.all)
                          , avg_covrd=mean(covrd.all)
                          , tot_total=sum(total.all)
                          , tot_covrd=sum(covrd.all)
                          , rel_total=mean(total.all)/means.dt$total.all
                          , rel_covrd=mean(covrd.all)/means.dt$covrd.all
                        ), by=type]
epi.cond.dt<-episode.dt[epi.cond.dt]

pcp.cond.dt<-setkey(epi.cond.dt[!is.na(pcp_npi),
                                .(num_episode=length(episode)
                                  , avg_total=mean(total.all)
                                  , avg_covrd=mean(covrd.all)
                                  , tot_total=sum(total.all)
                                  , tot_covrd=sum(covrd.all)
                                  , rel_total=mean(total.all)/avg_total
                                  , rel_covrd=mean(covrd.all)/avg_covrd
                                ), by="type,pcp_npi"],
                    type,pcp_npi)
doc.cond.dt<-setkey(epi.cond.dt[!is.na(doc_npi),
                                .(num_episode=length(episode)
                                  , avg_total=mean(total.all)
                                  , avg_covrd=mean(covrd.all)
                                  , tot_total=sum(total.all)
                                  , tot_covrd=sum(covrd.all)
                                  , rel_total=mean(total.all)/avg_total
                                  , rel_covrd=mean(covrd.all)/avg_covrd
                                ), by="type,doc_npi"],
                    type,doc_npi)

pcp.doc.cond.dt<-setkey(epi.cond.dt[!is.na(pcp_npi) & !is.na(doc_npi),
                                    .(num_episode=length(episode)
                                      , avg_total=mean(total.all)
                                      , avg_covrd=mean(covrd.all)
                                      , tot_total=sum(total.all)
                                      , tot_covrd=sum(covrd.all)
                                      , rel_total=mean(total.all)/avg_total
                                      , rel_covrd=mean(covrd.all)/avg_covrd
                                    ), by="type,pcp_npi,doc_npi"],
                        type,pcp_npi,doc_npi)


if (do.extra) {
#
# Distribution of Facility costs
#

# facility distribution
setkey(facility.dt,fac_npi)
fac.dt<-facility.dt[,
                    .(num_episode=length(episode)
                      , avg_total=mean(fac_total)
                      , avg_covrd=mean(fac_covrd)
                      , tot_total=sum(fac_total)
                      , tot_covrd=sum(fac_covrd)
                      , rel_total=mean(fac_total)/means.dt$total.fac
                      , rel_covrd=mean(fac_covrd)/means.dt$covrd.fac
                    ), by=fac_npi]

# pcp vs facility distribution - ignore any episodes with missing pcp npis
setkey(facility.dt,pcp_npi,fac_npi)
pcp.fac.dt<-facility.dt[!is.na(pcp_npi),
                        list(num_episode=length(episode)
                             , avg_total=mean(fac_total)
                             , avg_covrd=mean(fac_covrd)
                             , tot_total=sum(fac_total)
                             , tot_covrd=sum(fac_covrd)
                             , rel_total=mean(fac_total)/means.dt$total.fac
                             , rel_covrd=mean(fac_covrd)/means.dt$covrd.fac
                        ), by="pcp_npi,fac_npi"]

# attributed vs facility distribution - ignore any episodes with missing doc npis
setkey(facility.dt,doc_npi,fac_npi)
doc.fac.dt<-facility.dt[!is.na(doc_npi),
                       list(num_episode=length(episode)
                            , avg_total=mean(fac_total)
                            , avg_covrd=mean(fac_covrd)
                            , tot_total=sum(fac_total)
                            , tot_covrd=sum(fac_covrd)
                            , rel_total=mean(fac_total)/means.dt$total.fac
                            , rel_covrd=mean(fac_covrd)/means.dt$covrd.fac
                       ), by="doc_npi,fac_npi"]

#
#
#
fac.cond.dt<-setkey(epi.cond.dt[!is.na(fac_npi),
                                .(num_episode=length(episode)
                                     , avg_total=mean(total)
                                     , avg_covrd=mean(covrd)
                                     , rel_total=mean(total)/avg_total
                                     , rel_covrd=mean(covrd)/avg_covrd
                                ), by="type,fac_npi"],
                    type,fac_npi)

pcp.fac.cond.dt<-setkey(epi.cond.dt[!is.na(pcp_npi) & !is.na(doc_npi),
                                     list(num_episode=length(episode)
                                          , avg_total=mean(total)
                                          , avg_covrd=mean(covrd)
                                          , rel_total=mean(total)/avg_total
                                          , rel_covrd=mean(covrd)/avg_covrd
                                     ), by="type,pcp_npi,fac_npi"],
                         type,pcp_npi,fac_npi)
doc.fac.cond.dt<-setkey(epi.cond.dt[!is.na(pcp_npi) & !is.na(doc_npi),
                                    list(num_episode=length(episode)
                                         , avg_total=mean(total)
                                         , avg_covrd=mean(covrd)
                                         , rel_total=mean(total)/avg_total
                                         , rel_covrd=mean(covrd)/avg_covrd
                                    ), by="type,pcp_npi,fac_npi"],
                        type,pcp_npi,fac_npi)
}

episode.g <- ggplot(data = episode.dt) + theme_minimal()
episode.g + geom_density(aes(x=total.all)) + scale_x_log10() +
  #facet_wrap(~type) +
  labs(x="Episode Cost (log10)", y="Density")

                                                                                                                                                                                                
pcp.g <- ggplot(data=pcp.dt) + theme_minimal() + theme(axis.text.x=element_text(angle=-90))
pcp.g + geom_point(aes(x=reorder(pcp_npi, -rel_total), y=rel_total, colour=rel_total)) +
  scale_colour_gradient2(guide="colorbar", low="green", mid="grey50", high="red", midpoint = 1) +
  labs(x="PCP", y="Relative cost / Episode")

pcp.cond.g <- ggplot(data=pcp.cond.dt) + theme_minimal() + theme(axis.text.x=element_text(angle=-90))
pcp.cond.g + geom_point(aes(x=reorder(pcp_npi, -rel_total), y=rel_total, colour=rel_total)) +
  scale_colour_gradient2(guide="colorbar", midpoint = 0.75) + # low="green", mid="grey50", high="red", midpoint = 1) +
  labs(x="PCP", y="Relative cost / Episode") +
  facet_wrap(~type)


doc.g <- ggplot(data=doc.dt) + theme_minimal() + theme(axis.text.x=element_text(angle=-90))
doc.g + geom_bar(aes(x=reorder(doc_npi, -rel_total), y=rel_total), stat="identity") +
  labs(x="Attributed Physician", y="Avg. Episode $")

fac.g <- ggplot(data=fac.dt) + theme_minimal() + theme(axis.text.x=element_text(angle=-90))
fac.g + geom_bar(aes(x=reorder(fac_npi, -avg_total), y=avg_total), stat="identity") +
  labs(x="Facility", y="Avg. Episode $")

pcp.doc.g <- ggplot(data=pcp.doc.dt) + theme_minimal() + theme(axis.text.x=element_text(angle=-90))
pcp.doc.g + geom_tile(aes(y=doc_npi, x=pcp_npi, fill=rel_total)) +
  scale_fill_gradient2(low="green", mid="white", high="red", midpoint = 1) +
  labs(x=NULL, y=NULL)

pcp.doc.cond.g <- lapply(unique(episode.dt$type), function(x) {ggplot(data=pcp.doc.cond.dt[J(x)])})

pcp.doc.cond.g[[1]] +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=-90)) +
  geom_tile(aes(y=doc_npi, x=pcp_npi, fill=rel_total)) +
  scale_fill_gradient2(low="green", mid="white", high="red", midpoint = 1) +
  labs(x=NULL, y=NULL, title=type)



