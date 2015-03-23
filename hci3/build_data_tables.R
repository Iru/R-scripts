require(RPostgreSQL)
require(data.table)

if (run.once) {
  # connect to database
  con <-
    dbConnect(
      dbDriver("PostgreSQL")
      , dbname="analytics"
    )
  
  # 
  # Pull the code table from the database. Convert to data.table form for ease of 
  # manipulation. Convert strings to factors
  # 
  code.table.dt <-
    data.table(
      dbReadTable(
        con
        ,c("hci3_staging", "code_type")
      )
      , key = "type_id")
  dx.code.dt <-
    setkey(
      code.table.dt[type_id=="DX",
                    .(code_id = factor(code_id)
                      , group_id = factor(group_id)
                      , code_name = factor(code_name)
                      , group_name = factor(group_name)
                    )]
      , code_id)
  rx.code.dt <-
    setkey(
      code.table.dt[type_id == "RX",
                    .(code_id = factor(code_id)
                      , group_id = factor(group_id)
                      , code_name = factor(code_name)
                      , group_name = factor(group_name)
                    )]
      , code_id)
  px.code.dt <-
    setkey(
      code.table.dt[!(type_id == "DX" | type_id == "RX"),
                    .(code_id = factor(code_id)
                      , type_id = factor(type_id)
                      , group_id = factor(group_id)
                      , code_name = factor(code_name)
                      , group_name = factor(group_name)
                    )]
      , code_id)
  px.code.dt[,.(group_name=max(group_name), .N),keyby=.(group_id)]

  # 
  # Pull the complication data from the database. Convert to data.table form for ease of 
  # manipulation. Convert strings to factors
  # 
  complication.code.dt <-
    data.table(
      dbReadTable(
        con
        ,c("hci3_staging", "episode_complication_code")
      )
      , key = "type_id")
  
  # 
  # Pull claim data from database. Convert to data.table form for ease of 
  # manipulation. Convert strings to factors and shorten some names while we're at
  # it
  # 
  claim.dt <-
    data.table(
      dbReadTable(con
                  , c("hci3_staging", "episode_claim")
                  )
      , key="episode")
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
  
  run.once<-FALSE
}


# First build facility & support only tables
facility.dt <-
  unique(
    claim.dt[!is.na(fac_npi),
             .(type
               , doc_npi
               , pcp_npi
               , total.fac=sum(tot_amt)
             ),
             keyby=.(fac_npi, episode)],
  )
facility.rev.dt <-
  setkey(
    facility.dt[,.(episode, fac_npi)]
    , episode
    , fac_npi
  )

support.dt <-
  unique(
    claim.dt[!is.na(sup_npi),
             .(type
               , doc_npi
               , pcp_npi
               , total.sup=sum(tot_amt)
             ),
             keyby=.(sup_npi, episode)],
  )
support.rev.dt <-
  setkey(
    support.dt[,.(episode, sup_npi)]
    , episode
    , sup_npi
  )


# create episode level view - will need more of these that show dx/px groups later
episode.dt <-
  unique(
    claim.dt[,
             .(type
               , doc_npi
               , pcp_npi
               , total.all=sum(tot_amt)
             ),
             keyby=.(episode)]
    )

# add the per episode facility total
episode.dt<-facility.dt[, .(total.fac=sum(total.fac)), keyby=.(episode)][episode.dt]

# add the per episode support total
episode.dt<-support.dt[, .(total.sup=sum(total.sup)), keyby=.(episode)][episode.dt]

# Add the univeral and per condition means to each episode.  Also add the current cut-off value (80th percentile)
episode.dt[,c("mean.all"
              , "mean.fac"
              , "mean.sup"
              , "cutoff.all") :=
             list(mean(total.all, na.rm=TRUE)
                  , mean(total.fac, na.rm=TRUE)
                  , mean(total.sup, na.rm=TRUE)
                  , quantile(total.all, probs=0.8, type=8)
             )]
episode.dt[,c("type.mean.all"
              , "type.mean.fac"
              , "type.mean.sup"
              , "type.cutoff.all") :=
             list(mean(total.all, na.rm=TRUE)
                  , mean(total.fac, na.rm=TRUE)
                  , mean(total.sup, na.rm=TRUE)
                  , quantile(total.all, probs=0.8, type=8)
             ),
           by=.(type)]
print("built episode.dt")

#
# Create list of target entries based on claims above cutoff
#
pcp.list.dt <-
  setkey(
    unique(
      episode.dt[!is.na(pcp_npi) & total.all>=cutoff.all,
                 .(pcp_npi)]
    ),
    pcp_npi)

doc.list.dt <-
  setkey(
    unique(
      episode.dt[!is.na(doc_npi) & total.all>=cutoff.all,
                 .(doc_npi)]
    ),
    doc_npi)

pcp.doc.list.dt <-
  setkey(
    unique(
      episode.dt[!is.na(pcp_npi) & !is.na(doc_npi) & total.all>=cutoff.all,
                 .(pcp_npi, doc_npi)]
    ),
    pcp_npi, doc_npi)

print("built episode based *.list.dts")

fac.list.dt <-
 setkey(
   unique(
     episode.dt[total.all>=cutoff.all,
                .(episode)][facility.rev.dt,.(fac_npi)]
   ),
   fac_npi)

fac.pcp.list.dt <-
 setkey(
    unique(
      facility.dt[fac.list.dt & !is.na(pcp_npi),
                 .(fac_npi, pcp_npi)]
    ),
    fac_npi, pcp_npi)

fac.doc.list.dt <-
  setkey(
    unique(
      episode.dt[!is.na(fac_npi) & !is.na(doc_npi) & total.all>=cutoff.all,
                 .(fac_npi, doc_npi)]
    ),
    fac_npi, doc_npi)

sup.list.dt <-
  setkey(
    unique(
      episode.dt[!is.na(sup_npi) & total.all>=cutoff.all,
                 .(sup_npi)]
    ),
    sup_npi)

sup.pcp.list.dt <-
  setkey(
    unique(
      episode.dt[!is.na(sup_npi) & !is.na(pcp_npi) & total.all>=cutoff.all,
                 .(sup_npi, pcp_npi)]
    ),
    sup_npi, pcp_npi)

sup.doc.list.dt <-
  setkey(
    unique(
      episode.dt[!is.na(sup_npi) & !is.na(doc_npi) & total.all>=cutoff.all,
                 .(sup_npi, doc_npi)]
    ),
    sup_npi, doc_npi)


print("built *.list.dt")

#
# PCP only distribution, both overall and per episode type
#
setkey(episode.dt, pcp_npi)
pcp.dt <-
  unique(
    episode.dt[
      pcp.list.dt,
      .(episodes=.N
        , mean.all=mean(total.all)
        , total.all=sum(total.all)
        , relative.all=mean(total.all)/mean.all
        , mean.fac=mean(total.fac)
        , total.fac=sum(total.fac)
        , relative.fac=mean(total.fac)/mean.fac
        , mean.sup=mean(total.sup)
        , total.sup=sum(total.sup)
        , relative.sup=mean(total.sup)/mean.sup
      ),
      keyby=.(pcp_npi)]
  )

pcp.type.dt <-
  unique(
    episode.dt[
      pcp.list.dt,
      .(episodes=.N
        , mean.all=mean(total.all)
        , total.all=sum(total.all)
        , relative.all=mean(total.all)/type.mean.all
        , mean.fac=mean(total.fac)
        , total.fac=sum(total.fac)
        , relative.fac=mean(total.fac)/type.mean.fac
        , mean.sup=mean(total.sup)
        , total.sup=sum(total.sup)
        , relative.sup=mean(total.sup)/type.mean.sup
      ),
      keyby=.(pcp_npi, type)]
  )
pcp.type.dt[pcp.dt, pcp.relative.all := i.relative.all]

print("built PCP distributions")


#
# Attributed physician only distribution, both overall and per episode type
#
setkey(episode.dt, doc_npi)
doc.dt <-
  unique(
    episode.dt[
      doc.list.dt,
      .(episodes=.N
        , mean.all=mean(total.all)
        , total.all=sum(total.all)
        , relative.all=mean(total.all)/mean.all
        , mean.fac=mean(total.fac)
        , total.fac=sum(total.fac)
        , relative.fac=mean(total.fac)/mean.fac
        , mean.sup=mean(total.sup)
        , total.sup=sum(total.sup)
        , relative.sup=mean(total.sup)/mean.sup
      ), 
      keyby=.(doc_npi)]
  )

doc.type.dt <-
  unique(
    episode.dt[
      doc.list.dt,
      .(episodes=.N
        , mean.all=mean(total.all)
        , total.all=sum(total.all)
        , relative.all=mean(total.all)/type.mean.all
        , mean.fac=mean(total.fac)
        , total.fac=sum(total.fac)
        , relative.fac=mean(total.fac)/type.mean.fac
        , mean.sup=mean(total.sup)
        , total.sup=sum(total.sup)
        , relative.sup=mean(total.sup)/type.mean.sup
      ), 
      keyby=.(doc_npi, type)]
  )
doc.type.dt[doc.dt, doc.relative.all := i.relative.all]

print("built Attributed Physician distributions")

#
# PCP vs Attributed physician distribution, both overall and per episode type
#
setkey(episode.dt, pcp_npi, doc_npi)
pcp.doc.dt <-
  unique(
    episode.dt[
      pcp.doc.list.dt,
      .(episodes=.N
        , mean.all=mean(total.all)
        , total.all=sum(total.all)
        , relative.all=mean(total.all)/mean.all
        , mean.fac=mean(total.fac)
        , total.fac=sum(total.fac)
        , relative.fac=mean(total.fac)/mean.fac
        , mean.sup=mean(total.sup)
        , total.sup=sum(total.sup)
        , relative.sup=mean(total.sup)/mean.sup
      ),
      keyby=.(pcp_npi, doc_npi)]
  )

pcp.doc.type.dt <-
  unique(
    episode.dt[
      pcp.doc.list.dt,
      .(episodes=.N
        , mean.all=mean(total.all)
        , total.all=sum(total.all)
        , relative.all=mean(total.all)/type.mean.all
        , mean.fac=mean(total.fac)
        , total.fac=sum(total.fac)
        , relative.fac=mean(total.fac)/type.mean.fac
        , mean.sup=mean(total.sup)
        , total.sup=sum(total.sup)
        , relative.sup=mean(total.sup)/type.mean.sup
      ),
      keyby=.(pcp_npi, doc_npi, type)]
  )
pcp.doc.type.dt[pcp.doc.dt, pcp.doc.relative.all := i.relative.all]

print("built PCP vs Attributed Physician distributions")


if (do.extra) {
#
# Distribution of Facility costs
#

# facility distribution
setkey(facility.dt,fac_npi)
fac.dt <-
  facility.dt[fac.list.dt,
              .(episodes=.N
                , mean.fac=mean(total.fac)
                , total.fac=sum(total.fac)
                , relative.fac=mean(total.fac)/mean.fac
              ), 
              keyby=.(fac_npi)]

fac.type.dt <-
  facility.dt[fac.list.dt,
              .(episodes=.N
                , mean.fac=mean(total.fac)
                , total.fac=sum(total.fac)
                , relative.fac=mean(total.fac)/mean.fac
              ), 
              keyby=.(fac_npi, type)]

# pcp vs facility distribution - ignore any episodes with missing pcp npis
pcp.fac.dt<-facility.dt[fac.pcp.list,
                        .(episodes=.N
                          , mean.fac=mean(total.fac)
                          , total.fac=sum(total.fac)
                          , relative.fac=mean(total.fac)/mean.fac
                        ), 
                        keyby=.(pcp_npi, fac_npi)]

pcp.fac.type.dt<-facility.dt[fac.pcp.list,
                        .(episodes=.N
                          , mean.fac=mean(total.fac)
                          , total.fac=sum(total.fac)
                          , relative.fac=mean(total.fac)/mean.fac
                        ), 
                        keyby=.(pcp_npi, fac_npi, type)]

# attributed vs facility distribution - ignore any episodes with missing doc npis
doc.fac.dt<-facility.dt[fac.doc.list.dt,
                        .(episodes=.N
                          , mean.fac=mean(total.fac)
                          , total.fac=sum(total.fac)
                          , relative.fac=mean(total.fac)/mean.fac
                        ),
                        keyby=.(doc_npi, fac_npi)]

doc.fac.type.dt<-facility.dt[fac.doc.list.dt,
                        .(episodes=.N
                          , mean.fac=mean(total.fac)
                          , total.fac=sum(total.fac)
                          , relative.fac=mean(total.fac)/mean.fac
                        ),
                        keyby=.(doc_npi, fac_npi, type)]
}

