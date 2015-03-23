
#
# Libraries... just in case
#
require(reshape2)
require(data.table)
require(ggplot2)

#
# Reorder NPIs because ggplot is stupid
#
pcp.dt$pcp_npi <-
  reorder(pcp.dt$pcp_npi
          , -pcp.dt$relative.all
          , order=TRUE)
pcp.type.dt$pcp_npi <-
  reorder(pcp.type.dt$pcp_npi
          , -pcp.type.dt$pcp.relative.all
          , order=TRUE)

doc.dt$doc_npi <-
  reorder(doc.dt$doc_npi
          , -doc.dt$relative.all
          , order=TRUE)
doc.type.dt$doc_npi <-
  reorder(doc.type.dt$doc_npi
          , -doc.type.dt$doc.relative.all
          , order=TRUE)

print("reordered factors")

#
#  Here be graphs!!!!
#

# Overall cost distribution
episode.g <-
  ggplot(episode.dt, aes(x=total.all)) +
  theme_minimal() +
  labs(x="Episode Cost (log10)", y="Density") +
  geom_density() +
  scale_x_log10() +
  geom_vline(aes(xintercept=cutoff.all))

episode.g + 
  geom_vline(aes(xintercept=cutoff.all)) + 
  facet_wrap(~type) + geom_vline(aes(xintercept=type.cutoff.all), linetype=2)

#
# PCP cost performance: overall and by episode type
#
pcp.g <-
  ggplot(
    melt(pcp.dt
         , c("pcp_npi")
         , c("relative.all", "relative.fac", "relative.sup")
         , na.rm = TRUE),
    aes(x=pcp_npi
        , y=value
        , group=variable
        , color=variable)) +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=-90)) +
  labs(x="PCP", y="Relative cost / Episode") +
  scale_colour_discrete() +
  geom_point()

pcp.type.g <-
  ggplot(melt(pcp.type.dt
              , c("pcp_npi", "type")
              , c("relative.all", "relative.fac", "relative.sup")
              , na.rm = TRUE),
         aes(x=pcp_npi
             , y=value
             , group=variable
             , color=variable)) +
  theme_minimal() +
  theme(axis.text.x=element_blank()) +
  labs(x="PCP", y="Relative cost / Episode") +
  geom_point() +
  scale_colour_discrete() +
  facet_wrap(~type)

#
# PCP cost performance: overall and by episode type
#
doc.g <-
  ggplot(
    melt(doc.dt
         , c("doc_npi")
         , c("relative.all", "relative.fac", "relative.sup")
         , na.rm = TRUE),
    aes(x=doc_npi
        , y=value
        , group=variable
        , color=variable)) +
  theme(axis.text.x=element_text(angle=-90)) +
  labs(x="doc", y="Relative cost / Episode") +
  scale_colour_discrete() +
  geom_point()

doc.g + theme_minimal()

#
# PCP cost performance: by episode type
#
doc.type.g <-
  ggplot(melt(doc.type.dt
              , c("doc_npi", "type")
              , c("relative.all", "relative.fac", "relative.sup", "doc.relative.all")
              , na.rm = TRUE),
         aes(x=doc_npi
             , y=value
             , group=variable
             , color=variable)) +
  theme(axis.text.x=element_blank()) +
  labs(x="doc", y="Relative cost / Episode") +
  geom_point() +
  scale_colour_discrete() +
  facet_wrap(~type)

doc.type.g + theme_minimal()


pcp.doc.g <-
  ggplot(data=pcp.doc.dt,
         aes(y=reorder(doc_npi, -relative.all)
             , x=reorder(pcp_npi, -relative.all)
             , fill=relative.all)) +
  theme(axis.text.x=element_text(angle=-90)) +
  geom_tile() +
  scale_fill_gradient2(low="green", mid="white", high="red", midpoint = 1) +
  labs(x=NULL, y=NULL)

pcp.doc.g + theme_minimal() + theme(axis.text.x=element_text(angle=-90))


