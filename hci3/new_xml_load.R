require(XML)
require(data.table)

# parse definition file
hci <- xmlParse("/tmp/HCI3-ECR-Definition-Tables-2014-10-27-5.2.006_NON_LIC.xml")

# break out MDCs
mdc <- data.frame(
  episode_id=sapply(hci["//episode/episode_id"], xmlValue),
  acronym=sapply(hci["//episode/episode_acronym"], xmlValue),
  name=sapply(hci["//episode/episode_name"], xmlValue),
  type=sapply(hci["//episode/episode_type"], xmlValue),
  version=sapply(hci["//episode/episode_version"], xmlValue),
  modified_date=sapply(hci["//episode/episode_modified_date"], xmlValue),
  post_claim=sapply(hci["//episode/post_claim_assignment"], xmlValue),
  look_back=sapply(hci["//episode/look_back"], xmlValue),
  look_ahead=sapply(hci["//episode/look_ahead"], xmlValue),
  condition_min=sapply(hci["//episode/condition_min"], xmlValue)
)

if (length(mdc$episode_id)!=length(unique(mdc$episode_id))) print("Duplicate episode_id values in mdc!") else
  write.csv(mdc, file="mdc.csv", row.names=FALSE)

trigger_condition <- list()
trigger_code <- list()
dx_code <- list()
px_code <- list()
complication_code <- list()
association <- list()
rx_code <- list()
sub_type_code <- list()

for (e in mdc$episode_id)
{
  p <- paste("//episode[episode_id='", e ,"']", sep="")

  trigger_condition[[e]] <- xmlToDataFrame(nodes=hci[paste(p, "/trigger_condition", sep="")])
  trigger_code[[e]] <- xmlToDataFrame(nodes=hci[paste(p, "/trigger_code", sep="")])
  dx_code[[e]] <- xmlToDataFrame(nodes=hci[paste(p, "/dx_code", sep="")])
  px_code[[e]] <- xmlToDataFrame(nodes=hci[paste(p, "/px_code", sep="")])
  complication_code[[e]] <- xmlToDataFrame(nodes=hci[paste(p, "/complication_code", sep="")])
  association[[e]] <- xmlToDataFrame(nodes=hci[paste(p, "/association", sep="")])
  rx_code[[e]] <- xmlToDataFrame(nodes=hci[paste(p, "/rx_code", sep="")])
  sub_type_code[[e]] <- xmlToDataFrame(nodes=hci[paste(p, "/sub_type_code", sep="")])
}

