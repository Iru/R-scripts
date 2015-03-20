require(XML)
require(data.table)

# parse the schema
# xsd <- xmlSchemaParse("/tmp/builderExportScheme_2014-08-27.xsd")

# parse definition file
hci <- xmlParse("/tmp/HCI3-ECR-Definition-Tables-2014-10-27-5.2.006_NON_LIC.xml")

# break out MDCs
mdc <- unique(data.frame(
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
))


if (length(mdc$episode_id)!=length(unique(mdc$episode_id))) print("Duplicate episode_id values in mdc!") else
  write.csv(mdc, file="mdc.csv", row.names=FALSE)


# Generate unique list of procedures
#em <- setkey(unique(data.table(
em <- unique(data.frame(
  code_id=sapply(hci["//em_code/code_id"], xmlValue),
  type_id=sapply(hci["//em_code/type_id"], xmlValue),
  group_id=sapply(hci["//em_code/group_id"], xmlValue),
  code_name=sapply(hci["//em_code/code_name"], xmlValue),
  group_name=sapply(hci["//em_code/group_name"], xmlValue),
  specific_type_id=sapply(hci["//em_code/specific_type_id"], xmlValue)
  
)), code_id, type_id)

if (length(em[,c("code_id","type_id")]) == length(unique(em[,c("code_id","type_id")]))) {
  write.csv(em, file="em.csv", row.names=FALSE)
} else
  print("Duplicate code_id+type_id values in em!")

# Generate unique list of risk factors

risk <- setkey(unique(data.table(
  code_id=sapply(hci["//u_risk_factor/code_id"], xmlValue),
  type_id=sapply(hci["//u_risk_factor/type_id"], xmlValue),
  group_id=sapply(hci["//u_risk_factor/group_id"], xmlValue),
  code_name=sapply(hci["//u_risk_factor/code_name"], xmlValue),
  group_name=sapply(hci["//u_risk_factor/group_name"], xmlValue),
  specific_type_id=sapply(hci["//u_risk_factor/specific_type_id"], xmlValue),
  episode_id=sapply(hci["//u_risk_factor/episode_id"], xmlValue),
  episode_acronym=sapply(hci["//u_risk_factor/episode_acronym"], xmlValue)
)),code_id,type_id)

if (length(risk$code_id) == length(unique(risk$code_id))) {
  write.csv(risk, file="risk.csv", row.names=FALSE)
} else
  print("Duplicate code_id+type_id values in risk!") 

# Generate unique list of triggers
trigger <- setkey(unique(data.table(
  code_id=sapply(hci["//episode/trigger_code/code_id"], xmlValue),
  code_name=sapply(hci["//episode/trigger_code/code_name"], xmlValue),
  group_id=sapply(hci["//episode/trigger_code/group_id"], xmlValue),
  group_name=sapply(hci["//episode/trigger_code/group_name"], xmlValue),
  type_id=sapply(hci["//episode/trigger_code/type_id"], xmlValue),
  specific_type_id=sapply(hci["//episode/trigger_code/specific_type_id"], xmlValue)
)),code_id,type_id)

if (length(trigger[,list(code_id,type_id)]) == length(unique(trigger[,list(code_id,type_id)]))) {
  write.csv(trigger, file="trigger.csv", row.names=FALSE)
} else
  print("Duplicate code_id+type_id values in risk!") 

# Generate unique list of trigger conditions
trigger_condition <- unique(data.table(
  facility_type=sapply(hci["//episode/trigger_condition/facility_type"], xmlValue),
  px_code_position=sapply(hci["//episode/trigger_condition/px_code_position"], xmlValue),
  em_code_position=sapply(hci["//episode/trigger_condition/em_code_position"], xmlValue),
  and_or=sapply(hci["//episode/trigger_condition/and_or"], xmlValue),
  dx_code_position=sapply(hci["//episode/trigger_condition/dx_code_position"], xmlValue),
  episode_id=sapply(hci["//episode/trigger_condition/episode_id"], xmlValue),
  requires_confirming_code=sapply(hci["//episode/trigger_condition/requires_confirming_code"], xmlValue),
  min_code_separation=sapply(hci["//episode/trigger_condition/min_code_separation"], xmlValue),
  max_code_separation=sapply(hci["//episode/trigger_condition/max_code_separation"], xmlValue)
))

#write.csv(trigger_condition, file="trigger_condition.csv", row.names=FALSE)

# Generate unique list of diagnoses

dx <- setkey(unique(data.table(
  code_id=sapply(hci["//episode/dx_code/code_id"], xmlValue),
  type_id=sapply(hci["//episode/dx_code/type_id"], xmlValue),
  group_id=sapply(hci["//episode/dx_code/group_id"], xmlValue),
  code_name=sapply(hci["//episode/dx_code/code_name"], xmlValue),
  group_name=sapply(hci["//episode/dx_code/group_name"], xmlValue),
  specific_type_id=sapply(hci["//episode/dx_code/specific_type_id"], xmlValue)
)), code_id, type_id)

if (length(dx[,list(code_id,type_id)])!=length(unique(dx[,list(code_id,type_id)]))) {
  write.csv(dx, file="dx.csv", row.names=FALSE)
} else
  print("Duplicate code_id+type_id values in dx!") 

# Generate unique list of procedures
px <- setkey(unique(data.table(
  code_id=sapply(hci["//episode/px_code/code_id"], xmlValue),
  type_id=sapply(hci["//episode/px_code/type_id"], xmlValue),
  group_id=sapply(hci["//episode/px_code/group_id"], xmlValue),
  group_name=sapply(hci["//episode/px_code/group_name"], xmlValue),
  code_name=sapply(hci["//episode/px_code/code_name"], xmlValue),
  specific_type_id=sapply(hci["//episode/dx_code/specific_type_id"], xmlValue)
)), code_id, type_id)

if (length(px[,list(code_id,type_id)]) == length(unique(px[,list(code_id,type_id)]))) {
  write.csv(px, file="px.csv", row.names=FALSE)
} else
  print("Duplicate code_id+type_id values in px!")

# Generate unique list of complications
complication <- setkey(unique(data.table(
  code_id=sapply(hci["//episode/complication_code/code_id"], xmlValue),
  type_id=sapply(hci["//episode/complication_code/type_id"], xmlValue),
  group_id=sapply(hci["//episode/complication_code/group_id"], xmlValue),
  code_name=sapply(hci["//episode/complication_code/code_name"], xmlValue),
  group_name=sapply(hci["//episode/complication_code/group_name"], xmlValue),
  specific_type_id=sapply(hci["//episode/complication_code/specific_type_id"], xmlValue)
)), code_id, type_id)

if (length(complication[,list(code_id,type_id)]) == length(unique(complication[,list(code_id,type_id)]))) {
  write.csv(complication, file="complication.csv", row.names=FALSE)
} else
  print("Duplicate code_id+type_id values in complication!")
  
# Generate unique list of drugs
rx <- setkey(unique(data.table(
  code_id=sapply(hci["//episode/rx_code/code_id"], xmlValue),
  type_id=sapply(hci["//episode/rx_code/type_id"], xmlValue),
  group_id=sapply(hci["//episode/rx_code/group_id"], xmlValue),
  code_name=sapply(hci["//episode/rx_code/code_name"], xmlValue),
  group_name=sapply(hci["//episode/rx_code/group_name"], xmlValue),
  specific_type_id=sapply(hci["//episode/rx_code/specific_type_id"], xmlValue),
  rx_assignment=sapply(hci["//episode/rx_code/rx_assignment"], xmlValue)
)), code_id, type_id)

if (length(rx[,list(code_id,type_id)]) == length(unique(rx[,list(code_id,type_id)]))) {
  write.csv(rx, file="rx.csv", row.names=FALSE)
} else
  print("Duplicate code_id+type_id values in dx!")

# Generate unique list of sub types
sub_type <- setkey(unique(data.table(
  code_id=sapply(hci["//episode/sub_type_code/code_id"], xmlValue),
  type_id=sapply(hci["//episode/sub_type_code/type_id"], xmlValue),
  group_id=sapply(hci["//episode/sub_type_code/group_id"], xmlValue),
  code_name=sapply(hci["//episode/sub_type_code/code_name"], xmlValue),
  group_name=sapply(hci["//episode/sub_type_code/group_name"], xmlValue),
  specific_type_id=sapply(hci["//episode/sub_type_code/specific_type_id"], xmlValue),
  subtype_group_id=sapply(hci["//episode/sub_type_code/sub_type_group_id"], xmlValue),
  subtype_group_name=sapply(hci["//episode/sub_type_code/sub_type_group_name"], xmlValue)
)), code_id, type_id)

if (length(sub_type[,list(code_id,type_id)]) == length(unique(sub_type[,list(code_id,type_id)]))) {
  write.csv(sub_type, file="sub_type.csv", row.names=FALSE)
} else
  print("Duplicate code_id+type_id values in sub_type!")

# Generate unique list of associations
association <- unique(data.frame(
  episode_id=sapply(hci["//episode/association/episode_id"], xmlValue),
  episode_acronym=sapply(hci["//episode/association/episode_acronym"], xmlValue),
  subsidiary_to_procedural=sapply(hci["//episode/association/subsidiary_to_procedural"], xmlValue),
  ass_type=sapply(hci["//episode/association/ass_type"], xmlValue),
  ass_level=sapply(hci["//episode/association/ass_level"], xmlValue),
  ass_start_day=sapply(hci["//episode/association/ass_start_day"], xmlValue),
  ass_end_day=sapply(hci["//episode/association/ass_end_day"], xmlValue)
))
#write.csv(association, file="association.csv", row.names=FALSE)

# Generate episode_*_xwalk
dx_xwalk <- list()
px_xwalk <- list()
rx_xwalk <- list()
complication_xwalk <- list()
association_xwalk <- list()
sub_type_xwalk <- list()
trigger_xwalk <- list()

# for each MDC, pull dx data.  Use //episode[child::episode_acronym='STR'] to get the dx for a single mdc
for (e in mdc$episode_id)
{
  p <- paste("//episode[episode_id='", e ,"']", sep="")

  dx_xwalk[[e]] <- hci[paste(p, "/dx_code/code_id", sep="")]
  if (length(dx_xwalk[[e]]))
    dx_xwalk[[e]] <- data.frame(
      episode_id = I(e),
      dx_code=sapply(dx_xwalk[[e]], xmlValue))

  px_xwalk[[e]] <- hci[paste(p, "/px_code/code_id", sep="")]
  if (length(px_xwalk[[e]]))
    px_xwalk[[e]] <- data.frame(
      episode_id = I(e),
      px_code=sapply(px_xwalk[[e]], xmlValue),
      core=sapply(hci[paste(p, "/px_code/core", sep="")], xmlValue),
      sufficient=sapply(hci[paste(p, "/px_code/sufficient", sep="")], xmlValue),
      pas=sapply(hci[paste(p, "/px_code/pas", sep="")], xmlValue),
      betos_category=sapply(hci[paste(p, "/px_code/betos_category", sep="")], xmlValue))

  rx_xwalk[[e]] <- hci[paste(p, "/rx_code/code_id", sep="")]
  if (length(rx_xwalk[[e]]))
    rx_xwalk[[e]] <- data.frame(
      episode_id = I(e),
      rx_code=sapply(rx_xwalk[[e]], xmlValue))

  complication_xwalk[[e]] <- hci[paste(p, "/complication_code/code_id", sep="")]
  if (length(complication_xwalk[[e]]))
    complication_xwalk[[e]] <- data.frame(
      episode_id = I(e),
      complication_code=sapply(complication_xwalk[[e]], xmlValue),
      complication_type=sapply(hci[paste(p, "/complication_code/complication_type", sep="")], xmlValue))

  association_xwalk[[e]] <- hci[paste(p, "/association/episode_id", sep="")]
  if (length(association_xwalk[[e]]))
    association_xwalk[[e]] <- data.frame(
      episode_id = I(e),
      asscociated_id=sapply(association_xwalk[[e]], xmlValue))
  
  sub_type_xwalk[[e]] <- hci[paste(p, "/sub_type_code/code_id", sep="")]
  if (length(sub_type_xwalk[[e]]))
    sub_type_xwalk[[e]] <- data.frame(
      episode_id = I(e),
      sub_type_code=sapply(sub_type_xwalk[[e]], xmlValue))
  
  trigger_xwalk[[e]] <- hci[paste(p, "/trigger_code/code_id", sep="")]
  if (length(trigger_xwalk[[e]]))
    trigger_xwalk[[e]] <- data.frame(
      episode_id = I(e),
      code_id=sapply(trigger_xwalk[[e]], xmlValue),
      type_id=sapply(hci[paste(p, "/trigger_code/type_id", sep="")], xmlValue),
      qualifying_diagnosis=sapply(hci[paste(p, "/trigger_code/qualifying_diagnosis", sep="")], xmlValue))
}

write.csv(rbindlist(dx_xwalk), file="episode_dx_xwalk.csv.csv", row.names=FALSE)
write.csv(rbindlist(px_xwalk), file="episode_px_xwalk.csv.csv", row.names=FALSE)
write.csv(rbindlist(rx_xwalk), file="episode_rx_xwalk.csv.csv", row.names=FALSE)
write.csv(rbindlist(complication_xwalk), file="episode_complication_xwalk.csv", row.names=FALSE)
write.csv(rbindlist(association_xwalk), file="episode_association_xwalk.csv", row.names=FALSE)
write.csv(rbindlist(sub_type_xwalk), file="episode_sub_type_xwalk.csv", row.names=FALSE)
write.csv(rbindlist(trigger_xwalk), file="episode_trigger_xwalk.csv", row.names=FALSE)
