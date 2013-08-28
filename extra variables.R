#combine data from multiple datasets within the NLA based on the lake SITE_ID

setwd("~/Dropbox/nla-data/Lake Physical Habitat Condition Estimates")
physical = read.csv("Lake_phab_condtion_estimates_20091130.csv", header = TRUE)
physical = physical[ , c(1, 2, 43)]
physical = unique(physical) # get rid of duplicate data
physical = physical[physical$VISIT_NO == 1, ] #get rid of multiple visits

setwd("~/Dropbox/nla-data/Lake Physical Habitat Index Values")
physical_hab = read.csv("NLA_phabsupermetricindices.csv", header = TRUE)
physical_hab = physical_hab[ , c(1,3, 27:29)]
physical_hab = unique(physical_hab)
physical_hab = physical_hab[physical_hab$VISIT_NO == 1, ]

setwd("~/Dropbox/nla-data/Lake Visual Assessment Data")
visual = read.csv("nla_visassess_valid_20091015.csv", header = TRUE)
visual = visual[ , c(1, 2, 13:14, 62:69)]
visual = unique(visual)
visual = visual[visual$VISIT_NO == 1, ]

setwd("~/Dropbox/nla-data/Lake Physical Habitat Metrics")
hab_met = read.csv("NLA_phabmet_B.csv", header = TRUE)
hab_met = hab_met[ , c(1, 3, 58, 78)]
hab_met = unique(hab_met)
hab_met = hab_met[hab_met$VISIT_NO == 1, ]


setwd("~/Dropbox/nla-data/Lake Basin Landuse Metrics")
standard = read.csv("Lake_Basin_Landuse_Metrics_20061022.csv", header = TRUE)
standard = as.data.frame(standard[ , 1])

data = merge(standard, hab_met, by.y = 'SITE_ID')
data = merge(data, physical, by = 'SITE_ID')
data = merge(data, physical_hab, by = 'SITE_ID')
data = merge(data, visual, by = 'SITE_ID')
data = data[ , c(1, 3:4, 6, 8:10, 12:21)]

write.csv(data, "Extra_NLA_variables.csv")