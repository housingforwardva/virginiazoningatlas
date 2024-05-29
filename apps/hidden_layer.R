hidden <- mapedit::drawFeatures(
  
)

cols <- c("tooltipnotes", "plannedresidential_treatment", "accessory_treatment", 
          "jurisdiction", "family4_treatment", "family2_treatment", "abbrvname", 
          "accessory_family_required", "family1_treatment", "overlay", 
          "family3_treatment", "id", "name", "county", "type", "accessory_owner_required", 
          "accessory_elderly_only", "accessory_renter_prohibited", "acres", 
          "sfd", "region", "Zoning", "total_area", "Abbreviation", "Jurisdiction", 
          "Notes", "fill_color", "highlight_color")

for (i in cols) {
  hidden[[i]] <- zoning[1,][[i]]
}

hidden[["total_area"]] <- 1
hidden[["acres"]] <- 0

readr::write_rds(hidden, "apps/data/hidden_layer.rds")

