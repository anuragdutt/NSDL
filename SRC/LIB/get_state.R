##' The function inputs district names supplied by NSDL in their
##' master file and returns the corresponding state 
##'
##' The district state mapping is take from www.censusindia.gov.in
##' 
##' @title getState
##' @param dis vector of districts
##' @return state of every input district
##' @author Anurag  Dutt

getState <- function() {
    state.dt <-
        fread("/home/nsdl/CONSTANTS/list_of_district_of_india.csv")
    state.dt <- state.dt[, c("State",
                             "District",
                             "Headquarters"),
                         with = FALSE]
    dis.nsdl <- fread("/home/nsdl/CONSTANTS/nsdl_district_codes.csv")
    colnames(dis.nsdl) <- c("code", "district")
    colnames(state.dt) <- tolower(colnames(state.dt))
    dis.nsdl$district <- tolower(dis.nsdl$district)
    state.dt$district <- tolower(state.dt$district)

    state.data <- merge(dis.nsdl, state.dt,
                        by = "district",
                        all.x = TRUE)
    state.data <- state.data[!duplicated(state.data$district), ]
    
    state.data$headquarters <- NULL
    state.data$state[18] <- "Gujarat (GJ)"
    state.data$state[22] <- "Andaman & Nicobar (AN)"
    state.data$state[23] <- "Andaman & Nicobar (AN)"
    state.data$state[24] <- "Andhra Pradesh (AP)"
    state.data$state[26] <- "Uttar Pradesh (UP)"
    state.data$state[29] <- "Arunachal Pradesh (AR)"
    state.data$state[30] <- "Arunachal Pradesh (AR)"
    state.data$state[31] <- "Assam (AS)"
    state.data$state[34] <- "Uttar Pradesh (UP)"
    state.data$state[38] <- "Uttar Pradesh (UP)"
    state.data$state[42] <- "Odisha (OD)"
    state.data$state[48] <- "Karnataka (KA)"
    state.data$state[54] <- "Jammu and Kashmir (JK)"
    state.data$state[55] <- "West Bengal (WB)"
    state.data$state[58] <- "Odisha (OD)"
    state.data$state[76] <- "Punjab (PB)"
    state.data$state[84] <- "Bihar (BR)"
    state.data$state[92] <- "Odisha (OD)"
    state.data$state[96] <- "Maharashtra (MH)"
    state.data$state[103] <- "Karnataka (KA)"
    state.data$state[109] <- "Chhattisgarh (CG)"
    state.data$state[114] <- "Tamil Nadu (TN)"
    state.data$state[116] <- "Karnataka (KA)"
    state.data$state[128] <- "Dadra and Nagar Haveli (DN)"
    state.data$state[131] <- "Karnataka (KA)"
    state.data$state[139] <- "Karnataka (KA)"
    state.data$state[141] <- "Delhi (DL)"
    state.data$state[158] <- "Mizoram (MZ)"
    state.data$state[163] <- "Rajasthan (RJ)"
    state.data$state[169] <- "Andhra Pradesh (AP)"
    state.data$state[172] <- "West Bengal (WB)"
    state.data$state[173] <- "Madhya Pradesh (MP)"
    state.data$state[187] <- "Haryana (HR)"
    state.data$state[193] <- "Gujarat (GJ)"
    state.data$state[196] <- "Uttarakhand (UK)"
    state.data$state[197] <- "Uttar Pradesh (UP)"
    state.data$state[201] <- "Jharkhand (JH)"
    state.data$state[202] <- "Goa (GA)"
    state.data$state[210] <- "Gujarat (GJ)"
    state.data$state[223] <- "Haryana (HR)"
    state.data$state[225] <- "Uttar Pradesh (UP)"
    state.data$state[226] <- "Karnataka (KA)"
    state.data$state[228] <- "Himachal Pradesh (HP)"
    state.data$state[229] <- "Haryana (HR)"
    state.data$state[236] <- "Manipur (MN)"
    state.data$state[239] <- "Jammu and Kashmir (JK)"
    state.data$state[242] <- "Meghalaya (ML)"
    state.data$state[250] <- "Rajasthan (RJ)"
    state.data$state[256] <- "Chhattisgarh (CG)"
    state.data$state[258] <- "Uttar Pradesh (UP)"
    state.data$state[270] <- "Gujarat (GJ)"
    state.data$state[278] <- "Uttar Pradesh (UP)"
    state.data$state[279] <- "Uttar Pradesh (UP)"
    state.data$state[289] <- "Karnataka (KA)"
    state.data$state[291] <- "Kerala (KL)"
    state.data$state[296] <- "Odisha (OD)"
    state.data$state[297] <- "Kerala (KL)"
    state.data$state[300] <- "Madhya Pradesh (MP)"
    state.data$state[302] <- "Odisha (OD)"
    state.data$state[303] <- "Odisha (OD)"
    state.data$state[304] <- "Odisha (OD)"
    state.data$state[327] <- "Jammu and Kashmir (JK)"
    state.data$state[328] <- "Himachal Pradesh (HP)"
    state.data$state[344] <- "Madhya Pradesh (MP)"
    state.data$state[346] <- "Telangana (TS)"
    state.data$state[348] <- "Maharashtra (MH)"
    state.data$state[351] <- "Gujarat (GJ)"
    state.data$state[355] <- "West Bengal (WB)"
    state.data$state[360] <- "Karnataka (KA)"
    state.data$state[363] <- "Tamil Nadu (T)"
    state.data$state[366] <- "West Bengal (WB)"
    state.data$state[368] <- "Meghalaya (ML)"
    state.data$state[370] <- "Mizoram (MZ)"
    state.data$state[372] <- "Punjab (PB)"
    state.data$state[378] <- "Punjab (PB)"
    state.data$state[379] <- "Maharashtra (MH)"
    state.data$state[385] <- "Odisha (OD)"
    state.data$state[387] <- "West Bengal (WB)"
    state.data$state[398] <- "Madhya Pradesh (MP)"
    state.data$state[400] <- "Maharashtra (MH)"
    state.data$state[403] <- "Punjab (PB)"
    state.data$state[406] <- "Andhra Pradesh (AP)"
    state.data$state[412] <- "Uttar Pradesh (UP)"
    state.data$state[413] <- "Assam (AS)"
    state.data$state[414] <- "West Bengal (WB)"
    state.data$state[418] <- "Odisha (OD)"
    state.data$state[421] <- "Jharkhand (JH)"
    state.data$state[423] <- "Gujarat (GJ)"
    state.data$state[427] <- "Maharashtra (MH)"
    state.data$state[436] <- "Kerala (KL)"
    state.data$state[438] <- "Odisha (OD)"
    state.data$state[441] <- "Jammu and Kashmir (JK)"
    state.data$state[448] <- "Punjab (PB)"
    state.data$state[452] <- "West Bengal (WB)"
    state.data$state[459] <- "Rajasthan (RJ)"
    state.data$state[460] <- "Rajasthan (RJ)"
    state.data$state[465] <- "Karnataka (KA)"
    state.data$state[470] <- "Telangana (TS)"
    state.data$state[499] <- "Jharkhand (JH)"
    state.data$state[507] <- "Uttar Pradesh (UP)"
    state.data$state[509] <- "Uttar Pradesh (UP)"
    state.data$state[510] <- "Uttar Pradesh (UP)"
    state.data$state[515] <- "Jharkhand (JH)"
    state.data$state[526] <- "Uttar Pradesh (UP)"
    state.data$state[532] <- "West Bengal (WB)"
    state.data$state[536] <- "Rajasthan (RJ)"
    state.data$state[542] <- "Gujarat (GJ)"
    state.data$state[545] <- "Tamil Nadu (TN)"
    state.data$state[553] <- "Tamil Nadu (TN)"
    state.data$state[554] <- "Tamil Nadu (TN)"
    state.data$state[567] <- "Tamil Nadu (TN)"
    state.data$state[568] <- "Tripura (TR)"
    state.data$state[571] <- "Tamil Nadu (TN)"
    state.data$state[582] <- "Karnataka (KA)"
    state.data$state[583] <- "Uttar Pradesh (UP)"
    state.data$state[585] <- "Uttarakhand (UK)"
    state.data$state[592] <- "Tamil Nadu (TN)"
    state.data$state[595] <- "Andhra Pradesh (AP)"
    state.data$state[599] <- "West Bengal (WB)" 
    state.data$state[601] <- "West Bengal (WB)"
    state.data$state[606] <- "West Bengal (WB)"
    state.data$state[607] <- "Madhya Pradesh (MP)"

    state.data[which(is.na(state.data$state))[1:7], ] <-
        "Maharashtra (MH)"
    state.data[tail(which(is.na(state.data$state)), 5), ] <-
        "Gujarat (GJ)"
    colnames(state.data) <- c("district", "district_code", "state")
    state.data <- state.data[!is.na(state.data$district_code), ]
    save(state.data, file = "/home/nsdl/CONSTANTS/state_district_mapping.Rdata")

}

## getState()
