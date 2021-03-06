library("sp")
library("raster")
library("lattice")

load("../../../DOC/TABLES/DATA_VISUALISATION/NON_EMPTY/district_distribution_aggregated.Rdata")

ind <- getData("GADM", country = "IND", level = 2)
ind.backup <- ind
## plot(ind)

ind$NAME_2 <- tolower(ind$NAME_2)
ind <- ind[order(ind$NAME_2), ]

district.d <- data.frame(districts = as.character(names(district.dat)),
                         accounts = as.numeric(district.dat)
                         )

ind <- merge(ind, district.d,
             by.x = "NAME_2",
             by.y = "districts",
             all.x = TRUE)
ind <- ind[order(ind$NAME_2), ]


#### check aurangabad 36-37

## nm <- district.d[-which(district.d$districts %in% ind$NAME_2), ]
## write.csv(nm, file = "district_mapping.csv")

ind$accounts[4] <- district.d$accounts[which(district.d$districts ==
                                           "ahmedabad")] 
ind$accounts[5] <- district.d$accounts[which(district.d$districts ==
                                           "ahmednagar")] 
ind$accounts[27] <- district.d$accounts[which(district.d$districts ==
                                            "angul")]
ind$accounts[46] <- district.d$accounts[which(district.d$districts ==
                                            "baleswar")]
ind$accounts[52] <- district.d$accounts[which(district.d$districts ==
                                            "banaskantha")]
ind$accounts[61] <- district.d$accounts[which(district.d$districts ==
                                            "baramula")]
ind$accounts[72] <- district.d$accounts[which(district.d$districts ==
                                            "bhatinda")]
ind$accounts[90] <- district.d$accounts[which(district.d$districts ==
                                            "beed")]
ind$accounts[125] <- district.d$accounts[which(district.d$districts ==
                                             "chikkaballapur")]
ind$accounts[126] <- district.d$accounts[which(district.d$districts ==
                                             "chikmangalur")]
ind$accounts[130] <- district.d$accounts[which(district.d$districts ==
                                             "chittorgarh")]
ind$accounts[137] <- district.d$accounts[which(district.d$districts ==
                                             "dadra & nagar haveli")]
ind$accounts[139] <- district.d$accounts[which(district.d$districts ==
                                             "dakshin kannada")]
ind$accounts[140] <- district.d$accounts[which(district.d$districts ==
                                             "dinajpur")]
ind$accounts[145] <- district.d$accounts[which(district.d$districts ==
                                             "darjeeling")]
ind$accounts[149] <- district.d$accounts[which(district.d$districts ==
                                             "davangere")]
ind$accounts[162] <- district.d$accounts[which(district.d$districts ==
                                             "dholpur")]
ind$accounts[193] <- district.d$accounts[which(district.d$districts ==
                                             "fathebad")]
ind$accounts[202] <- district.d$accounts[which(district.d$districts ==
                                             "gandhi nagar")]
ind$accounts[205] <- district.d$accounts[which(district.d$districts ==
                                             "gadchiroli")]
ind$accounts[213] <- district.d$accounts[which(district.d$districts ==
                                             "giridh")]
ind$accounts[220] <- district.d$accounts[which(district.d$districts ==
                                             "gondia")]
ind$accounts[238] <- district.d$accounts[which(district.d$districts ==
                                             "haridwar")]
ind$accounts[242] <- district.d$accounts[which(district.d$districts ==
                                             "hazaribag")]
ind$accounts[247] <- district.d$accounts[which(district.d$districts ==
                                             "hooghly")]
ind$accounts[250] <- district.d$accounts[which(district.d$districts ==
                                             "imphal")]
ind$accounts[251] <- district.d$accounts[which(district.d$districts ==
                                             "imphal")]
ind$accounts[254] <- district.d$accounts[which(district.d$districts ==
                                             "jagatsinghpur")]
ind$accounts[258] <- district.d$accounts[which(district.d$districts ==
                                             "jajpur")]
ind$accounts[278] <- district.d$accounts[which(district.d$districts ==
                                             "jhunjhunu")]
ind$accounts[290] <- district.d$accounts[which(district.d$districts ==
                                             "kanchipuram")]
ind$accounts[294] <- district.d$accounts[which(district.d$districts ==
                                             "kanyakumari")]
ind$accounts[307] <- district.d$accounts[which(district.d$districts ==
                                             "kasargod")]
ind$accounts[318] <- district.d$accounts[which(district.d$districts ==
                                             "khorda")]
ind$accounts[325] <- district.d$accounts[which(district.d$districts ==
                                             "cooch behar")]
ind$accounts[327] <- district.d$accounts[which(district.d$districts ==
                                             "koderma")]
ind$accounts[361] <- district.d$accounts[which(district.d$districts ==
                                             "ladakh")]
ind$accounts[384] <- district.d$accounts[which(district.d$districts ==
                                             "malda")]
ind$accounts[407] <- district.d$accounts[which(district.d$districts ==
                                             "mumbai")]
ind$accounts[408] <- district.d$accounts[which(district.d$districts ==
                                             "mumbai")]
ind$accounts[418] <- district.d$accounts[which(district.d$districts ==
                                             "nagapattinam")]
ind$accounts[438] <- district.d$accounts[which(district.d$districts ==
                                             "andaman & nicobar islands")]
ind$accounts[450] <- district.d$accounts[which(district.d$districts ==
                                             "palamau")]
ind$accounts[461] <- district.d$accounts[which(district.d$districts ==
                                             "west champaran")]
ind$accounts[462] <- district.d$accounts[which(district.d$districts ==
                                             "west singhbhum")]
ind$accounts[463] <- district.d$accounts[which(district.d$districts ==
                                             "west medinipur")]
#ind$accounts[474] <- district.d$accounts[which(district.d$districts ==
#                                             "ponch")]
ind$accounts[479] <- district.d$accounts[which(district.d$districts ==
                                             "pondicherry")]
ind$accounts[483] <- district.d$accounts[which(district.d$districts ==
                                             "east champaran")]
ind$accounts[484] <- district.d$accounts[which(district.d$districts ==
                                             "east medinipur")]
ind$accounts[485] <- district.d$accounts[which(district.d$districts ==
                                             "east singhbhum")]
ind$accounts[488] <- district.d$accounts[which(district.d$districts ==
                                             "purulia")]
ind$accounts[489] <- district.d$accounts[which(district.d$districts ==
                                             "raebareli")]
ind$accounts[500] <- district.d$accounts[which(district.d$districts ==
                                             "ramanagar")]
ind$accounts[506] <- district.d$accounts[which(district.d$districts ==
                                             "rangareddi")]
ind$accounts[518] <- district.d$accounts[which(district.d$districts ==
                                             "sabarkantha")]
ind$accounts[533] <- district.d$accounts[which(district.d$districts ==
                                             "sant ravidas nagar")]
ind$accounts[534] <- district.d$accounts[which(district.d$districts ==
                                             "seraikela-kharsawan")]
ind$accounts[556] <- district.d$accounts[which(district.d$districts ==
                                             "siddharthnagar")]
ind$accounts[554] <- district.d$accounts[which(district.d$districts ==
                                             "shrawasti")]
ind$accounts[573] <- district.d$accounts[which(district.d$districts ==
                                             "sonebhadra")]
ind$accounts[593] <- district.d$accounts[which(district.d$districts ==
                                             "surendra nagar")]
ind$accounts[604] <- district.d$accounts[which(district.d$districts ==
                                             "nilgiris")]
ind$accounts[605] <- district.d$accounts[which(district.d$districts ==
                                             "tiruvallur")]
ind$accounts[607] <- district.d$accounts[which(district.d$districts ==
                                             "tiruvarur")]
ind$accounts[608] <- district.d$accounts[which(district.d$districts ==
                                             "tuticorin")]
ind$accounts[634] <- district.d$accounts[which(district.d$districts ==
                                             "uttar kannada")]
ind$accounts[636] <- district.d$accounts[which(district.d$districts ==
                                             "north dinajpur")]
ind$accounts[644] <- district.d$accounts[which(district.d$districts ==
                                             "villupuram")]
ind$accounts[645] <- district.d$accounts[which(district.d$districts ==
                                             "virudhunagar")]
ind$accounts[647] <- district.d$accounts[which(district.d$districts ==
                                             "vishakapatnam")]
ind$accounts[663] <- district.d$accounts[which(district.d$districts ==
                                             "yamuna nagar")]
#ind$accounts[62] <- district.d$accounts[which(district.d$districts ==
#                                            "badaun")]
ind$accounts[508] <- district.d$accounts[which(district.d$districts ==
                                             "sriganganagar")]
ind$accounts[207] <- district.d$accounts[which(district.d$districts ==
                                             "pauri garhwal")]
ind$accounts[186] <- district.d$accounts[which(district.d$districts ==
                                             "periyar")]
ind$accounts[234] <- district.d$accounts[which(district.d$districts ==
                                             "howrah")]
ind$accounts[354] <- district.d$accounts[which(district.d$districts ==
                                             "kheri")]
ind$accounts[617] <- district.d$accounts[which(district.d$districts ==
                                             "north arcot ambedkar")]
ind$accounts[448] <- district.d$accounts[which(district.d$districts ==
                                             "raigad")]
ind$accounts[18] <- district.d$accounts[which(district.d$districts ==
                                            "medinipur")]
ind$accounts[394] <- district.d$accounts[which(district.d$districts ==
                                             "delhi")]
ind$accounts[450] <- district.d$accounts[which(district.d$districts ==
                                             "delhi")]
ind$accounts[492] <- district.d$accounts[which(district.d$districts ==
                                             "raigad")]

###############################################
ind <- ind[-which(ind$NAME_2 == "aurangabad" &
                  ind$NAME_1 == "Bihar"), ]
ind <- ind[!duplicated(ind$NAME_2), ]


ind$NAME_2 <- as.factor(ind$NAME_2)

ind$accounts[25] <- state.d$accounts[which(state.d$states == "Delhi")]
ind$accounts[24] <- 0

col.series.red <- c(229,204,153,102,65,35,0,255)
ind$col_series_red <- NA
ind$col_series_red[which(ind$accounts < 100)] <- col.series.red[1]
ind$col_series_red[which(ind$accounts >= 100 &
                     ind$accounts < 1000)] <- col.series.red[2]
ind$col_series_red[which(ind$accounts >= 1000 &
                     ind$accounts < 5000)] <- col.series.red[3]
ind$col_series_red[which(ind$accounts >= 5000 &
                     ind$accounts < 10000)] <- col.series.red[4]
ind$col_series_red[which(ind$accounts >= 10000 &
                     ind$accounts < 50000)] <- col.series.red[5]
ind$col_series_red[which(ind$accounts >= 50000 &
                     ind$accounts < 100000)] <- col.series.red[6]
ind$col_series_red[which(ind$accounts >= 100000)] <- col.series.red[7]


col.series.green <- c(245,236,216,194,174,139,88,255)
ind$col_series_green <- NA
ind$col_series_green[which(ind$accounts < 100)] <- col.series.green[1]
ind$col_series_green[which(ind$accounts >= 100 &
                     ind$accounts < 1000)] <- col.series.green[2]
ind$col_series_green[which(ind$accounts >= 1000 &
                     ind$accounts < 5000)] <- col.series.green[3]
ind$col_series_green[which(ind$accounts >= 5000 &
                     ind$accounts < 10000)] <- col.series.green[4]
ind$col_series_green[which(ind$accounts >= 10000 &
                     ind$accounts < 50000)] <- col.series.green[5]
ind$col_series_green[which(ind$accounts >= 50000 &
                     ind$accounts < 100000)] <- col.series.green[6]
ind$col_series_green[which(ind$accounts >= 100000)] <- col.series.green[7]

col.series.blue <- c(249,230,201,164,118,69,36,255)
ind$col_series_blue <- NA
ind$col_series_blue[which(ind$accounts < 100)] <- col.series.blue[1]
ind$col_series_blue[which(ind$accounts >= 100 &
                     ind$accounts < 1000)] <- col.series.blue[2]
ind$col_series_blue[which(ind$accounts >= 1000 &
                     ind$accounts < 5000)] <- col.series.blue[3]
ind$col_series_blue[which(ind$accounts >= 5000 &
                     ind$accounts < 10000)] <- col.series.blue[4]
ind$col_series_blue[which(ind$accounts >= 10000 &
                     ind$accounts < 50000)] <- col.series.blue[5]
ind$col_series_blue[which(ind$accounts >= 50000 &
                     ind$accounts < 100000)] <- col.series.blue[6]
ind$col_series_blue[which(ind$accounts >= 100000)] <- col.series.blue[7]



ind$col_series_red[which(is.na(ind$accounts))] <- col.series.red[8]
ind$col_series_green[which(is.na(ind$accounts))] <- col.series.green[8]
ind$col_series_blue[which(is.na(ind$accounts))] <- col.series.blue[8]

## ind <- ind[!duplicated(ind$NAME_2), ]

## ind <- ind[-35, ]
ind <- ind[order(ind[["NAME_2"]]), ]
## ind <- ind[-31, ]

pdf("/FRG/db_nsdl/DOC/GRAPHS/DATA_VISUALISATION/district_accounts_distribution.pdf")

legend.args <- list(
    fun = draw.colorkey,
    args = list(
        key = list(
            height = 0.75,
            at = seq(0.625, by = 0.75, length.out = 9),
            col = unique(rgb(col.series.red,
                             col.series.green,
                             col.series.blue,
                             maxColorValue = 255
                             )),
            labels = list(
                at = seq(0.5, by = 0.75, length.out = 9) + 0.5,
                labels = c("0 - 100",
                           "100 - 1000",
                           "1000 - 5000",
                           "5000 - 10000",
                           "10000 - 50000",
                           "50000 - 100000",
                           "> 100000",
                           "not tracked")
            )
        )
    ),
    corner = c(1.45, 0.5)
)
padding <- list(
    layout.widths = list(
        right.padding = 20
    )
)

color.palette <- rgb(ind$col_series_red,
                     ind$col_series_green,
                     ind$col_series_blue,
                     maxColorValue = 255
                     )

spplot(ind,
       "NAME_2",
       col.regions=color.palette,
       main = "",
       colorkey = FALSE,
       legend = list(inside = legend.args),
       par.settings = padding
      )

      

dev.off()




