library("sp")
library("raster")
library("lattice")

load("../../../DOC/TABLES/DATA_VISUALISATION/NON_EMPTY/state_distribution_aggregated.Rdata")

ind <- getData("GADM", country = "IND", level = 1)
ind.backup <- ind
#plot(ind)

state.names <- NULL
for(i in names(state.dat)) {
    state.names <- c(state.names, substr(i, 1, nchar(i) - 5))
}

names(state.dat) <- state.names
state.dat[1] <- state.dat[1] + state.dat[2]
state.dat <- state.dat[-2]

state.d <- data.frame(states = as.character(names(state.dat)),
                      accounts = as.numeric(state.dat)
                      )

ind$NAME_1 <- as.factor(ind$NAME_1)
ind <- merge(ind, state.d,
             by.x = "NAME_1",
             by.y = "states",
             all.x = TRUE)
ind$accounts[25] <- state.d$accounts[which(state.d$states == "Delhi")]
ind$accounts[24] <- 0

col.series.red <- c(237, 178, 102, 44 ,0)
ind$col_series_red <- NA
ind$col_series_red[which(ind$accounts < 1000)] <- col.series.red[1]
ind$col_series_red[which(ind$accounts >= 1000 &
                     ind$accounts < 20000)] <- col.series.red[2]
ind$col_series_red[which(ind$accounts >= 20000 &
                     ind$accounts < 100000)] <- col.series.red[3]
ind$col_series_red[which(ind$accounts >= 100000 &
                     ind$accounts < 300000)] <- col.series.red[4]
ind$col_series_red[which(ind$accounts >= 300000)] <- col.series.red[5]

col.series.green <- c(248, 226, 194, 162, 109)
ind$col_series_green <- NA
ind$col_series_green[which(ind$accounts < 1000)] <- col.series.green[1]
ind$col_series_green[which(ind$accounts >= 1000 &
                     ind$accounts < 20000)] <- col.series.green[2]
ind$col_series_green[which(ind$accounts >= 20000 &
                     ind$accounts < 100000)] <- col.series.green[3]
ind$col_series_green[which(ind$accounts >= 100000 &
                     ind$accounts < 300000)] <- col.series.green[4]
ind$col_series_green[which(ind$accounts >= 300000)] <- col.series.green[5]


col.series.blue <- c(251, 226, 164, 95, 44)
ind$col_series_blue <- NA
ind$col_series_blue[which(ind$accounts < 1000)] <- col.series.blue[1]
ind$col_series_blue[which(ind$accounts >= 1000 &
                     ind$accounts < 20000)] <- col.series.blue[2]
ind$col_series_blue[which(ind$accounts >= 20000 &
                     ind$accounts < 100000)] <- col.series.blue[3]
ind$col_series_blue[which(ind$accounts >= 100000 &
                     ind$accounts < 300000)] <- col.series.blue[4]
ind$col_series_blue[which(ind$accounts >= 300000)] <- col.series.blue[5]


## ind <- ind[-35, ]
ind <- ind[order(ind[["NAME_1"]]), ]
ind <- ind[-31, ]

pdf("/FRG/db_nsdl/DOC/GRAPHS/DATA_VISUALISATION/state_accounts_distribution.pdf")

legend.args <- list(
    fun = draw.colorkey,
    args = list(
        key = list(
            height = 0.75,
            at = seq(0.625, by = 0.75, length.out = 6),
            col = unique(rgb(col.series.red,
                             col.series.green,
                             col.series.blue,
                             maxColorValue = 255
                             )),
            labels = list(
                at = seq(0.5, by = 0.75, length.out = 6) + 0.5,
                labels = c("0 - 1000",
                           "1000 - 20000",
                           "20000 - 100000",
                           "100000 - 300000",
                           "> 300000")
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
spplot(ind,
       "NAME_1",
       col.regions=rgb(ind$col_series_red,
                       ind$col_series_green,
                       ind$col_series_blue,
                       maxColorValue = 255),
       main = "",
       colorkey = FALSE,
       legend = list(inside = legend.args),
       par.settings = padding
      )
      
      

dev.off()




