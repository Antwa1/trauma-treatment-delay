library(Gmisc, quietly = TRUE)
library(glue)
library(htmlTable)
library(grid)
library(magrittr)
library(gtsummary)

treated <- boxGrob(glue("Treated at KS between 2012 and 2022",
                           "n = {pop}",
                           pop = txtInt(11864),
                           .sep = "\n"))
screened <- boxGrob(glue("Screened for ofi",
                         "n = {pop}",
                         pop = txtInt(6582),
                         .sep = "\n"))
included <- boxGrob(glue("Included in study",
                         "n = {incl}",
                         incl = txtInt(1835),
                         .sep = "\n"))
delay <- boxGrob(glue("Delay in treatment",
                      "n = {recr}",
                      recr = txtInt(28),
                      .sep = "\n"))

no_delay <- boxGrob(glue("No delay in treatment",
                      "n = {recr}",
                      recr = txtInt(1807),
                      .sep = "\n"))

not_screened <- boxGrob(glue("Not screened for ofi",
                             "n = {recr}",
                             recr = txtInt(11864-6582),
                             .sep = "\n"))

missing_data <- boxGrob(glue("Missing data",
                             "n = {recr}",
                             recr = txtInt(6582-1807),
                             .sep = "\n"))
grid.newpage()
vert <- spreadVertical(treated = treated,
                       screened = screened,
                       included = included,
                       grps = delay)
grps <- alignVertical(reference = vert$grps,
                      delay, no_delay) %>%
  spreadHorizontal()
vert$grps <- NULL

not_screened <- moveBox(not_screened,
                    x = .85,
                    y = .75,)

missing_data <- moveBox(missing_data,
                    x = .85,
                    y = .5,)

for (i in 1:(length(vert) - 1)) {
  connectGrob(vert[[i]], vert[[i + 1]], type = "vert") %>%
    print
}
connectGrob(vert$included, grps[[1]], type = "N")
connectGrob(vert$included, grps[[2]], type = "N")
connectGrob(vert$treated, not_screened, type = "L")
connectGrob(vert$screened, missing_data, type = "L")

# Print boxes
vert
grps
not_screened
missing_data

