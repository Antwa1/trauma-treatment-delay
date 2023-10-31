library(Gmisc, quietly = TRUE)
library(glue)
library(htmlTable)
library(grid)
library(magrittr)

org_cohort <- boxGrob(glue("Treated at KS between 2012 and 2022",
                           "n = {pop}",
                           pop = txtInt(11,864),
                           .sep = "\n"))
eligible <- boxGrob(glue("Eligible",
                         "n = {pop}",
                         pop = txtInt(10032),
                         .sep = "\n"))
included <- boxGrob(glue("Randomized",
                         "n = {incl}",
                         incl = txtInt(122),
                         .sep = "\n"))
grp_a <- boxGrob(glue("Treatment A",
                      "n = {recr}",
                      recr = txtInt(43),
                      .sep = "\n"))

grp_b <- boxGrob(glue("Treatment B",
                      "n = {recr}",
                      recr = txtInt(122 - 43 - 30),
                      .sep = "\n"))

excluded <- boxGrob(glue("Excluded (n = {tot}):",
                         " - not interested: {uninterested}",
                         " - contra-indicated: {contra}",
                         tot = 30,
                         uninterested = 12,
                         contra = 30 - 12,
                         .sep = "\n"),
                    just = "left")

grid.newpage()
vert <- spreadVertical(org_cohort,
                       eligible = eligible,
                       included = included,
                       grps = grp_a)
grps <- alignVertical(reference = vert$grps,
                      grp_a, grp_b) %>%
  spreadHorizontal()
vert$grps <- NULL

excluded <- moveBox(excluded,
                    x = .8,
                    y = coords(vert$included)$top + distance(vert$eligible, vert$included, half = TRUE, center = FALSE))

for (i in 1:(length(vert) - 1)) {
  connectGrob(vert[[i]], vert[[i + 1]], type = "vert") %>%
    print
}
connectGrob(vert$included, grps[[1]], type = "N")
connectGrob(vert$included, grps[[2]], type = "N")

connectGrob(vert$eligible, excluded, type = "L")

# Print boxes
vert
grps
excluded

