test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

testthat::test_that(
  desc = "insufficient arguments",
  code = {
    testthat::expect_error(
      CGPfunctions::newggslopegraph(
        dataframe = mtcars
      )
    )
  }
)

# path issue?
# disp_hist_ggplot <- ggplot(mtcars, aes(disp)) + geom_histogram()
# vdiffr::expect_doppelganger("ggplot2 histogram", disp_hist_ggplot)

# complex formatting with recycling and wider labels see vignette for more examples
# disp_slopegraph1 <- newggslopegraph(newcancer, Year, Survival, Type,
#                                    Title = "Estimates of Percent Survival Rates",
#                                    SubTitle = "Based on: Edward Tufte, Beautiful Evidence, 174, 176.",
#                                    Caption = "https://www.edwardtufte.com/bboard/q-and-a-fetch-msg?msg_id=0003nk",
#                                    LineColor = c("black", "red", "grey"),
#                                    LineThickness = .5
#                                    )
# vdiffr::expect_doppelganger("slopegraph1", disp_slopegraph1)

p <- newggslopegraph(newcancer, Year, Survival, Type,
                     Title = "Estimates of Percent Survival Rates",
                     SubTitle = "Based on: Edward Tufte, Beautiful Evidence, 174, 176.",
                     Caption = "https://www.edwardtufte.com/bboard/q-and-a-fetch-msg?msg_id=0003nk",
                     LineColor = c("black", "red", "grey"),
                     LineThickness = .5,
                     WiderLabels = TRUE
)
pb <- ggplot2::ggplot_build(p)

testthat::expect_equal(length(pb$data), 5L)
yyy <- c("Prostate", "Thyroid", "Testis", "Melanomas", "Breast", "Hodgkin's", "Uterus", "Urinary", "Cervix", "Larynx", "Rectum", "Kidney", "Colon", "Non-Hodgkin's", "Oral", "Ovary", "Leukemia", "Brain", "Multiple myeloma", "Stomach", "Lung", "Esophagus", "Liver", "Pancreas")
testthat::expect_identical(as.character(pb$data[[3]]$label), yyy)
