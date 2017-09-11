Vcombo <- Venn(SetNames = c("Female", "Visible Minority", "CS Major"),
                 Weight = c(0, 4148, 409, 604, 543, 67, 183, 146))

obj <- Vennerable::compute.Venn(Vcombo, doWeights = T, doEuler = TRUE)

r <- c()
center <- matrix(NA, ncol = 2, nrow = 0)

for (i in seq_along(obj@edgeList)) {
  r <- c(r, obj@edgeList[[i]]@radius)
  center <- rbind(center, as.numeric(obj@edgeList[[i]]@centre))
}

center <- center[!duplicated(center), ]
r <- unique(r)



