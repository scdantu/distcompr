



density_function = function(x) {
  density_output <- stats::density(x, n= 100)
  return(density_output$y)
}



function_1 <- function(dataset, method, class_labels) {
  if (method == "pdf") {
    # data_1 <- dataset
    density_results <- apply(X = dataset,
                             FUN = density_function,
                             MARGIN = 2)
    density_results <- t(density_results)
    density_results <- as.data.frame(density_results)
  }


  else {
    data_t <- t(dataset)
  }

  Similarity_Methods <- c("euclidean", "manhattan", "chebyshev", "sorensen", "gower", "soergel", "kulczynski_d", "canberra", "lorentzian", "intersection", "wavehedges", "czekanowski", "motyka", "tanimoto", "inner_product", "harmonic_mean", "cosine", "jaccard", "dice", "hassebrook", "fidelity", "bhattacharyya", "squared_chord")

  val <- c()
  Actual_Clusters <- class_labels
  cal <- 1
  for (value in Similarity_Methods)

  {

    if (method == "pdf") {
      distance_values <-
        philentropy::distance(density_results,
                              method = value,
                              use.row.names = TRUE)
    }

    else {
      distance_values <-
        philentropy::distance(data_t, method = value, use.row.names = TRUE)
    }


    fclustering <- cluster::fanny(x = distance_values, k = 3, maxit = 500, memb.exp = 1.5)

    confidence_interval <- (mean(fclustering$membership[101:200,2]) + mean(fclustering$membership[1:100,1]) + mean(fclustering$membership[201:300,3]))/3
    print(confidence_interval)

    print(fclustering[10]$silinfo$avg.width)
    print(fclustering[10]$silinfo$clus.avg.widths)
    val[cal] <- fclustering[10]$silinfo$avg.width
    # print(head(fclustering$membership[200:205,]))
    Predicted_Clusters <- fclustering$clustering
    rand_value <- fossil::rand.index(Actual_Clusters, Predicted_Clusters)
    purity_value <- funtimes::purity(Actual_Clusters, Predicted_Clusters)
    print(rand_value)
    print(purity_value)
    cal <- cal + 1
  }
  names(val) <- Similarity_Methods
  dist <- names(which.max(val))

  if (method == "pdf") {
    distance_values_1 <-
      philentropy::distance(density_results,
                            method = dist,
                            use.row.names = TRUE)
  }

  else {
    distance_values_1 <-
      philentropy::distance(data_t, method = dist, use.row.names = TRUE)
  }

  fclustering_1 <- cluster::fanny(x = distance_values_1, k = 3, maxit = 500, memb.exp = 1.5)


  confidence_interval <- (mean(fclustering$membership[101:200,2]) + mean(fclustering$membership[1:100,1]) + mean(fclustering$membership[201:300,3]))/3
  print(confidence_interval)

  print(dist)
  plot(fclustering_1)

  Predicted_Clusters <- fclustering_1$clustering

  factoextra::fviz_cluster(fclustering_1, ellipse.type = "norm", repel = TRUE,
                           palette = "jco",
                           legend = "right")

  rand_value <- fossil::rand.index(Actual_Clusters, Predicted_Clusters)
  purity_value <- funtimes::purity(Actual_Clusters, Predicted_Clusters)
  print(rand_value)
  print(purity_value)
}
