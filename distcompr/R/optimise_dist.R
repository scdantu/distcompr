

density_function = function(dataset) {
  # density_output <- stats::density(x, bw = 'SJ')
  density_output <- stats::density(dataset, n = 512)
  return(density_output$y)
}

#' R package to analyse univariate distributions
#'
#' @param dataset A data matrix or data frame.
#' @param method If package is supposed to treat input dataset as univariate distributions then method name should be "pdf" else "non-pdf" to treat this dataset as continuous data vectors.
#' @param k An integer value used as to feed number of clusters.
#'
#' @return cluster groups of univariate distributions
#' @export
#' @section Details:
#' This package offers a flexibility to find an optimised distance measure on the basis of provided dataset for both continuous data vectors as well as for univariate distributions computed through kernel density function. The performance of optimized distance measure is evaluated through analyzing the clustering results.
#' @examples
#' \dontrun{
#' optimise_distance(dataset, method = "pdf", k = 3)
#' }
#'
optimise_distance <- function(dataset, method, k) {
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

   Similarity_Methods <-
    c(
      "euclidean",
      "manhattan",
      "chebyshev",
      "sorensen",
      "gower",
      "soergel",
      "kulczynski_d",
      "canberra",
      "lorentzian",
      "intersection",
      "wavehedges",
      "czekanowski",
      "motyka",
      "tanimoto",
      "inner_product",
      "harmonic_mean",
      "cosine",
      "jaccard",
      "dice",
      "hassebrook",
      "fidelity",
      "bhattacharyya",
      "squared_chord"
    )


  silhouette_score <- c()

  increment <- 1
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


    fclustering <-
      cluster::fanny(
        x = distance_values,
        k = k,
        maxit = 1500,
        memb.exp = 3
      )



    print(fclustering[10]$silinfo$avg.width)
    print(fclustering[10]$silinfo$clus.avg.widths)



    Predicted_Clusters <- fclustering$clustering

    if ((fclustering[10]$silinfo$clus.avg.widths > 0.20) == TRUE) {
      silhouette_score[increment] <- fclustering[10]$silinfo$avg.width
      names(silhouette_score)[increment] <- value
      increment <- increment + 1
    }

    else {
      increment <- increment
    }


  }
  print(names(silhouette_score))
  omtimised_distance_measure <- names(which.max(silhouette_score))
  if (method == "pdf") {
    distance_values_1 <-
      philentropy::distance(density_results,
                            method = omtimised_distance_measure,
                            use.row.names = TRUE)
  }

  else {
    distance_values_1 <-
      philentropy::distance(data_t, method = omtimised_distance_measure, use.row.names = TRUE)
  }
  fclustering_1 <-
    cluster::fanny(
      x = distance_values_1,
      k = k,
      maxit = 1500,
      memb.exp = 3
    )


  plot(fclustering_1)



  factoextra::fviz_cluster(
    fclustering_1,
    ellipse.type = "norm",
    repel = TRUE,
    palette = "jco",
    legend = "right"
  )




}
