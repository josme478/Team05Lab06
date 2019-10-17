#include <Rcpp.h>
#include <numeric>

using namespace Rcpp;


//' vecSum
//' 
//' This function returns the sum of the elements of a vector
//' @param v A numeric vector
//' @export
// [[Rcpp::export]]

double vecSum(NumericVector v) {
  return std::accumulate(v.begin(), v.end(), 0.0);
}
