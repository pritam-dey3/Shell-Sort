#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector ShellSort(NumericVector arr, NumericVector Gaps) {
  
  int gap, j, i;
  int n = arr.size();
  
  for(int t = Gaps.size()-1; t >= 0; t--) {
    gap = Gaps[t]; 
    for(j = gap; j<n; j++) {
      int key = arr[j];
      for(i = j-gap; i>=0; i -= gap) {
        if(arr[i] < key)
          break;
        else
          arr[i+gap] = arr[i];
      }
      arr[i+gap] = key;
    }
  }
  //Rcout << arr << "\n";
  return 1;
}