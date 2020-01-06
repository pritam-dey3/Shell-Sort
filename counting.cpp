// counting swaps and comparisons of different swaps
#include <Rcpp.h>
using namespace Rcpp;

class Special{
public:
  int nswap;
  int ncomp;
  
  bool Less(int a, int b){
    ncomp++;
    return a < b;
  }
  
  void Swap(NumericVector arr, int i, int j){
    nswap++;
    int temp = arr[i];
    arr[i] = arr[j];
    arr[j] = temp;
  }
  
  Special(){
      nswap = 0;
      ncomp = 0;
  }

};
  

// [[Rcpp::export]]
NumericVector bubble_sort(NumericVector arr) {
  
  int len = arr.size();
  Special special;
  
  for(int i = 0; special.Less(i, len); i++) {
    for(int j = i+1; special.Less(j, len); j++)
    {
      if(special.Less(arr[j], arr[i])) {
        special.Swap(arr, j , i);
      }
    }
  }
  
  NumericVector res(2);
  res[0] = special.nswap;
  res[1] = special.ncomp;
  
  return res;
}
