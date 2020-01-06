#include <Rcpp.h>
using namespace Rcpp;

class Special{
public:
  int nswap;
  int ncomp;
  
  bool Less(int a, int b){
    ncomp++;
    return a <= b;
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
NumericVector ShellSort(NumericVector arr, NumericVector Gaps) {
  
  int gap, j, i;
  int n = arr.size();
  Special special;
  
  for(int t = Gaps.size()-1; t >= 0; t--) {
    gap = Gaps[t]; 
    for(j = gap; j<n; j++) {
      int key = arr[j];
      for(i = j-gap; i>=0; i -= gap) {
        if(special.Less(arr[i], key))
          break;
        else
          arr[i+gap] = arr[i];
      }
      arr[i+gap] = key;
    }
  }
  NumericVector res(2);
  res[0] = special.nswap;
  res[1] = special.ncomp;
  //Rcout << arr << "\n";
  return res;
}