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


int partition(NumericVector a,int start,int end, Special& special)
{
  int pivot = a[end];
  
  int P_index = start;
  int i;
  
  for(i=start;i<end;i++)
  {
    //Rcout << a[i] << " " << pivot << "\n";
    if(special.Less(a[i], pivot))
    {
      special.Swap(a, i, P_index);
      P_index++;
    }
  }
  
  special.Swap(a, end, P_index);
  
  return P_index;
}


void qsort(NumericVector a,int start,int end, Special& special)
{
  if(start < end)
  {
    int P_index = partition(a, start, end, special);
    //Rcout << a <<"; swap: " << special.nswap << "; com: " << special.ncomp << "\n";
    qsort(a, start,P_index-1, special);
    qsort(a, P_index+1,end, special);
  }
}

// [[Rcpp::export]]
NumericVector QuickSortL(NumericVector arr)
{
  Special special;
  int len = arr.size();
  qsort(arr, 0, len-1, special);
  
  NumericVector res(2);
  res[0] = special.nswap;
  res[1] = special.ncomp;
  //Rcout << arr << "\n";
  return res;
}
