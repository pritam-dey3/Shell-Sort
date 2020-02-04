#include <Rcpp.h>
using namespace Rcpp;

int partition(NumericVector a,int start,int end)
{
  int pivot = a[end];
  
  int P_index = start;
  int i;
  
  for(i=start;i<end;i++)
  {
    //Rcout << a[i] << " " << pivot << "\n";
    if(a[i] < pivot)
    {
      //special.Swap(a, i, P_index);
      std::swap(a[i], a[P_index]);
      P_index++;
    }
  }
  
  //special.Swap(a, end, P_index);
  std::swap(a[end], a[P_index]);
  
  return P_index;
}


void qsort(NumericVector a,int start,int end)
{
  if(start < end)
  {
    int P_index = partition(a, start, end);
    //Rcout << a << "\n";
    qsort(a, start,P_index-1);
    qsort(a, P_index+1,end);
  }
}

// [[Rcpp::export]]
NumericVector QuickSortL_WC(NumericVector arr)
{
  int len = arr.size();
  qsort(arr, 0, len-1);
  
  return 1;
}
