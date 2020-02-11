// partition using hoare's scheme

#include <Rcpp.h>
using namespace Rcpp;

int partition(NumericVector a,int start,int end)
{
	double pivot = a[start];
	int i = start - 1;
	int j = end + 1;
	//Rcout << a <<"\n";
	while(1)
	{
		do {
			i++;
		} while (a[i] < pivot);

		do {
			j--;
		} while (pivot < a[j]);

		if(i >= j)
		  return j;
    
    std::swap(a[i], a[j]);
	}
}

void qsort(NumericVector a,int start,int end)
{
  //Rcout << start <<"," << end <<"\n";
  if(start < end)
  {
    int P_index = partition(a, start, end);
    //Rcout << P_index << "\n";
    qsort(a, start, P_index);
    qsort(a, P_index + 1, end);
  }
}


// [[Rcpp::export]]
NumericVector QuickSortH_WC(NumericVector arr)
{
  int len = arr.size();
  qsort(arr, 0, len-1);
  //Rcout << arr <<"\n";
  return 1;
}
