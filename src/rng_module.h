#ifndef RNG_MODULE_
#define RNG_MODULE_

#include <algorithm>
#include <vector>
#include <random>
// [[Rcpp::plugins("cpp17")]]

std::vector<int> sample_indices( const int n,
                                 const int size)
{
  std::vector<int> result(n);
  for(int i=0; i<n;i++) {
    result[i] = i;
  }

  std::shuffle(result.begin(),
               result.end(),
               std::mt19937{std::random_device{}()}
              );
  result.resize(size);

  return result;
}

template <class T> T sample_from_sequence( T seq, int size ) {

  auto indices = sample_indices( seq.size(), size );

  T result(size);
  for( int i = 0;i<size;i++) {
    result[i] = seq[indices[i]];
  }
  return result;
}

template <class T> T ordered_sample_from_sequence( T seq, int size ) {
  T result;
  std::sample(seq.begin(),
              seq.end(),
              std::back_inserter(result),
              size, std::mt19937{std::random_device{}()}
             );
  return result;
}

#endif
