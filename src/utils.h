#include <RcppEigen.h>
#include "Eigen/Dense"
#include <vector>
#include <unordered_set>
using Eigen::VectorXf;

#include <Rcpp.h>
using namespace Rcpp;

#ifndef UTILS_H
#define UTILS_H

double mse( const Eigen::VectorXf & y, const Eigen::VectorXf & y_hat );
double rmse( const Eigen::VectorXf & y, const Eigen::VectorXf & y_hat );
double mae( const Eigen::VectorXf & y, const Eigen::VectorXf & y_hat );
double accuracy( const Eigen::VectorXf & y, const Eigen::VectorXf & y_hat);
int eig_which_is_min(Eigen::VectorXf x);

template <class T> int max_size( T a, T b) {
  return a.size() < b.size() ? b.size() : a.size();
}

template <class T> int min_size( T a, T b) {
  return a.size() < b.size() ? a.size() : b.size();
}

template <class T, typename U> T intersect( T a, T b ) {
  if( a.empty() || b.empty() ) {
    return T();
  }
  std::unordered_set<U> set( a.begin(), a.end() );
  T result;
  result.reserve( min_size(a,b) );
  for (auto item : b ) {
    if( set.count(item) ) {
      result.push_back(item);
      set.erase(item);
    }
  }
  return result;
}

template <class T, class I, typename R> void eigvec_indexed_replacement( T &eigen_vec, I index_vec, R replacement ) {
  for(const auto& index: index_vec) {
    eigen_vec(index) = replacement;
  }
}

template <typename T> std::vector<T> sequence( T from, T to, T by ) {
  int size = (to-from)/by;
  std::vector<T> result( size );
  result[0] = from;
  for( int i = 1; i < size; i++) {
    result[i] = result[i-1] + by;
  }
  return result;
}

#endif
