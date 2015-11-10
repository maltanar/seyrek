#ifndef CSCSPMV_HPP
#define CSCSPMV_HPP

#include <vector>
#include <string>
#include "semiring.hpp"

// base class for doing SpMV-on-semirings for CSC-encoded sparse matrices
template <class SpMVInd, class SpMVVal>
class CSCSpMV : public virtual Semiring<SpMVInd, SpMVVal> {
public:
  virtual ~CSCSpMV() {};

  // access to basic sparse matrix properties
  SpMVInd getRows() {return m_rows;}
  SpMVInd getCols() {return m_cols;}
  SpMVInd getNNZ() {return m_nnz;}
  SpMVVal * getY() {return m_y;}

  // execute one SpMV step, y = A*x
  virtual bool exec() = 0;
  // functions for querying stats
  virtual unsigned int statInt(std::string name) = 0;
  virtual std::vector<std::string> statKeys() = 0;

protected:
  SpMVInd m_rows;
  SpMVInd m_cols;
  SpMVInd m_nnz;

  SpMVInd * m_colPtr;
  SpMVInd * m_rowInd;
  SpMVVal * m_nzData;

  SpMVVal * m_x;
  SpMVVal * m_y;
};

#endif
