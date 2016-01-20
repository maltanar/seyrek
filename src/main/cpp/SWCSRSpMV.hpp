#ifndef SWCSRSPMV_HPP
#define SWCSRSPMV_HPP

#include "CSRSpMV.hpp"

// implements the exec() for software-based SpMV-over-semirings
// add() and mul() must be implemented in the derived class

template <class SpMVInd, class SpMVVal>
class SWCSRSpMV : public virtual CSRSpMV<SpMVInd, SpMVVal> {
protected:
  using CSRSpMV<SpMVInd, SpMVVal>::m_A;
  using CSRSpMV<SpMVInd, SpMVVal>::m_y;
  using CSRSpMV<SpMVInd, SpMVVal>::m_x;

public:
  virtual ~SWCSRSpMV() {};

  virtual bool exec() {
    unsigned int rows = m_A->getRows();
    SpMVInd * rowPtr = m_A->getIndPtrs();
    SpMVInd * colInds = m_A->getInds();
    SpMVVal * nzData = m_A->getNZData();

    for(SpMVInd row = 0; row < rows; row++) {
      SpMVVal addRes = m_y[row];
      for(SpMVInd ep = rowPtr[row]; ep < rowPtr[row+1]; ep++) {
        SpMVInd colInd = colInds[ep];
        SpMVVal mulRes = this->mul(nzData[ep], m_x[colInd], row, colInd);
        addRes = this->add(addRes, mulRes, row, colInd);
      }
      m_y[row] = addRes;
    }
    return true;
  }
};

#endif // SWCSRSPMV_HPP
