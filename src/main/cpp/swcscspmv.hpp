#ifndef SWCSCSPMV_HPP
#define SWCSCSPMV_HPP

#include "cscspmv.hpp"

// implements the exec() for software-based SpMV-over-semirings
// add() and mul() must be implemented in the derived class

template <class SpMVInd, class SpMVVal>
class SWSpMV : public virtual CSCSpMV<SpMVInd, SpMVVal> {
protected:
  using CSCSpMV<SpMVInd, SpMVVal>::m_A;
  using CSCSpMV<SpMVInd, SpMVVal>::m_y;
  using CSCSpMV<SpMVInd, SpMVVal>::m_x;

public:
  virtual ~SWSpMV() {};

  virtual bool exec() {
    unsigned int cols = m_A->getCols();
    SpMVInd * colPtr = m_A->getIndPtrs();
    SpMVInd * rowInds = m_A->getInds();
    SpMVVal * nzData = m_A->getNzData();
    for(SpMVInd col = 0; col < cols; col++) {
      for(SpMVInd ep = colPtr[col]; ep < colPtr[col+1]; ep++) {
        SpMVInd rowInd = rowInds[ep];
        SpMVVal mulRes = this->mul(nzData[ep], m_x[col], rowInd, col);
        SpMVVal addRes = this->add(m_y[rowInd], mulRes, rowInd, col);
        m_y[rowInd] = addRes;
      }
    }
    return true;
  }
};

#endif // SWCSCSPMV_HPP
