#ifndef SWCSCSPMV_HPP
#define SWCSCSPMV_HPP

#include "cscspmv.hpp"

// implements the exec() for software-based SpMV-over-semirings
// add() and mul() must be implemented in the derived class

template <class SpMVInd, class SpMVVal>
class SWSpMV : public virtual CSCSpMV<SpMVInd, SpMVVal> {
protected:
  using CSCSpMV<SpMVInd, SpMVVal>::m_cols;
  using CSCSpMV<SpMVInd, SpMVVal>::m_colPtr;
  using CSCSpMV<SpMVInd, SpMVVal>::m_rowInd;
  using CSCSpMV<SpMVInd, SpMVVal>::m_y;
  using CSCSpMV<SpMVInd, SpMVVal>::m_x;
  using CSCSpMV<SpMVInd, SpMVVal>::m_nzData;

public:
  virtual ~SWSpMV() {};

  virtual bool exec() {
    for(SpMVInd col = 0; col < m_cols; col++) {
      for(SpMVInd ep = m_colPtr[col]; ep < m_colPtr[col+1]; ep++) {
        SpMVInd rowInd = m_rowInd[ep];
        SpMVVal mulRes = this->mul(m_nzData[ep], m_x[col], rowInd, col);
        SpMVVal addRes = this->add(m_y[rowInd], mulRes, rowInd, col);
        m_y[rowInd] = addRes;
      }
    }
    return true;
  }
};

#endif // SWCSCSPMV_HPP
