#ifndef HWCSCSPMV_HPP
#define HWCSCSPMV_HPP

#include "wrapperregdriver.h"

// implements the exec() for software-based SpMV-over-semirings
// add() and mul() must be implemented in the derived class

template <class SpMVInd, class SpMVVal>
class HWSpMV : public virtual CSCSpMV<SpMVInd, SpMVVal> {
protected:
  using CSCSpMV<SpMVInd, SpMVVal>::m_A;
  using CSCSpMV<SpMVInd, SpMVVal>::m_y;
  using CSCSpMV<SpMVInd, SpMVVal>::m_x;

  WrapperRegDriver * m_regDriver;

public:
  HWSpMV(WrapperRegDriver * driver) {
    m_regDriver = driver;
  }
  virtual ~HWSpMV() {};

  virtual bool exec() {
    // TODO allocate accel buffers
    // TODO handle host2accel buffer copies
    // TODO init, regular, flush
    // TODO handle accel2host buffer copies
    return true;
  }
};

#endif // HWCSCSPMV_HPP
