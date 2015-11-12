#ifndef HWCSCSPMV_HPP
#define HWCSCSPMV_HPP

#include "cscspmv.hpp"
#include "wrapperregdriver.h"

// implements the exec() for software-based SpMV-over-semirings
// add() and mul() must be implemented in the derived class

// TODO better control of acc-host buffer duplication in sw drivers --
// right now there are two copies of all (host+accel) and coherency mvs.
// are automatically called every time

template <class SpMVInd, class SpMVVal>
class HWSpMV : public virtual CSCSpMV<SpMVInd, SpMVVal> {
protected:
  using CSCSpMV<SpMVInd, SpMVVal>::m_A;
  using CSCSpMV<SpMVInd, SpMVVal>::m_y;
  using CSCSpMV<SpMVInd, SpMVVal>::m_x;

  WrapperRegDriver * m_platform;
  const char * m_attachName;
  AccelReg readReg(unsigned int i) {return m_platform->readReg(i);}
  void writeReg(unsigned int i, AccelReg v) {m_platform->writeReg(i,v);}

  // register access functions -- copied from the TidbitsPlatformWrapper-generated driver
  // note that these will change if the SpMV accelerator interface is modified!
  AccelReg get_signature() {return readReg(0);}
  void set_start(AccelReg value) {writeReg(1, value);}
  void set_mode(AccelReg value) {writeReg(2, value);}
  AccelReg get_finished() {return readReg(3);}
  void set_csc_colPtr(AccelDblReg value) { writeReg(4, (AccelReg)(value >> 32)); writeReg(5, (AccelReg)(value & 0xffffffff)); }
  void set_csc_rowInd(AccelDblReg value) { writeReg(6, (AccelReg)(value >> 32)); writeReg(7, (AccelReg)(value & 0xffffffff)); }
  void set_csc_nzData(AccelDblReg value) { writeReg(8, (AccelReg)(value >> 32)); writeReg(9, (AccelReg)(value & 0xffffffff)); }
  void set_csc_inpVec(AccelDblReg value) { writeReg(10, (AccelReg)(value >> 32)); writeReg(11, (AccelReg)(value & 0xffffffff)); }
  void set_csc_outVec(AccelDblReg value) { writeReg(12, (AccelReg)(value >> 32)); writeReg(13, (AccelReg)(value & 0xffffffff)); }
  void set_csc_rows(AccelReg value) {writeReg(14, value);}
  void set_csc_cols(AccelReg value) {writeReg(15, value);}
  void set_csc_nz(AccelReg value) {writeReg(16, value);}



  // accelerator-side versions of SpMV data
  SpMVInd * m_acc_indPtrs;
  SpMVInd * m_acc_inds;
  SpMVVal * m_acc_nzData;
  SpMVVal * m_acc_x;
  SpMVVal * m_acc_y;
  unsigned int m_indPtrSize;
  unsigned int m_indSize;
  unsigned int m_nzDataSize;
  unsigned int m_xSize;
  unsigned int m_ySize;

  virtual void setA(CSC<SpMVInd, SpMVVal> * A) {
    // free the old accel buffers first, if alloc'd
    if(m_acc_indPtrs != 0) {
      m_platform->deallocAccelBuffer((void *) m_acc_indPtrs);
      m_platform->deallocAccelBuffer((void *) m_acc_inds);
      m_platform->deallocAccelBuffer((void *) m_acc_nzData);
    }
    if(m_acc_x) m_platform->deallocAccelBuffer((void *) m_acc_x);
    if(m_acc_y) m_platform->deallocAccelBuffer((void *) m_acc_y);
    // call base class impl
    CSCSpMV<SpMVInd, SpMVVal>::setA(A);
    // calculate the associated buffer sizes
    m_indPtrSize = sizeof(SpMVInd) * (m_A->getCols() + 1);
    m_indSize = sizeof(SpMVInd) * m_A->getNNZ();
    m_nzDataSize = sizeof(SpMVVal) * m_A->getNNZ();
    m_xSize = sizeof(SpMVVal) * m_A->getCols();
    m_ySize = sizeof(SpMVVal) * m_A->getRows();
    // alloc new accel buffers
    m_acc_indPtrs = (SpMVInd *) m_platform->allocAccelBuffer(indPtrSize);
    m_acc_inds = (SpMVInd *) m_platform->allocAccelBuffer(indSize);
    m_acc_nzData = (SpMVVal *) m_platform->allocAccelBuffer(nzDataSize);
    m_acc_x = (SpMVVal *) m_platform->allocAccelBuffer(xSize);
    m_acc_y = (SpMVVal *) m_platform->allocAccelBuffer(ySize);
    // copy matrix data host -> accel
    m_platform->copyBufferHostToAccel((void *)m_A->getIndPtrs(), (void *) m_acc_indPtrs, indPtrSize);
    m_platform->copyBufferHostToAccel((void *)m_A->getInds(), (void *) m_acc_inds, indSize);
    m_platform->copyBufferHostToAccel((void *)m_A->getNZData(), (void *) m_acc_nzData, nzDataSize);
  }

  virtual void setx(SpMVVal * x) {
    // call base class impl
    CSCSpMV<SpMVInd, SpMVVal>::setx(x);
    // copy data
    m_platform->copyBufferHostToAccel((void *)x, (void *)m_acc_x, xSize);
  }

  virtual void sety(SpMVVal * y) {
    // call base class impl
    CSCSpMV<SpMVInd, SpMVVal>::sety(y);
    // copy data host -> accel
    m_platform->copyBufferHostToAccel((void *)y, (void *)m_acc_y, ySize);
  }

  // mode settings for Seyrek
  typedef enum {
    START_REGULAR = 0,
    START_INIT = 1,
    START_FLUSH = 2
  } SeyrekModes;

  void execAccelMode(SeyrekModes mode) {
    // TODO ensure finished before starting new commands!
    set_mode(mode);
    set_start(1);
    // TODO add timeout option here?
    while(get_finished() != 1);
    set_start(0);
  }

public:
  HWSpMV(const char * attachName, WrapperRegDriver * driver) {
    m_platform = driver;
    m_attachName = attachName;
    m_platform->attach(attachName);
    m_acc_indPtrs = 0;
    m_acc_inds = 0;
    m_acc_nzData = 0;
    m_acc_x = 0;
    m_acc_y = 0;
    m_indPtrSize = 0;
    m_indSize = 0;
    m_nzDataSize = 0;
    m_xSize = 0;
    m_ySize = 0;
  }
  virtual ~HWSpMV() {m_platform->detach();}

  virtual bool exec() {
    if(!m_A || !m_x || !m_y) throw "One or more SpMV data comps not assigned";
    setx(m_x);
    sety(m_y);
    // TODO may not be always needed to do init and flush
    execAccelMode(START_INIT);
    execAccelMode(START_REGULAR);
    execAccelMode(START_FLUSH);
    // copy back y data to the host side
    m_platform->copyBufferAccelToHost((void *)m_acc_y, (void *)m_y, ySize);
    return true;
  }
};

#endif // HWCSCSPMV_HPP
