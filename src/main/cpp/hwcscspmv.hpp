#ifndef HWCSCSPMV_HPP
#define HWCSCSPMV_HPP

#include <vector>
#include <string>
#include <iostream>
using namespace std;

#include "cscspmv.hpp"
#include "wrapperregdriver.h"
#include "commonsemirings.hpp"
#include "seyrekconsts.hpp"

// number of registers per SpMV PE
#define HWSPMVPE_REGS   18

// TODO better control of acc-host buffer duplication in sw drivers --
// right now there are two copies of all (host+accel) and coherency mvs.
// are automatically called every time

template <class SpMVInd, class SpMVVal>
class HWSpMV : public virtual CSCSpMV<SpMVInd, SpMVVal>, public AddMulSemiring<SpMVInd, SpMVVal> {
public:
  HWSpMV(WrapperRegDriver * driver, unsigned int peNum = 0, const char * attachName = 0) {
    m_cyclesRegular = 0;
    m_attachName = attachName;
    m_platform = driver;
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
    m_peNum = peNum;
    if(attachName != 0)
      m_platform->attach(attachName);
  }

  virtual ~HWSpMV() {if(m_attachName !=0) m_platform->detach();}

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
    m_acc_indPtrs = (SpMVInd *) m_platform->allocAccelBuffer(m_indPtrSize);
    m_acc_inds = (SpMVInd *) m_platform->allocAccelBuffer(m_indSize);
    m_acc_nzData = (SpMVVal *) m_platform->allocAccelBuffer(m_nzDataSize);
    m_acc_x = (SpMVVal *) m_platform->allocAccelBuffer(m_xSize);
    m_acc_y = (SpMVVal *) m_platform->allocAccelBuffer(m_ySize);
    // copy matrix data host -> accel
    m_platform->copyBufferHostToAccel((void *)m_A->getIndPtrs(), (void *) m_acc_indPtrs, m_indPtrSize);
    m_platform->copyBufferHostToAccel((void *)m_A->getInds(), (void *) m_acc_inds, m_indSize);
    m_platform->copyBufferHostToAccel((void *)m_A->getNZData(), (void *) m_acc_nzData, m_nzDataSize);
    // set up matrix metadata in the accelerator
    set_csc_colPtr((AccelDblReg) m_acc_indPtrs);
    set_csc_cols(m_A->getCols());
    set_csc_inpVec((AccelDblReg) m_acc_x);
    set_csc_nz(m_A->getNNZ());
    set_csc_nzData((AccelDblReg) m_acc_nzData);
    set_csc_outVec((AccelDblReg) m_acc_y);
    set_csc_rowInd((AccelDblReg) m_acc_inds);
    set_csc_rows(m_A->getRows());
  }

  virtual void setx(SpMVVal * x) {
    // call base class impl
    CSCSpMV<SpMVInd, SpMVVal>::setx(x);
    // copy data
    m_platform->copyBufferHostToAccel((void *)x, (void *)m_acc_x, m_xSize);
  }

  virtual void sety(SpMVVal * y) {
    // call base class impl
    CSCSpMV<SpMVInd, SpMVVal>::sety(y);
    // copy data host -> accel
    m_platform->copyBufferHostToAccel((void *)y, (void *)m_acc_y, m_ySize);
  }

  virtual bool exec() {
    if(!m_A || !m_x || !m_y) throw "One or more SpMV data comps not assigned";
    // make sure x and y are up to date on the accel
    setx(m_x);
    sety(m_y);
    // TODO may not be always needed to do init and flush
    execAccelMode(START_INIT);
    execAccelMode(START_REGULAR);
    execAccelMode(START_FLUSH);
    copyOutputToHost();
    return true;
  }

  // TODO expose proper stats
  virtual unsigned int statInt(std::string name) {
    if(name == "cyclesRegular") return m_cyclesRegular;
	else return 0;
  }

  virtual std::vector<std::string> statKeys() {
    std::vector<std::string> keys;
    keys.push_back("cyclesRegular");
    return keys;
  }

  // HWSpMV-specific functions
  void copyOutputToHost() {
    // copy back y data to the host side
    m_platform->copyBufferAccelToHost((void *)m_acc_y, (void *)m_y, m_ySize);
  }

  virtual bool isFinished() {
    return get_finished() == 1;
  }

  virtual void setModeAsync(SeyrekModes mode, bool start) {
    // TODO check current status first!
    set_mode(mode);
    if(start) set_start(1);
    else {
      if(mode == START_REGULAR) {
        m_cyclesRegular = get_cycle_count();
      }
      set_start(0);
    }
  }

  virtual void setOutstandingTxns(unsigned int txns) {
    // TODO txns should be verified against the hardware capabilities!
    if(txns > 16 || txns == 0)
      throw "The HW probably doesn't support that many transactions";
    set_ctx_txns(txns);
    execAccelMode(START_CONFIG);
  }

protected:
  // statistics
  unsigned int m_cyclesRegular;

  // matrix data
  using CSCSpMV<SpMVInd, SpMVVal>::m_A;
  using CSCSpMV<SpMVInd, SpMVVal>::m_y;
  using CSCSpMV<SpMVInd, SpMVVal>::m_x;

  WrapperRegDriver * m_platform;
  const char * m_attachName;
  unsigned int m_peNum;


  // register offsets for the HW SpMV control/status registers
  // note that these may change if the SpMV accelerator interface is modified in Chisel!
  typedef enum {
    offsStart       = 1,
    offsMode        = 2,
    offsFinished    = 3,
    offsColPtrHi    = 4,
    offsColPtrLo    = 5,
    offsRowIndHi    = 6,
    offsRowIndLo    = 7,
    offsNzDatHi     = 8,
    offsNzDatLo     = 9,
    offsInpVecHi    = 10,
    offsInpVecLo    = 11,
    offsOutVecHi    = 12,
    offsOutVecLo    = 13,
    offsRows        = 14,
    offsCols        = 15,
    offsNZ          = 16,
    offsCycleCount  = 17,
    offsCtxTxns		= 18
  } HWSpMVReg;
  // readReg and writeReg use peNum to add a base offset to the desired register ID
  AccelReg readReg(HWSpMVReg reg) {return m_platform->readReg(m_peNum * HWSPMVPE_REGS + reg);}
  void writeReg(HWSpMVReg reg, AccelReg v) {m_platform->writeReg(m_peNum * HWSPMVPE_REGS + reg, v);}
  // register accessor functions for this PE
  AccelReg get_signature() {return m_platform->readReg(0);} // signature is always at reg 0 (one for the entire accelerator)
  void set_start(AccelReg value) {writeReg(offsStart, value);}
  void set_mode(AccelReg value) {writeReg(offsMode, value);}
  AccelReg get_finished() {return readReg(offsFinished);}
  void set_csc_colPtr(AccelDblReg value) { writeReg(offsColPtrHi, (AccelReg)(value >> 32)); writeReg(offsColPtrLo, (AccelReg)(value & 0xffffffff)); }
  void set_csc_rowInd(AccelDblReg value) { writeReg(offsRowIndHi, (AccelReg)(value >> 32)); writeReg(offsRowIndLo, (AccelReg)(value & 0xffffffff)); }
  void set_csc_nzData(AccelDblReg value) { writeReg(offsNzDatHi, (AccelReg)(value >> 32)); writeReg(offsNzDatLo, (AccelReg)(value & 0xffffffff)); }
  void set_csc_inpVec(AccelDblReg value) { writeReg(offsInpVecHi, (AccelReg)(value >> 32)); writeReg(offsInpVecLo, (AccelReg)(value & 0xffffffff)); }
  void set_csc_outVec(AccelDblReg value) { writeReg(offsOutVecHi, (AccelReg)(value >> 32)); writeReg(offsOutVecLo, (AccelReg)(value & 0xffffffff)); }
  void set_csc_rows(AccelReg value) {writeReg(offsRows, value);}
  void set_csc_cols(AccelReg value) {writeReg(offsCols, value);}
  void set_csc_nz(AccelReg value) {writeReg(offsNZ, value);}
  AccelReg get_cycle_count() {return readReg(offsCycleCount);}
  void set_ctx_txns(AccelReg value) {writeReg(offsCtxTxns, value);}


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

  void execAccelMode(SeyrekModes mode) {
    // TODO ensure finished before starting new commands!
    set_mode(mode);
    set_start(1);
    // TODO add timeout option here?
    while(get_finished() != 1);
    if(mode == START_REGULAR) {
      m_cyclesRegular = get_cycle_count();
    }
    set_start(0);
  }

};

#endif // HWCSCSPMV_HPP
