#ifndef PARALLELSPMV_HPP
#define PARALLELSPMV_HPP

#include <vector>
#include "cscspmv.hpp"
#include "hwcscspmv.hpp"
#include "commonsemirings.hpp"
#include "seyrekconsts.hpp"

#define MAX_HWSPMV_PE   64

// a simple driver for handling multiple HWSpMVs executing an SpMV
// operation in parallel

// TODO:
// - only for static prepartitioned for now, support dynamic
// - add statistics from individual PEs + aggregate stats
// - more control over partition coherency actions (at HWSpMV's)
// - better control over the partitioning process
// - callback-based PE completion
// - async exec?

template <class SpMVInd, class SpMVVal>
class ParallelHWSpMV : public virtual CSCSpMV<SpMVInd, SpMVVal>, public AddMulSemiring<SpMVInd, SpMVVal> {
public:
  ParallelHWSpMV(unsigned int numPEs, WrapperRegDriver * driver,
                 const char * attachName) {
    m_attachName = attachName;
    m_platform = driver;
    m_numPEs = numPEs;
    for(unsigned int pe = 0; pe < m_numPEs; pe++) {
        m_pe[pe] = new HWSpMV<SpMVInd, SpMVVal>(driver, pe);
    }
    m_platform->attach(attachName);
  }

  virtual ~ParallelHWSpMV() {
    m_platform->detach();
    for(unsigned int pe = 0; pe < m_numPEs; pe++) {
        delete m_pe[pe];
    }
  }

  virtual void setA(CSC<SpMVInd, SpMVVal> * A) {
    CSCSpMV<SpMVInd, SpMVVal>::setA(A);
    // create the partitions
    m_partitions = A->partition(m_numPEs);
    // assign the partitions to PEs
    for(unsigned int pe = 0; pe < m_numPEs; pe++) {
      m_pe[pe]->setA(m_partitions[pe]);
    }
  }

  virtual void setx(SpMVVal * x) {
    CSCSpMV<SpMVInd, SpMVVal>::setx(x);
    // assign input vector for each PE
    // TODO dont't mke multiple copies of input vector
    for(unsigned int pe = 0; pe < m_numPEs; pe++) {
      m_pe[pe]->setx(x);
    }
  }

  virtual void sety(SpMVVal * y) {
    CSCSpMV<SpMVInd, SpMVVal>::sety(y);
    // assign rebased output vector for each PE
    for(unsigned int pe = 0; pe < m_numPEs; pe++) {
      m_pe[pe]->sety(&y[m_partitions[pe]->getStartingRow()]);
    }
  }

  virtual bool exec() {
    execForAll(START_INIT);
    execForAll(START_REGULAR);
    execForAll(START_FLUSH);

    for(unsigned int pe = 0; pe < m_numPEs; pe++) {
      m_pe[pe]->copyOutputToHost();
    }
  }

  // TODO expose proper stats
  virtual unsigned int statInt(std::string name) { return 0;}

  virtual std::vector<std::string> statKeys() {
    std::vector<std::string> keys;
    keys.push_back("matrix");
  }

protected:
  unsigned int m_numPEs;
  const char * m_attachName;
  HWSpMV<SpMVInd, SpMVVal> * m_pe[MAX_HWSPMV_PE];
  WrapperRegDriver * m_platform;
  std::vector<CSC<SpMVInd, SpMVVal> * > m_partitions;

  bool isAllPEsFinished() {
    bool allFinished = true;
    for(unsigned int pe = 0; pe < m_numPEs; pe++) {
        allFinished = allFinished & m_pe[pe]->isFinished();
    }
    return allFinished;
  }

  void execForAll(SeyrekModes mode) {
    // TODO check status of PEs first!
    // set mode and give start signal
    for(unsigned int pe = 0; pe < m_numPEs; pe++) {
      m_pe[pe]->setModeAsync(mode, true);
    }
    // TODO sleep/yield while waiting?
    while(!isAllPEsFinished());
    // clear start signal
    for(unsigned int pe = 0; pe < m_numPEs; pe++) {
      m_pe[pe]->setModeAsync(mode, false);
    }
  }

};

#endif // PARALLELSPMV_HPP
