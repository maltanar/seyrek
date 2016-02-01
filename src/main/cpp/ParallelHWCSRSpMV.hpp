#ifndef PARALLELSPMVCSR_HPP
#define PARALLELSPMVCSR_HPP

#include <iostream>
using namespace std;

#include <vector>
#include "CSRSpMV.hpp"
#include "HWCSRSpMV.hpp"
#include "commonsemirings.hpp"
#include "seyrekconsts.hpp"

#define MAX_HWSPMV_PE   64

// a simple driver for handling multiple HWSpMVs executing an SpMV
// operation in parallel

// TODO:
// - add statistics from individual PEs + aggregate stats
// - more control over partition coherency actions (at HWSpMV's)
// - better control over the partitioning process
// - callback-based PE completion
// - async exec?

template <class SpMVInd, class SpMVVal>
class ParallelHWCSRSpMV : public virtual CSRSpMV<SpMVInd, SpMVVal>, public AddMulSemiring<SpMVInd, SpMVVal> {
public:
  ParallelHWCSRSpMV(unsigned int numPEs, WrapperRegDriver * driver,
                 const char * attachName) {
    m_attachName = attachName;
    m_platform = driver;
    m_numPEs = numPEs;
    for(unsigned int pe = 0; pe < m_numPEs; pe++) {
        m_pe[pe] = new HWCSRSpMV<SpMVInd, SpMVVal>(driver, pe);
    }
    m_platform->attach(attachName);
  }

  virtual ~ParallelHWCSRSpMV() {
    m_platform->detach();
    for(unsigned int pe = 0; pe < m_numPEs; pe++) {
        delete m_pe[pe];
    }
  }

  void forceExit() {
    for(unsigned int pe = 0; pe < m_numPEs; pe++) {
      m_pe[pe]->forceExit();
    }
  }

  virtual void setA(CSR<SpMVInd, SpMVVal> * A) {
    CSRSpMV<SpMVInd, SpMVVal>::setA(A);
    // create the partitions
    m_partitions = A->rowPartitionedView(A->calcRowPartitionBoundaries(m_numPEs));
    // assign the partitions to PEs
    for(unsigned int pe = 0; pe < m_numPEs; pe++) {
      m_pe[pe]->setA(m_partitions[pe]);
    }
  }

  virtual void setx(SpMVVal * x) {
    CSRSpMV<SpMVInd, SpMVVal>::setx(x);
    // assign input vector for each PE
    // TODO dont't mke multiple copies of input vector
    for(unsigned int pe = 0; pe < m_numPEs; pe++) {
      m_pe[pe]->setx(x);
    }
  }

  virtual void sety(SpMVVal * y) {
    CSRSpMV<SpMVInd, SpMVVal>::sety(y);
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
    return true;
  }

  HWCSRSpMV<SpMVInd, SpMVVal> * getPE(unsigned int ind) {
      return m_pe[ind];
  }

  void printAllStats() {
    for(unsigned int pe = 0; pe < m_numPEs; pe++) {
      m_pe[pe]->printAllStats();
    }
  }

  // TODO expose proper stats
  virtual unsigned int statInt(std::string name) {
    if(name == "cyclesRegular") return findMaxPEStat(name);
    else return 0;
  }


  virtual std::vector<std::string> statKeys() {
    std::vector<std::string> keys;
    keys.push_back("cyclesRegular");
    return keys;
  }

protected:
  unsigned int m_numPEs;
  const char * m_attachName;
  HWCSRSpMV<SpMVInd, SpMVVal> * m_pe[MAX_HWSPMV_PE];
  WrapperRegDriver * m_platform;
  std::vector<CSR<SpMVInd, SpMVVal> * > m_partitions;

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

  unsigned int findMaxPEStat(std::string key) {
        unsigned int foundMax = 0;
        unsigned int foundID = 0;
        for(unsigned int pe = 0; pe < m_numPEs; pe++) {
          unsigned int stat = m_pe[pe]->statInt(key);
          //cout << "pe " << pe << " " << key << " " << stat << endl;
          if(stat > foundMax) {
            foundID = pe;
            foundMax = stat;
          }
        }
        return foundMax;
  }

};

#endif // PARALLELSPMVCSR_HPP

