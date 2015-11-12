#ifndef CSCSPMV_HPP
#define CSCSPMV_HPP

#include <vector>
#include <string>
#include "semiring.hpp"
#include "csc.hpp"

// base class for doing SpMV-on-semirings for CSC-encoded sparse matrices
template <class SpMVInd, class SpMVVal>
class CSCSpMV : public virtual Semiring<SpMVInd, SpMVVal> {
public:
  CSCSpMV() {m_A = 0; m_x = 0; m_y = 0;}
  virtual ~CSCSpMV() {};

  virtual void setA(CSC<SpMVInd, SpMVVal> * A) {m_A = A;}
  CSC<SpMVInd, SpMVVal> * getA() {return m_A;}
  virtual void setx(SpMVVal * x) {m_x = x;}
  SpMVVal * getx() {return m_x;}
  virtual void sety(SpMVVal * y) {m_y = y;}
  SpMVVal * gety() {return m_y;}

  // execute one SpMV step, y = A*x
  virtual bool exec() = 0;
  // functions for querying stats
  virtual unsigned int statInt(std::string name) = 0;
  virtual std::vector<std::string> statKeys() = 0;

protected:
  CSC<SpMVInd, SpMVVal> * m_A;

  SpMVVal * m_x;
  SpMVVal * m_y;

};

#endif
