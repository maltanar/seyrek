#ifndef COMMONSEMIRINGS_HPP
#define COMMONSEMIRINGS_HPP

#include "semiring.hpp"

template <class SpMVInd, class SpMVVal>
class AddMulSemiring: public virtual Semiring<SpMVInd, SpMVVal> {
protected:
  virtual SpMVVal add(SpMVVal first, SpMVVal second, SpMVInd row, SpMVInd col) {
    return first + second;
  }

  virtual SpMVVal mul(SpMVVal first, SpMVVal second, SpMVInd row, SpMVInd col) {
    return first * second;
  }
};

template <class SpMVInd, class SpMVVal>
class MinPlusSemiring: public virtual Semiring<SpMVInd, SpMVVal> {
protected:
  virtual SpMVVal add(SpMVVal first, SpMVVal second, SpMVInd row, SpMVInd col) {
    return (first < second ? first : second);
  }

  virtual SpMVVal mul(SpMVVal first, SpMVVal second, SpMVInd row, SpMVInd col) {
    return first + second;
  }
};

#endif // COMMONSEMIRINGS_HPP
